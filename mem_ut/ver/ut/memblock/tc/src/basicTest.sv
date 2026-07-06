`ifndef _BASICTEST_SV
`define  _BASICTEST_SV

//import seqlib_pkg::*;
//import tc_pkg::*;
class basicTest extends tcnt_test_base ;
      plus    plus_args ;
      memblock_env env;
      memblock_env_cfg real_smoke_cfg;
      `uvm_component_utils(basicTest)
      virtual tc_if rst_vif;

      function new(string name = "basicTest", uvm_component parent);
          super.new(name,parent);
          plus_args = new();
      endfunction:new

      virtual function void build_phase(uvm_phase phase);

            string usr_test_vseq="virtual_base_sequence";
            uvm_cmdline_processor uvm_cmdline_proc;
            super.build_phase(phase);
                    seq_csr_common::reload_from_plus();
            real_smoke_cfg = memblock_env_cfg::type_id::create("real_smoke_cfg");
            void'(real_smoke_cfg.randomize());
            configure_real_env_cfg(real_smoke_cfg);
            uvm_config_db#(memblock_env_cfg)::set(this, "env", "cfg", real_smoke_cfg);
            this.env = memblock_env::type_id::create("env", this);
            uvm_top.set_timeout(10000us,1);
            uvm_cmdline_proc = uvm_cmdline_processor::get_inst();
            `uvm_info(get_type_name(),"enter test_build_phase",UVM_LOW)

            if (!$value$plusargs("VSEQ_MAIN=%s", usr_test_vseq)) begin
                void'(uvm_cmdline_proc.get_arg_value("+VSEQ_MAIN=", usr_test_vseq));
            end
            `uvm_info(get_type_name(),$sformatf("usr_test_vseq_name:%0s",usr_test_vseq),UVM_LOW)
            if (usr_test_vseq != "virtual_base_sequence") begin
`ifdef TCNT_USE_UVM12
                uvm_factory::get().set_type_override_by_name("virtual_base_sequence",usr_test_vseq);
`else
                factory.set_type_override_by_name("virtual_base_sequence",usr_test_vseq);
`endif
            end
            uvm_config_db#(uvm_object_wrapper)::set(this, "env.vsqr.main_phase", "default_sequence",virtual_base_sequence::type_id::get());
            if (!uvm_config_db#(virtual tc_if)::get(this, "", "vif", rst_vif)) `uvm_error(get_type_name(), "Failed to get tc_if interface!");
      endfunction:build_phase

    virtual function void configure_real_env_cfg(input memblock_env_cfg cfg);
        if (cfg == null) begin
            `uvm_fatal(get_type_name(), "configure_real_env_cfg got null cfg")
        end

        cfg.u_backendToTopBypass_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_fence_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_csr_ctrl_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_lsqcommit_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_lsqenq_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_lintsissue_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_vecissue_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_redirect_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_sbuffer_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_dcache_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_int_sink_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_L2tlb_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_itlb_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_prefetch_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_io_mem_to_ooo_ctrl_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_io_mem_to_ooo_int_wb_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_io_mem_to_ooo_vec_wb_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_io_mem_to_ooo_wakeup_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_other_ctrl_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;

        cfg.u_backendToTopBypass_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_fence_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_csr_ctrl_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_lsqcommit_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_lsqenq_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_lintsissue_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_vecissue_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_redirect_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_sbuffer_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_dcache_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_int_sink_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_L2tlb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_itlb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_prefetch_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_io_mem_to_ooo_ctrl_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_io_mem_to_ooo_int_wb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_io_mem_to_ooo_vec_wb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_io_mem_to_ooo_wakeup_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_other_ctrl_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    endfunction:configure_real_env_cfg


      virtual function void connect_phase(uvm_phase phase);
         super.connect_phase(phase);
         check_config_usage()      ;
      endfunction:connect_phase

      virtual task reset_phase(uvm_phase phase) ;
         super.reset_phase(phase);
         phase.raise_objection(this);
         phase.drop_objection(this);
      endtask:reset_phase

      virtual task configure_phase(uvm_phase phase);

      endtask:configure_phase

      virtual function void end_of_elaboration_phase(uvm_phase phase);
        super.end_of_elaboration_phase(phase);
        uvm_top.print_topology();
      endfunction:end_of_elaboration_phase

      virtual function void report_phase(uvm_phase phase);
            uvm_report_server rs ;
            rs = uvm_report_server::get_server();
            if(rs.get_severity_count(UVM_FATAL)+rs.get_severity_count(UVM_ERROR)== 0) begin
                 $display("--------------------TEST_PASS---------------------");
                 $display("          ||                  ||      ||          ");
                 $display("      ||||||||      |       |||||   |||||         ");
                 $display("        ||  ||     |||     |||  || |||  ||        ");
                 $display("        ||   ||    |||     ||    | ||    |        ");
                 $display("        ||   ||    | |      |||     |||           ");
                 $display("        ||  ||    |||||       |||     |||         ");
                 $display("        |||||     |||||        |||     |||        ");
                 $display("        ||       ||   |          |       |        ");
                 $display("        ||       |    ||   ||    | ||    |        ");
                 $display("        ||       |    ||   ||||||| |||||||        ");
                 $display("       ||||    ||||  ||||| ||||||  ||||||         ");
            end
            else begin
                $display("------------------ TSET_FAULT----------------------");
                $display("       ++++++     ++     ++  ++   ++       ++++++  ");
                $display("       ++++++     ++     ++  ++   ++       ++++++  ");
                $display("       ++        ++++    ++  ++   ++         ++    ");
                $display("       ++        ++++    ++  ++   ++         ++    ");
                $display("       +++++     ++++    ++  ++   ++         ++    ");
                $display("       +++++     +  +    ++  ++   ++         ++    ");
                $display("       ++       ++++++   ++  ++   ++         ++    ");
                $display("       ++       ++++++   ++  ++   ++         ++    ");
                $display("       ++       ++  ++   ++  ++   ++         ++    ");
                $display("       ++      ++    ++  ++++++   ++++++     ++    ");
                $display("       ++      ++    ++   ++++    ++++++     ++    ");
            end
      endfunction:report_phase
endclass
`endif
