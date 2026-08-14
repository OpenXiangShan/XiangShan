`ifndef _BASICTEST_SV
`define  _BASICTEST_SV

//import seqlib_pkg::*;
//import tc_pkg::*;
class basicTest extends tcnt_test_base ;
      plus    plus_args ;
      memblock_env env;
      memblock_env_cfg real_smoke_cfg;
      string main_vseq_name;
      uvm_object_wrapper main_vseq_wrapper;
      `uvm_component_utils(basicTest)
      virtual tc_if rst_vif;

      function new(string name = "basicTest", uvm_component parent);
          super.new(name,parent);
          plus_args = new();
          main_vseq_name = "";
      endfunction:new

      function bit vseq_starts_l2tlb(input string vseq_name);
          return vseq_name == "memblock_dispatch_real_smoke_vseq" ||
                 vseq_name == "memblock_dispatch_manual_control_vseq" ||
                 vseq_name == "memblock_dispatch_real_cancel_reconcile_vseq";
      endfunction:vseq_starts_l2tlb

      // 中文注释：只有这两个专项 VSEQ 可以成为 active control topology 的显式
      // worker/main sequence owner；普通 VSEQ 不能因为 plus=1/3 被隐式扩展为控制场景。
      function bit vseq_supports_control_worker_topology(input string vseq_name);
          return vseq_name == "memblock_dispatch_real_smoke_vseq" ||
                 vseq_name == "memblock_dispatch_manual_control_vseq";
      endfunction:vseq_supports_control_worker_topology

      function void initialize_l2tlb_testcase_lifecycle();
          bit needs_response;
          memblock_sync_pkg::memblock_l2tlb_responder_mode_e responder_mode;
          memblock_sync_pkg::memblock_l2tlb_dispatch_topology_e dispatch_topology;
          memblock_sync_pkg::memblock_l2tlb_start_mode_e start_mode;

          // basicTest only starts the selected virtual sequence.  Keep the
          // default responder plus from creating an unowned responder when
          // that sequence is not a dispatch topology.
          needs_response = seq_csr_common::get_l2tlb_seq_en() &&
                           vseq_starts_l2tlb(main_vseq_name);
          responder_mode = needs_response ?
              memblock_sync_pkg::MEMBLOCK_L2TLB_RESPONDER_ENABLED :
              memblock_sync_pkg::MEMBLOCK_L2TLB_RESPONDER_DISABLED;
          dispatch_topology = vseq_starts_l2tlb(main_vseq_name) ?
              memblock_sync_pkg::MEMBLOCK_L2TLB_TOPOLOGY_DISPATCH_ACTIVE :
              memblock_sync_pkg::MEMBLOCK_L2TLB_TOPOLOGY_NO_DISPATCH;
          if (!needs_response) begin
              start_mode = memblock_sync_pkg::MEMBLOCK_L2TLB_START_DISABLED;
          end
          else if (vseq_starts_l2tlb(main_vseq_name)) begin
              start_mode = memblock_sync_pkg::MEMBLOCK_L2TLB_START_EXPLICIT;
          end
          else begin
              start_mode = memblock_sync_pkg::MEMBLOCK_L2TLB_START_DISABLED;
          end
          memblock_sync_pkg::initialize_l2tlb_testcase_lifecycle(
              responder_mode,
              dispatch_topology,
              start_mode,
              needs_response,
              memblock_sync_pkg::l2tlb_responder_active,
              get_type_name());
          if (seq_csr_common::get_l2tlb_seq_en() && !needs_response) begin
              `uvm_info(get_type_name(),
                        "MEMBLOCK_L2TLB_SEQ_EN is ignored because the selected basicTest VSEQ has no dispatch topology",
                        UVM_LOW)
          end
      endfunction:initialize_l2tlb_testcase_lifecycle

      virtual function void build_phase(uvm_phase phase);

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

            main_vseq_name = "virtual_base_sequence";
            if (!$value$plusargs("VSEQ_MAIN=%s", main_vseq_name)) begin
                void'(uvm_cmdline_proc.get_arg_value("+VSEQ_MAIN=", main_vseq_name));
            end
            // 中文注释：basicTest 先解析 VSEQ_MAIN，再冻结 topology 并校验场景能力；
            // VSEQ 本身只读 snapshot，不拥有 mode 的设置或修正权限。
            seq_csr_common::initialize_control_worker_topology_from_plus(get_type_name());
            seq_csr_common::check_control_worker_dispatch_capability(
                vseq_supports_control_worker_topology(main_vseq_name), main_vseq_name);
            // 中文注释：第二种拓扑下 CSR/Fence worker 由选中的 VSEQ 显式启动。
            // main_phase 默认 sequence 仍安装无 producer 的 idle 基类，避免 agent 自身
            // fallback/default sequence 在同一 sequencer 上随机发送 item 与 worker 竞争。
            if (memblock_sync_pkg::uses_control_barrier_topology()) begin
                uvm_config_db#(uvm_object_wrapper)::set(
                    this,
                    "env.u_csr_ctrl_agent_agent.sqr.main_phase",
                    "default_sequence",
                    tcnt_default_sequence_base#(csr_ctrl_agent_agent_xaction)::type_id::get());
                uvm_config_db#(uvm_object_wrapper)::set(
                    this,
                    "env.u_fence_agent_agent.sqr.main_phase",
                    "default_sequence",
                    tcnt_default_sequence_base#(fence_agent_agent_xaction)::type_id::get());
            end
            `uvm_info(get_type_name(),$sformatf("usr_test_vseq_name:%0s",main_vseq_name),UVM_LOW)
            main_vseq_wrapper = uvm_factory::get().find_wrapper_by_name(main_vseq_name);
            if (main_vseq_wrapper == null) begin
                `uvm_fatal("BASIC_VSEQ_FACTORY",
                           $sformatf("+VSEQ_MAIN type is not registered: %0s",
                                     main_vseq_name))
            end
            if (!uvm_config_db#(virtual tc_if)::get(this, "", "vif", rst_vif)) `uvm_error(get_type_name(), "Failed to get tc_if interface!");
      endfunction:build_phase

      virtual task main_phase(uvm_phase phase);
          uvm_object created_obj;
          virtual_base_sequence main_vseq;

          super.main_phase(phase);
          if (env == null || env.vsqr == null) begin
              `uvm_fatal("BASIC_VSEQ_SQR",
                         "memblock_env.vsqr is null; cannot start +VSEQ_MAIN")
          end
          if (main_vseq_wrapper == null) begin
              `uvm_fatal("BASIC_VSEQ_FACTORY",
                         "main virtual sequence wrapper was not resolved in build_phase")
          end

          created_obj = uvm_factory::get().create_object_by_type(
              main_vseq_wrapper, env.vsqr.get_full_name(), main_vseq_name);
          if (created_obj == null) begin
              `uvm_fatal("BASIC_VSEQ_CREATE",
                         $sformatf("factory failed to create +VSEQ_MAIN type: %0s",
                                   main_vseq_name))
          end
          if (!$cast(main_vseq, created_obj)) begin
              `uvm_fatal("BASIC_VSEQ_TYPE",
                         $sformatf("+VSEQ_MAIN type must extend virtual_base_sequence: %0s",
                                   created_obj.get_type_name()))
          end

          // 中文注释：testcase objection 覆盖整个 start() 调用；派生 vseq 不依赖
          // pre_body()/post_body() objection 来保持 main_phase 存活。
          phase.raise_objection(this, "starting main virtual sequence");
          main_vseq.set_sequencer(env.vsqr);
          main_vseq.reseed();
          main_vseq.set_starting_phase(phase);
          if (!main_vseq.do_not_randomize && !main_vseq.randomize()) begin
              `uvm_fatal("BASIC_VSEQ_RANDOMIZE",
                         $sformatf("failed to randomize +VSEQ_MAIN type: %0s",
                                   main_vseq.get_type_name()))
          end
          main_vseq.uvm_report_info(
              "VSEQ_BODY",
              $sformatf("starting body on %0s", env.vsqr.get_full_name()),
              UVM_LOW);
          main_vseq.start(env.vsqr);
          main_vseq.uvm_report_info("VSEQ_BODY", "body completed", UVM_LOW);
          phase.drop_objection(this, "main virtual sequence completed");
      endtask:main_phase

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
        initialize_l2tlb_testcase_lifecycle();
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
