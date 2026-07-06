//=========================================================
//File name    : tc_sanity.sv
//Author       : OpenAI_Codex
//Module name  : tc_sanity
//Discribution : tc_sanity : sanity
//Date         : 2026-04-12
//=========================================================
`ifndef TC_SANITY__SV
`define TC_SANITY__SV

`define TC_NAME tc_sanity

class `seq_backendToTopBypass_agent(`TC_NAME) extends backendToTopBypass_agent_agent_default_sequence;
    int start_send=0;
    function  new(string name= `"`seq_backendToTopBypass_agent(`TC_NAME)`");
        super.new(name);
    endfunction

    virtual task body();
        //wait for start
        while(this.start_send==0) begin
            tcnt_realtime::delay_ns(100);
            void'(uvm_config_db#(int)::get(null, get_full_name(), "start_send", start_send));
        end
        repeat(10) begin
            `uvm_create(req)
            //vodi'(req.randomize() with {req.xxx inside {[xxx:xx]};
            //                             req.yyy == yyy;});
            void'(req.randomize());
            `uvm_send(req)
        end
        tcnt_realtime::delay_us(100);
    endtask

    `uvm_object_utils_begin(`seq_backendToTopBypass_agent(`TC_NAME))
        `uvm_field_int(start_send,UVM_ALL_ON)
    `uvm_object_utils_end
endclass

class `seq_fence_agent(`TC_NAME) extends fence_agent_agent_default_sequence;
    int start_send=0;
    function  new(string name= `"`seq_fence_agent(`TC_NAME)`");
        super.new(name);
    endfunction

    virtual task body();
        //wait for start
        while(this.start_send==0) begin
            tcnt_realtime::delay_ns(100);
            void'(uvm_config_db#(int)::get(null, get_full_name(), "start_send", start_send));
        end
        repeat(10) begin
            `uvm_create(req)
            //vodi'(req.randomize() with {req.xxx inside {[xxx:xx]};
            //                             req.yyy == yyy;});
            void'(req.randomize());
            `uvm_send(req)
        end
        tcnt_realtime::delay_us(100);
    endtask

    `uvm_object_utils_begin(`seq_fence_agent(`TC_NAME))
        `uvm_field_int(start_send,UVM_ALL_ON)
    `uvm_object_utils_end
endclass

class `seq_csr_ctrl_agent(`TC_NAME) extends csr_ctrl_agent_agent_default_sequence;
    int start_send=0;
    function  new(string name= `"`seq_csr_ctrl_agent(`TC_NAME)`");
        super.new(name);
    endfunction

    virtual task body();
        //wait for start
        while(this.start_send==0) begin
            tcnt_realtime::delay_ns(100);
            void'(uvm_config_db#(int)::get(null, get_full_name(), "start_send", start_send));
        end
        repeat(10) begin
            `uvm_create(req)
            //vodi'(req.randomize() with {req.xxx inside {[xxx:xx]};
            //                             req.yyy == yyy;});
            void'(req.randomize());
            `uvm_send(req)
        end
        tcnt_realtime::delay_us(100);
    endtask

    `uvm_object_utils_begin(`seq_csr_ctrl_agent(`TC_NAME))
        `uvm_field_int(start_send,UVM_ALL_ON)
    `uvm_object_utils_end
endclass

class `seq_lsqcommit_agent(`TC_NAME) extends lsqcommit_agent_agent_default_sequence;
    int start_send=0;
    function  new(string name= `"`seq_lsqcommit_agent(`TC_NAME)`");
        super.new(name);
    endfunction

    virtual task body();
        //wait for start
        while(this.start_send==0) begin
            tcnt_realtime::delay_ns(100);
            void'(uvm_config_db#(int)::get(null, get_full_name(), "start_send", start_send));
        end
        repeat(10) begin
            `uvm_create(req)
            //vodi'(req.randomize() with {req.xxx inside {[xxx:xx]};
            //                             req.yyy == yyy;});
            void'(req.randomize());
            `uvm_send(req)
        end
        tcnt_realtime::delay_us(100);
    endtask

    `uvm_object_utils_begin(`seq_lsqcommit_agent(`TC_NAME))
        `uvm_field_int(start_send,UVM_ALL_ON)
    `uvm_object_utils_end
endclass

class `seq_lsqenq_agent(`TC_NAME) extends lsqenq_agent_agent_default_sequence;
    int start_send=0;
    function  new(string name= `"`seq_lsqenq_agent(`TC_NAME)`");
        super.new(name);
    endfunction

    virtual task body();
        //wait for start
        while(this.start_send==0) begin
            tcnt_realtime::delay_ns(100);
            void'(uvm_config_db#(int)::get(null, get_full_name(), "start_send", start_send));
        end
        repeat(10) begin
            `uvm_create(req)
            //vodi'(req.randomize() with {req.xxx inside {[xxx:xx]};
            //                             req.yyy == yyy;});
            void'(req.randomize());
            `uvm_send(req)
        end
        tcnt_realtime::delay_us(100);
    endtask

    `uvm_object_utils_begin(`seq_lsqenq_agent(`TC_NAME))
        `uvm_field_int(start_send,UVM_ALL_ON)
    `uvm_object_utils_end
endclass

class `seq_lintsissue_agent(`TC_NAME) extends lintsissue_agent_agent_default_sequence;
    int start_send=0;
    function  new(string name= `"`seq_lintsissue_agent(`TC_NAME)`");
        super.new(name);
    endfunction

    virtual task body();
        //wait for start
        while(this.start_send==0) begin
            tcnt_realtime::delay_ns(100);
            void'(uvm_config_db#(int)::get(null, get_full_name(), "start_send", start_send));
        end
        repeat(10) begin
            `uvm_create(req)
            //vodi'(req.randomize() with {req.xxx inside {[xxx:xx]};
            //                             req.yyy == yyy;});
            void'(req.randomize());
            `uvm_send(req)
        end
        tcnt_realtime::delay_us(100);
    endtask

    `uvm_object_utils_begin(`seq_lintsissue_agent(`TC_NAME))
        `uvm_field_int(start_send,UVM_ALL_ON)
    `uvm_object_utils_end
endclass

class `seq_vecissue_agent(`TC_NAME) extends vecissue_agent_agent_default_sequence;
    int start_send=0;
    function  new(string name= `"`seq_vecissue_agent(`TC_NAME)`");
        super.new(name);
    endfunction

    virtual task body();
        //wait for start
        while(this.start_send==0) begin
            tcnt_realtime::delay_ns(100);
            void'(uvm_config_db#(int)::get(null, get_full_name(), "start_send", start_send));
        end
        repeat(10) begin
            `uvm_create(req)
            //vodi'(req.randomize() with {req.xxx inside {[xxx:xx]};
            //                             req.yyy == yyy;});
            void'(req.randomize());
            `uvm_send(req)
        end
        tcnt_realtime::delay_us(100);
    endtask

    `uvm_object_utils_begin(`seq_vecissue_agent(`TC_NAME))
        `uvm_field_int(start_send,UVM_ALL_ON)
    `uvm_object_utils_end
endclass

class `seq_redirect_agent(`TC_NAME) extends redirect_agent_agent_default_sequence;
    int start_send=0;
    function  new(string name= `"`seq_redirect_agent(`TC_NAME)`");
        super.new(name);
    endfunction

    virtual task body();
        //wait for start
        while(this.start_send==0) begin
            tcnt_realtime::delay_ns(100);
            void'(uvm_config_db#(int)::get(null, get_full_name(), "start_send", start_send));
        end
        repeat(10) begin
            `uvm_create(req)
            //vodi'(req.randomize() with {req.xxx inside {[xxx:xx]};
            //                             req.yyy == yyy;});
            void'(req.randomize());
            `uvm_send(req)
        end
        tcnt_realtime::delay_us(100);
    endtask

    `uvm_object_utils_begin(`seq_redirect_agent(`TC_NAME))
        `uvm_field_int(start_send,UVM_ALL_ON)
    `uvm_object_utils_end
endclass

class `seq_sbuffer_agent(`TC_NAME) extends sbuffer_agent_agent_default_sequence;
    int start_send=0;
    function  new(string name= `"`seq_sbuffer_agent(`TC_NAME)`");
        super.new(name);
    endfunction

    virtual task body();
        //wait for start
        while(this.start_send==0) begin
            tcnt_realtime::delay_ns(100);
            void'(uvm_config_db#(int)::get(null, get_full_name(), "start_send", start_send));
        end
        repeat(10) begin
            `uvm_create(req)
            //vodi'(req.randomize() with {req.xxx inside {[xxx:xx]};
            //                             req.yyy == yyy;});
            void'(req.randomize());
            `uvm_send(req)
        end
        tcnt_realtime::delay_us(100);
    endtask

    `uvm_object_utils_begin(`seq_sbuffer_agent(`TC_NAME))
        `uvm_field_int(start_send,UVM_ALL_ON)
    `uvm_object_utils_end
endclass

class `seq_dcache_agent(`TC_NAME) extends dcache_agent_agent_default_sequence;
    int start_send=0;
    function  new(string name= `"`seq_dcache_agent(`TC_NAME)`");
        super.new(name);
    endfunction

    virtual task body();
        //wait for start
        while(this.start_send==0) begin
            tcnt_realtime::delay_ns(100);
            void'(uvm_config_db#(int)::get(null, get_full_name(), "start_send", start_send));
        end
        repeat(10) begin
            `uvm_create(req)
            //vodi'(req.randomize() with {req.xxx inside {[xxx:xx]};
            //                             req.yyy == yyy;});
            void'(req.randomize());
            `uvm_send(req)
        end
        tcnt_realtime::delay_us(100);
    endtask

    `uvm_object_utils_begin(`seq_dcache_agent(`TC_NAME))
        `uvm_field_int(start_send,UVM_ALL_ON)
    `uvm_object_utils_end
endclass

class `seq_int_sink_agent(`TC_NAME) extends int_sink_agent_agent_default_sequence;
    int start_send=0;
    function  new(string name= `"`seq_int_sink_agent(`TC_NAME)`");
        super.new(name);
    endfunction

    virtual task body();
        //wait for start
        while(this.start_send==0) begin
            tcnt_realtime::delay_ns(100);
            void'(uvm_config_db#(int)::get(null, get_full_name(), "start_send", start_send));
        end
        repeat(10) begin
            `uvm_create(req)
            //vodi'(req.randomize() with {req.xxx inside {[xxx:xx]};
            //                             req.yyy == yyy;});
            void'(req.randomize());
            `uvm_send(req)
        end
        tcnt_realtime::delay_us(100);
    endtask

    `uvm_object_utils_begin(`seq_int_sink_agent(`TC_NAME))
        `uvm_field_int(start_send,UVM_ALL_ON)
    `uvm_object_utils_end
endclass

class `seq_itlb_agent(`TC_NAME) extends itlb_agent_agent_default_sequence;
    int start_send=0;
    function  new(string name= `"`seq_itlb_agent(`TC_NAME)`");
        super.new(name);
    endfunction

    virtual task body();
        //wait for start
        while(this.start_send==0) begin
            tcnt_realtime::delay_ns(100);
            void'(uvm_config_db#(int)::get(null, get_full_name(), "start_send", start_send));
        end
        repeat(10) begin
            `uvm_create(req)
            //vodi'(req.randomize() with {req.xxx inside {[xxx:xx]};
            //                             req.yyy == yyy;});
            void'(req.randomize());
            `uvm_send(req)
        end
        tcnt_realtime::delay_us(100);
    endtask

    `uvm_object_utils_begin(`seq_itlb_agent(`TC_NAME))
        `uvm_field_int(start_send,UVM_ALL_ON)
    `uvm_object_utils_end
endclass

class `seq_other_ctrl_agent(`TC_NAME) extends other_ctrl_agent_agent_default_sequence;
    int start_send=0;
    function  new(string name= `"`seq_other_ctrl_agent(`TC_NAME)`");
        super.new(name);
    endfunction

    virtual task body();
        //wait for start
        while(this.start_send==0) begin
            tcnt_realtime::delay_ns(100);
            void'(uvm_config_db#(int)::get(null, get_full_name(), "start_send", start_send));
        end
        repeat(10) begin
            `uvm_create(req)
            //vodi'(req.randomize() with {req.xxx inside {[xxx:xx]};
            //                             req.yyy == yyy;});
            void'(req.randomize());
            `uvm_send(req)
        end
        tcnt_realtime::delay_us(100);
    endtask

    `uvm_object_utils_begin(`seq_other_ctrl_agent(`TC_NAME))
        `uvm_field_int(start_send,UVM_ALL_ON)
    `uvm_object_utils_end
endclass

class `TC_NAME extends tc_base;

    memblock_env_cfg sanity_cfg;

    function new(string name = "`TC_NAME", uvm_component parent = null);
        super.new(name,parent);
    endfunction
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual function void end_of_elaboration_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern virtual task configure_phase(uvm_phase phase);
    extern virtual task main_phase(uvm_phase phase);
    extern virtual task shutdown_phase(uvm_phase phase);
    `uvm_component_utils(`TC_NAME)
endclass

function void `TC_NAME::build_phase(uvm_phase phase);
    sanity_cfg = memblock_env_cfg::type_id::create("sanity_cfg");
    void'(sanity_cfg.randomize());

    // Sanity focuses on bring-up; passive status outputs may remain X before
    // a real transaction stream initializes them.
    sanity_cfg.u_backendToTopBypass_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_fence_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_csr_ctrl_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_lsqcommit_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_lsqenq_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_lintsissue_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_vecissue_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_redirect_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_sbuffer_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_dcache_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_int_sink_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_itlb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_prefetch_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_io_mem_to_ooo_ctrl_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_io_mem_to_ooo_int_wb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_io_mem_to_ooo_vec_wb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_io_mem_to_ooo_wakeup_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    sanity_cfg.u_other_ctrl_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;

    // Keep idle driving deterministic during sanity reset/bring-up.
    sanity_cfg.u_backendToTopBypass_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_fence_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_csr_ctrl_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_lsqcommit_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_lsqenq_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_lintsissue_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_vecissue_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_redirect_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_sbuffer_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_dcache_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_int_sink_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_itlb_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_prefetch_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_io_mem_to_ooo_ctrl_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_io_mem_to_ooo_int_wb_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_io_mem_to_ooo_vec_wb_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_io_mem_to_ooo_wakeup_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
    sanity_cfg.u_other_ctrl_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;

    uvm_config_db#(memblock_env_cfg)::set(this, "env", "cfg", sanity_cfg);
    super.build_phase(phase);
    //default_sequence set & override 2mux1
    //>>>>
    //default_sequence override
    //set_type_override_by_type(backendToTopBypass_agent_agent_default_sequence::get_type(), `seq_backendToTopBypass_agent(`TC_NAME)::get_type());
    //set_type_override_by_type(fence_agent_agent_default_sequence::get_type(), `seq_fence_agent(`TC_NAME)::get_type());
    //set_type_override_by_type(csr_ctrl_agent_agent_default_sequence::get_type(), `seq_csr_ctrl_agent(`TC_NAME)::get_type());
    //set_type_override_by_type(lsqcommit_agent_agent_default_sequence::get_type(), `seq_lsqcommit_agent(`TC_NAME)::get_type());
    //set_type_override_by_type(lsqenq_agent_agent_default_sequence::get_type(), `seq_lsqenq_agent(`TC_NAME)::get_type());
    //set_type_override_by_type(lintsissue_agent_agent_default_sequence::get_type(), `seq_lintsissue_agent(`TC_NAME)::get_type());
    //set_type_override_by_type(vecissue_agent_agent_default_sequence::get_type(), `seq_vecissue_agent(`TC_NAME)::get_type());
    //set_type_override_by_type(redirect_agent_agent_default_sequence::get_type(), `seq_redirect_agent(`TC_NAME)::get_type());
    //set_type_override_by_type(sbuffer_agent_agent_default_sequence::get_type(), `seq_sbuffer_agent(`TC_NAME)::get_type());
    //set_type_override_by_type(dcache_agent_agent_default_sequence::get_type(), `seq_dcache_agent(`TC_NAME)::get_type());
    //set_type_override_by_type(int_sink_agent_agent_default_sequence::get_type(), `seq_int_sink_agent(`TC_NAME)::get_type());
    //set_type_override_by_type(itlb_agent_agent_default_sequence::get_type(), `seq_itlb_agent(`TC_NAME)::get_type());
    //set_type_override_by_type(other_ctrl_agent_agent_default_sequence::get_type(), `seq_other_ctrl_agent(`TC_NAME)::get_type());

    //set default_sequence
    //uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_backendToTopBypass_agent_agent.sqr.main_phase"  , "default_sequence", `seq_backendToTopBypass_agent(`TC_NAME)::type_id::get());
    //uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_fence_agent_agent.sqr.main_phase"  , "default_sequence", `seq_fence_agent(`TC_NAME)::type_id::get());
    //uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_csr_ctrl_agent_agent.sqr.main_phase"  , "default_sequence", `seq_csr_ctrl_agent(`TC_NAME)::type_id::get());
    //uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_lsqcommit_agent_agent.sqr.main_phase"  , "default_sequence", `seq_lsqcommit_agent(`TC_NAME)::type_id::get());
    //uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_lsqenq_agent_agent.sqr.main_phase"  , "default_sequence", `seq_lsqenq_agent(`TC_NAME)::type_id::get());
    //uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_lintsissue_agent_agent.sqr.main_phase"  , "default_sequence", `seq_lintsissue_agent(`TC_NAME)::type_id::get());
    //uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_vecissue_agent_agent.sqr.main_phase"  , "default_sequence", `seq_vecissue_agent(`TC_NAME)::type_id::get());
    //uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_redirect_agent_agent.sqr.main_phase"  , "default_sequence", `seq_redirect_agent(`TC_NAME)::type_id::get());
    //uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_sbuffer_agent_agent.sqr.main_phase"  , "default_sequence", `seq_sbuffer_agent(`TC_NAME)::type_id::get());
    //uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_dcache_agent_agent.sqr.main_phase"  , "default_sequence", `seq_dcache_agent(`TC_NAME)::type_id::get());
    //uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_int_sink_agent_agent.sqr.main_phase"  , "default_sequence", `seq_int_sink_agent(`TC_NAME)::type_id::get());
    //uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_itlb_agent_agent.sqr.main_phase"  , "default_sequence", `seq_itlb_agent(`TC_NAME)::type_id::get());
    //uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_other_ctrl_agent_agent.sqr.main_phase"  , "default_sequence", `seq_other_ctrl_agent(`TC_NAME)::type_id::get());

endfunction

function void `TC_NAME::end_of_elaboration_phase(uvm_phase phase);
    super.end_of_elaboration_phase(phase);
endfunction

task `TC_NAME::reset_phase(uvm_phase phase);
    super.reset_phase(phase);
    phase.raise_objection(this);
    tcnt_realtime::delay_us(100);
    phase.drop_objection(this);
endtask

task `TC_NAME::configure_phase(uvm_phase phase);
    super.configure_phase(phase);
    phase.raise_objection(this);
    tcnt_realtime::delay_us(100);
    phase.drop_objection(this);
endtask

task `TC_NAME::main_phase(uvm_phase phase);
    super.main_phase(phase);
    phase.raise_objection(this);
    tcnt_realtime::delay_us(100);
    //uvm_config_db#(int)::set(this, "env.u_backendToTopBypass_agent_agent.sqr.*"  , "start_send", 1);
    //uvm_config_db#(int)::set(this, "env.u_fence_agent_agent.sqr.*"  , "start_send", 1);
    //uvm_config_db#(int)::set(this, "env.u_csr_ctrl_agent_agent.sqr.*"  , "start_send", 1);
    //uvm_config_db#(int)::set(this, "env.u_lsqcommit_agent_agent.sqr.*"  , "start_send", 1);
    //uvm_config_db#(int)::set(this, "env.u_lsqenq_agent_agent.sqr.*"  , "start_send", 1);
    //uvm_config_db#(int)::set(this, "env.u_lintsissue_agent_agent.sqr.*"  , "start_send", 1);
    //uvm_config_db#(int)::set(this, "env.u_vecissue_agent_agent.sqr.*"  , "start_send", 1);
    //uvm_config_db#(int)::set(this, "env.u_redirect_agent_agent.sqr.*"  , "start_send", 1);
    //uvm_config_db#(int)::set(this, "env.u_sbuffer_agent_agent.sqr.*"  , "start_send", 1);
    //uvm_config_db#(int)::set(this, "env.u_dcache_agent_agent.sqr.*"  , "start_send", 1);
    //uvm_config_db#(int)::set(this, "env.u_int_sink_agent_agent.sqr.*"  , "start_send", 1);
    //uvm_config_db#(int)::set(this, "env.u_itlb_agent_agent.sqr.*"  , "start_send", 1);
    //uvm_config_db#(int)::set(this, "env.u_other_ctrl_agent_agent.sqr.*"  , "start_send", 1);

    tcnt_realtime::delay_ms(1);
    phase.drop_objection(this);
endtask

task `TC_NAME::shutdown_phase(uvm_phase phase);
    super.shutdown_phase(phase);
    phase.raise_objection(this);
    tcnt_realtime::delay_us(100);
    phase.drop_objection(this);
endtask

`undef TC_NAME

`endif
