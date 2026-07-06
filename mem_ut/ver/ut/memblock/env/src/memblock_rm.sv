//=========================================================
//File name    : memblock_rm.sv
//Author       : OpenAI_Codex
//Module name  : memblock_rm
//Discribution : memblock_rm : reference model
//Date         : 2026-04-12
//=========================================================
`ifndef MEMBLOCK_RM__SV
`define MEMBLOCK_RM__SV

class memblock_rm  extends tcnt_rm_base #(.seq_item_t(memblock_common_xaction));

    //virtual tc_if vif;
    memblock_env_cfg cfg;

    //aa_test_reg_model		reg_model;

    uvm_blocking_get_port #(backendToTopBypass_agent_agent_xaction) backendToTopBypass_agent_mon_item_port;
    uvm_blocking_get_port #(fence_agent_agent_xaction) fence_agent_mon_item_port;
    uvm_blocking_get_port #(csr_ctrl_agent_agent_xaction) csr_ctrl_agent_mon_item_port;
    uvm_blocking_get_port #(lsqcommit_agent_agent_xaction) lsqcommit_agent_mon_item_port;
    uvm_blocking_get_port #(lsqenq_agent_agent_xaction) lsqenq_agent_mon_item_port;
    uvm_blocking_get_port #(lintsissue_agent_agent_xaction) lintsissue_agent_mon_item_port;
    uvm_blocking_get_port #(vecissue_agent_agent_xaction) vecissue_agent_mon_item_port;
    uvm_blocking_get_port #(redirect_agent_agent_xaction) redirect_agent_mon_item_port;
    uvm_blocking_get_port #(sbuffer_agent_agent_xaction) sbuffer_agent_mon_item_port;
    uvm_blocking_get_port #(dcache_agent_agent_xaction) dcache_agent_mon_item_port;
    uvm_blocking_get_port #(int_sink_agent_agent_xaction) int_sink_agent_mon_item_port;
    uvm_blocking_get_port #(L2tlb_agent_agent_xaction) L2tlb_agent_mon_item_port;
    uvm_blocking_get_port #(itlb_agent_agent_xaction) itlb_agent_mon_item_port;
    uvm_blocking_get_port #(prefetch_agent_agent_xaction) prefetch_agent_mon_item_port;
    uvm_blocking_get_port #(io_mem_to_ooo_ctrl_agent_agent_xaction) io_mem_to_ooo_ctrl_agent_mon_item_port;
    uvm_blocking_get_port #(io_mem_to_ooo_int_wb_agent_agent_xaction) io_mem_to_ooo_int_wb_agent_mon_item_port;
    uvm_blocking_get_port #(io_mem_to_ooo_vec_wb_agent_agent_xaction) io_mem_to_ooo_vec_wb_agent_mon_item_port;
    uvm_blocking_get_port #(io_mem_to_ooo_wakeup_agent_agent_xaction) io_mem_to_ooo_wakeup_agent_mon_item_port;
    uvm_blocking_get_port #(io_mem_to_ooo_iq_feedback_agent_agent_xaction) io_mem_to_ooo_iq_feedback_agent_mon_item_port;
    uvm_blocking_get_port #(other_ctrl_agent_agent_xaction) other_ctrl_agent_mon_item_port;

    `uvm_component_utils(memblock_rm)

    extern         function      new(string name , uvm_component parent);
    extern         function void build_phase(uvm_phase phase);
    extern virtual task main_phase(uvm_phase phase);
    extern virtual task main_process();
endclass

function memblock_rm::new(string name , uvm_component parent);
    super.new(name, parent);
endfunction

function void memblock_rm::build_phase(uvm_phase phase);
    super.build_phase(phase);
    //if(!uvm_config_db#(virtual tc_if)::get(this, "", "vif", vif)) begin
    //    `uvm_fatal(get_type_name(),$sformatf("virtual interface must be set for vif(tc_if)!!!"))
    //end
    if(!uvm_config_db#(memblock_env_cfg)::get(this,"","cfg",this.cfg)) begin
        `uvm_fatal(get_type_name(),$sformatf("build_phase: env cfg is not set!!!"));
    end else begin
        `uvm_info(get_type_name(),$sformatf("build_phase: get_cfg !!!"),UVM_DEBUG);
    end

    this.backendToTopBypass_agent_mon_item_port = new($sformatf("backendToTopBypass_agent_mon_item_port"), this);
    this.fence_agent_mon_item_port = new($sformatf("fence_agent_mon_item_port"), this);
    this.csr_ctrl_agent_mon_item_port = new($sformatf("csr_ctrl_agent_mon_item_port"), this);
    this.lsqcommit_agent_mon_item_port = new($sformatf("lsqcommit_agent_mon_item_port"), this);
    this.lsqenq_agent_mon_item_port = new($sformatf("lsqenq_agent_mon_item_port"), this);
    this.lintsissue_agent_mon_item_port = new($sformatf("lintsissue_agent_mon_item_port"), this);
    this.vecissue_agent_mon_item_port = new($sformatf("vecissue_agent_mon_item_port"), this);
    this.redirect_agent_mon_item_port = new($sformatf("redirect_agent_mon_item_port"), this);
    this.sbuffer_agent_mon_item_port = new($sformatf("sbuffer_agent_mon_item_port"), this);
    this.dcache_agent_mon_item_port = new($sformatf("dcache_agent_mon_item_port"), this);
    this.int_sink_agent_mon_item_port = new($sformatf("int_sink_agent_mon_item_port"), this);
    this.L2tlb_agent_mon_item_port = new($sformatf("L2tlb_agent_mon_item_port"), this);
    this.itlb_agent_mon_item_port = new($sformatf("itlb_agent_mon_item_port"), this);
    this.prefetch_agent_mon_item_port = new($sformatf("prefetch_agent_mon_item_port"), this);
    this.io_mem_to_ooo_ctrl_agent_mon_item_port = new($sformatf("io_mem_to_ooo_ctrl_agent_mon_item_port"), this);
    this.io_mem_to_ooo_int_wb_agent_mon_item_port = new($sformatf("io_mem_to_ooo_int_wb_agent_mon_item_port"), this);
    this.io_mem_to_ooo_vec_wb_agent_mon_item_port = new($sformatf("io_mem_to_ooo_vec_wb_agent_mon_item_port"), this);
    this.io_mem_to_ooo_wakeup_agent_mon_item_port = new($sformatf("io_mem_to_ooo_wakeup_agent_mon_item_port"), this);
    this.io_mem_to_ooo_iq_feedback_agent_mon_item_port = new($sformatf("io_mem_to_ooo_iq_feedback_agent_mon_item_port"), this);
    this.other_ctrl_agent_mon_item_port = new($sformatf("other_ctrl_agent_mon_item_port"), this);

endfunction

task memblock_rm::main_phase(uvm_phase phase);
    super.main_phase(phase);
    this.main_process();
endtask

task memblock_rm::main_process();

    backendToTopBypass_agent_agent_xaction  backendToTopBypass_agent_tr_in;
    memblock_common_xaction  backendToTopBypass_agent_tr_out;

    fence_agent_agent_xaction  fence_agent_tr_in;
    memblock_common_xaction  fence_agent_tr_out;

    csr_ctrl_agent_agent_xaction  csr_ctrl_agent_tr_in;
    memblock_common_xaction  csr_ctrl_agent_tr_out;

    lsqcommit_agent_agent_xaction  lsqcommit_agent_tr_in;
    memblock_common_xaction  lsqcommit_agent_tr_out;

    lsqenq_agent_agent_xaction  lsqenq_agent_tr_in;
    memblock_common_xaction  lsqenq_agent_tr_out;

    lintsissue_agent_agent_xaction  lintsissue_agent_tr_in;
    memblock_common_xaction  lintsissue_agent_tr_out;

    vecissue_agent_agent_xaction  vecissue_agent_tr_in;
    memblock_common_xaction  vecissue_agent_tr_out;

    redirect_agent_agent_xaction  redirect_agent_tr_in;
    memblock_common_xaction  redirect_agent_tr_out;

    sbuffer_agent_agent_xaction  sbuffer_agent_tr_in;
    memblock_common_xaction  sbuffer_agent_tr_out;

    dcache_agent_agent_xaction  dcache_agent_tr_in;
    memblock_common_xaction  dcache_agent_tr_out;

    int_sink_agent_agent_xaction  int_sink_agent_tr_in;
    memblock_common_xaction  int_sink_agent_tr_out;

    L2tlb_agent_agent_xaction  L2tlb_agent_tr_in;
    memblock_common_xaction  L2tlb_agent_tr_out;

    itlb_agent_agent_xaction  itlb_agent_tr_in;
    memblock_common_xaction  itlb_agent_tr_out;

    prefetch_agent_agent_xaction  prefetch_agent_tr_in;
    memblock_common_xaction  prefetch_agent_tr_out;

    io_mem_to_ooo_ctrl_agent_agent_xaction  io_mem_to_ooo_ctrl_agent_tr_in;
    memblock_common_xaction  io_mem_to_ooo_ctrl_agent_tr_out;

    io_mem_to_ooo_int_wb_agent_agent_xaction  io_mem_to_ooo_int_wb_agent_tr_in;
    memblock_common_xaction  io_mem_to_ooo_int_wb_agent_tr_out;

    io_mem_to_ooo_vec_wb_agent_agent_xaction  io_mem_to_ooo_vec_wb_agent_tr_in;
    memblock_common_xaction  io_mem_to_ooo_vec_wb_agent_tr_out;

    io_mem_to_ooo_wakeup_agent_agent_xaction  io_mem_to_ooo_wakeup_agent_tr_in;
    memblock_common_xaction  io_mem_to_ooo_wakeup_agent_tr_out;

    io_mem_to_ooo_iq_feedback_agent_agent_xaction  io_mem_to_ooo_iq_feedback_agent_tr_in;
    memblock_common_xaction  io_mem_to_ooo_iq_feedback_agent_tr_out;

    other_ctrl_agent_agent_xaction  other_ctrl_agent_tr_in;
    memblock_common_xaction  other_ctrl_agent_tr_out;

    fork

        while(1)begin
            this.backendToTopBypass_agent_mon_item_port.get(backendToTopBypass_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("backendToTopBypass_agent_mon_item_port get as %s",backendToTopBypass_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(backendToTopBypass_agent_tr_out, backendToTopBypass_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("backendToTopBypass_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            backendToTopBypass_agent_tr_out = memblock_common_xaction::type_id::create("backendToTopBypass_agent_tr_out");
            backendToTopBypass_agent_tr_out.channel_id = backendToTopBypass_agent_tr_in.channel_id;
            backendToTopBypass_agent_tr_out.pack_backendToTopBypass_agent(backendToTopBypass_agent_tr_in);

            this.rm_item_exp_port.write(backendToTopBypass_agent_tr_out);
            //this.rm_item_act_port.write(backendToTopBypass_agent_tr_out);
        end

        while(1)begin
            this.fence_agent_mon_item_port.get(fence_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("fence_agent_mon_item_port get as %s",fence_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(fence_agent_tr_out, fence_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("fence_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            fence_agent_tr_out = memblock_common_xaction::type_id::create("fence_agent_tr_out");
            fence_agent_tr_out.channel_id = fence_agent_tr_in.channel_id;
            fence_agent_tr_out.pack_fence_agent(fence_agent_tr_in);

            this.rm_item_exp_port.write(fence_agent_tr_out);
            //this.rm_item_act_port.write(fence_agent_tr_out);
        end

        while(1)begin
            this.csr_ctrl_agent_mon_item_port.get(csr_ctrl_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("csr_ctrl_agent_mon_item_port get as %s",csr_ctrl_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(csr_ctrl_agent_tr_out, csr_ctrl_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("csr_ctrl_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            csr_ctrl_agent_tr_out = memblock_common_xaction::type_id::create("csr_ctrl_agent_tr_out");
            csr_ctrl_agent_tr_out.channel_id = csr_ctrl_agent_tr_in.channel_id;
            csr_ctrl_agent_tr_out.pack_csr_ctrl_agent(csr_ctrl_agent_tr_in);

            this.rm_item_exp_port.write(csr_ctrl_agent_tr_out);
            //this.rm_item_act_port.write(csr_ctrl_agent_tr_out);
        end

        while(1)begin
            this.lsqcommit_agent_mon_item_port.get(lsqcommit_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("lsqcommit_agent_mon_item_port get as %s",lsqcommit_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(lsqcommit_agent_tr_out, lsqcommit_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("lsqcommit_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            lsqcommit_agent_tr_out = memblock_common_xaction::type_id::create("lsqcommit_agent_tr_out");
            lsqcommit_agent_tr_out.channel_id = lsqcommit_agent_tr_in.channel_id;
            lsqcommit_agent_tr_out.pack_lsqcommit_agent(lsqcommit_agent_tr_in);

            this.rm_item_exp_port.write(lsqcommit_agent_tr_out);
            //this.rm_item_act_port.write(lsqcommit_agent_tr_out);
        end

        while(1)begin
            this.lsqenq_agent_mon_item_port.get(lsqenq_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("lsqenq_agent_mon_item_port get as %s",lsqenq_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(lsqenq_agent_tr_out, lsqenq_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("lsqenq_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            lsqenq_agent_tr_out = memblock_common_xaction::type_id::create("lsqenq_agent_tr_out");
            lsqenq_agent_tr_out.channel_id = lsqenq_agent_tr_in.channel_id;
            lsqenq_agent_tr_out.pack_lsqenq_agent(lsqenq_agent_tr_in);

            this.rm_item_exp_port.write(lsqenq_agent_tr_out);
            //this.rm_item_act_port.write(lsqenq_agent_tr_out);
        end

        while(1)begin
            this.lintsissue_agent_mon_item_port.get(lintsissue_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("lintsissue_agent_mon_item_port get as %s",lintsissue_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(lintsissue_agent_tr_out, lintsissue_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("lintsissue_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            lintsissue_agent_tr_out = memblock_common_xaction::type_id::create("lintsissue_agent_tr_out");
            lintsissue_agent_tr_out.channel_id = lintsissue_agent_tr_in.channel_id;
            lintsissue_agent_tr_out.pack_lintsissue_agent(lintsissue_agent_tr_in);

            this.rm_item_exp_port.write(lintsissue_agent_tr_out);
            //this.rm_item_act_port.write(lintsissue_agent_tr_out);
        end

        while(1)begin
            this.vecissue_agent_mon_item_port.get(vecissue_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("vecissue_agent_mon_item_port get as %s",vecissue_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(vecissue_agent_tr_out, vecissue_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("vecissue_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            vecissue_agent_tr_out = memblock_common_xaction::type_id::create("vecissue_agent_tr_out");
            vecissue_agent_tr_out.channel_id = vecissue_agent_tr_in.channel_id;
            vecissue_agent_tr_out.pack_vecissue_agent(vecissue_agent_tr_in);

            this.rm_item_exp_port.write(vecissue_agent_tr_out);
            //this.rm_item_act_port.write(vecissue_agent_tr_out);
        end

        while(1)begin
            this.redirect_agent_mon_item_port.get(redirect_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("redirect_agent_mon_item_port get as %s",redirect_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(redirect_agent_tr_out, redirect_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("redirect_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            redirect_agent_tr_out = memblock_common_xaction::type_id::create("redirect_agent_tr_out");
            redirect_agent_tr_out.channel_id = redirect_agent_tr_in.channel_id;
            redirect_agent_tr_out.pack_redirect_agent(redirect_agent_tr_in);

            this.rm_item_exp_port.write(redirect_agent_tr_out);
            //this.rm_item_act_port.write(redirect_agent_tr_out);
        end

        while(1)begin
            this.sbuffer_agent_mon_item_port.get(sbuffer_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("sbuffer_agent_mon_item_port get as %s",sbuffer_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(sbuffer_agent_tr_out, sbuffer_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("sbuffer_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            sbuffer_agent_tr_out = memblock_common_xaction::type_id::create("sbuffer_agent_tr_out");
            sbuffer_agent_tr_out.channel_id = sbuffer_agent_tr_in.channel_id;
            sbuffer_agent_tr_out.pack_sbuffer_agent(sbuffer_agent_tr_in);

            this.rm_item_exp_port.write(sbuffer_agent_tr_out);
            //this.rm_item_act_port.write(sbuffer_agent_tr_out);
        end

        while(1)begin
            this.dcache_agent_mon_item_port.get(dcache_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("dcache_agent_mon_item_port get as %s",dcache_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(dcache_agent_tr_out, dcache_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("dcache_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            dcache_agent_tr_out = memblock_common_xaction::type_id::create("dcache_agent_tr_out");
            dcache_agent_tr_out.channel_id = dcache_agent_tr_in.channel_id;
            dcache_agent_tr_out.pack_dcache_agent(dcache_agent_tr_in);

            this.rm_item_exp_port.write(dcache_agent_tr_out);
            //this.rm_item_act_port.write(dcache_agent_tr_out);
        end

        while(1)begin
            this.int_sink_agent_mon_item_port.get(int_sink_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("int_sink_agent_mon_item_port get as %s",int_sink_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(int_sink_agent_tr_out, int_sink_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("int_sink_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            int_sink_agent_tr_out = memblock_common_xaction::type_id::create("int_sink_agent_tr_out");
            int_sink_agent_tr_out.channel_id = int_sink_agent_tr_in.channel_id;
            int_sink_agent_tr_out.pack_int_sink_agent(int_sink_agent_tr_in);

            this.rm_item_exp_port.write(int_sink_agent_tr_out);
            //this.rm_item_act_port.write(int_sink_agent_tr_out);
        end

        while(1)begin
            this.L2tlb_agent_mon_item_port.get(L2tlb_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("L2tlb_agent_mon_item_port get as %s",L2tlb_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(L2tlb_agent_tr_out, L2tlb_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("L2tlb_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            L2tlb_agent_tr_out = memblock_common_xaction::type_id::create("L2tlb_agent_tr_out");
            L2tlb_agent_tr_out.channel_id = L2tlb_agent_tr_in.channel_id;
            L2tlb_agent_tr_out.pack_L2tlb_agent(L2tlb_agent_tr_in);

            this.rm_item_exp_port.write(L2tlb_agent_tr_out);
            //this.rm_item_act_port.write(L2tlb_agent_tr_out);
        end

        while(1)begin
            this.itlb_agent_mon_item_port.get(itlb_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("itlb_agent_mon_item_port get as %s",itlb_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(itlb_agent_tr_out, itlb_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("itlb_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            itlb_agent_tr_out = memblock_common_xaction::type_id::create("itlb_agent_tr_out");
            itlb_agent_tr_out.channel_id = itlb_agent_tr_in.channel_id;
            itlb_agent_tr_out.pack_itlb_agent(itlb_agent_tr_in);

            this.rm_item_exp_port.write(itlb_agent_tr_out);
            //this.rm_item_act_port.write(itlb_agent_tr_out);
        end

        while(1)begin
            this.prefetch_agent_mon_item_port.get(prefetch_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("prefetch_agent_mon_item_port get as %s",prefetch_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(prefetch_agent_tr_out, prefetch_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("prefetch_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            prefetch_agent_tr_out = memblock_common_xaction::type_id::create("prefetch_agent_tr_out");
            prefetch_agent_tr_out.channel_id = prefetch_agent_tr_in.channel_id;
            prefetch_agent_tr_out.pack_prefetch_agent(prefetch_agent_tr_in);

            this.rm_item_exp_port.write(prefetch_agent_tr_out);
            //this.rm_item_act_port.write(prefetch_agent_tr_out);
        end

        while(1)begin
            this.io_mem_to_ooo_ctrl_agent_mon_item_port.get(io_mem_to_ooo_ctrl_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("io_mem_to_ooo_ctrl_agent_mon_item_port get as %s",io_mem_to_ooo_ctrl_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(io_mem_to_ooo_ctrl_agent_tr_out, io_mem_to_ooo_ctrl_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("io_mem_to_ooo_ctrl_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            io_mem_to_ooo_ctrl_agent_tr_out = memblock_common_xaction::type_id::create("io_mem_to_ooo_ctrl_agent_tr_out");
            io_mem_to_ooo_ctrl_agent_tr_out.channel_id = io_mem_to_ooo_ctrl_agent_tr_in.channel_id;
            io_mem_to_ooo_ctrl_agent_tr_out.pack_io_mem_to_ooo_ctrl_agent(io_mem_to_ooo_ctrl_agent_tr_in);

            this.rm_item_exp_port.write(io_mem_to_ooo_ctrl_agent_tr_out);
            //this.rm_item_act_port.write(io_mem_to_ooo_ctrl_agent_tr_out);
        end

        while(1)begin
            this.io_mem_to_ooo_int_wb_agent_mon_item_port.get(io_mem_to_ooo_int_wb_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("io_mem_to_ooo_int_wb_agent_mon_item_port get as %s",io_mem_to_ooo_int_wb_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(io_mem_to_ooo_int_wb_agent_tr_out, io_mem_to_ooo_int_wb_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("io_mem_to_ooo_int_wb_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            io_mem_to_ooo_int_wb_agent_tr_out = memblock_common_xaction::type_id::create("io_mem_to_ooo_int_wb_agent_tr_out");
            io_mem_to_ooo_int_wb_agent_tr_out.channel_id = io_mem_to_ooo_int_wb_agent_tr_in.channel_id;
            io_mem_to_ooo_int_wb_agent_tr_out.pack_io_mem_to_ooo_int_wb_agent(io_mem_to_ooo_int_wb_agent_tr_in);

            this.rm_item_exp_port.write(io_mem_to_ooo_int_wb_agent_tr_out);
            //this.rm_item_act_port.write(io_mem_to_ooo_int_wb_agent_tr_out);
        end

        while(1)begin
            this.io_mem_to_ooo_vec_wb_agent_mon_item_port.get(io_mem_to_ooo_vec_wb_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("io_mem_to_ooo_vec_wb_agent_mon_item_port get as %s",io_mem_to_ooo_vec_wb_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(io_mem_to_ooo_vec_wb_agent_tr_out, io_mem_to_ooo_vec_wb_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("io_mem_to_ooo_vec_wb_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            io_mem_to_ooo_vec_wb_agent_tr_out = memblock_common_xaction::type_id::create("io_mem_to_ooo_vec_wb_agent_tr_out");
            io_mem_to_ooo_vec_wb_agent_tr_out.channel_id = io_mem_to_ooo_vec_wb_agent_tr_in.channel_id;
            io_mem_to_ooo_vec_wb_agent_tr_out.pack_io_mem_to_ooo_vec_wb_agent(io_mem_to_ooo_vec_wb_agent_tr_in);

            this.rm_item_exp_port.write(io_mem_to_ooo_vec_wb_agent_tr_out);
            //this.rm_item_act_port.write(io_mem_to_ooo_vec_wb_agent_tr_out);
        end

        while(1)begin
            this.io_mem_to_ooo_wakeup_agent_mon_item_port.get(io_mem_to_ooo_wakeup_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("io_mem_to_ooo_wakeup_agent_mon_item_port get as %s",io_mem_to_ooo_wakeup_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(io_mem_to_ooo_wakeup_agent_tr_out, io_mem_to_ooo_wakeup_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("io_mem_to_ooo_wakeup_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            io_mem_to_ooo_wakeup_agent_tr_out = memblock_common_xaction::type_id::create("io_mem_to_ooo_wakeup_agent_tr_out");
            io_mem_to_ooo_wakeup_agent_tr_out.channel_id = io_mem_to_ooo_wakeup_agent_tr_in.channel_id;
            io_mem_to_ooo_wakeup_agent_tr_out.pack_io_mem_to_ooo_wakeup_agent(io_mem_to_ooo_wakeup_agent_tr_in);

            this.rm_item_exp_port.write(io_mem_to_ooo_wakeup_agent_tr_out);
            //this.rm_item_act_port.write(io_mem_to_ooo_wakeup_agent_tr_out);
        end

        while(1)begin
            this.io_mem_to_ooo_iq_feedback_agent_mon_item_port.get(io_mem_to_ooo_iq_feedback_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("io_mem_to_ooo_iq_feedback_agent_mon_item_port get as %s",io_mem_to_ooo_iq_feedback_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(io_mem_to_ooo_iq_feedback_agent_tr_out, io_mem_to_ooo_iq_feedback_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("io_mem_to_ooo_iq_feedback_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            io_mem_to_ooo_iq_feedback_agent_tr_out = memblock_common_xaction::type_id::create("io_mem_to_ooo_iq_feedback_agent_tr_out");
            io_mem_to_ooo_iq_feedback_agent_tr_out.channel_id = io_mem_to_ooo_iq_feedback_agent_tr_in.channel_id;
            io_mem_to_ooo_iq_feedback_agent_tr_out.pack_io_mem_to_ooo_iq_feedback_agent(io_mem_to_ooo_iq_feedback_agent_tr_in);

            this.rm_item_exp_port.write(io_mem_to_ooo_iq_feedback_agent_tr_out);
            //this.rm_item_act_port.write(io_mem_to_ooo_iq_feedback_agent_tr_out);
        end

        while(1)begin
            this.other_ctrl_agent_mon_item_port.get(other_ctrl_agent_tr_in);
            `uvm_info(get_type_name(),$sformatf("other_ctrl_agent_mon_item_port get as %s",other_ctrl_agent_tr_in.psdisplay()),UVM_DEBUG)
            //if(!$cast(other_ctrl_agent_tr_out, other_ctrl_agent_tr_in)) begin
            //    `uvm_fatal(get_type_name(),$sformatf("other_ctrl_agent_tr_in,is not a memblock_common_xaction or its extend"))
            //end
            other_ctrl_agent_tr_out = memblock_common_xaction::type_id::create("other_ctrl_agent_tr_out");
            other_ctrl_agent_tr_out.channel_id = other_ctrl_agent_tr_in.channel_id;
            other_ctrl_agent_tr_out.pack_other_ctrl_agent(other_ctrl_agent_tr_in);

            this.rm_item_exp_port.write(other_ctrl_agent_tr_out);
            //this.rm_item_act_port.write(other_ctrl_agent_tr_out);
        end

    join_none
endtask

`endif
