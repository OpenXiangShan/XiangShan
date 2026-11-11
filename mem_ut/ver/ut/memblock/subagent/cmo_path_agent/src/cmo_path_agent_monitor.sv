`ifndef CMO_PATH_AGENT_MONITOR_SV
`define CMO_PATH_AGENT_MONITOR_SV
class cmo_path_agent_monitor extends uvm_monitor;
    `uvm_component_utils(cmo_path_agent_monitor)
    virtual cmo_path_agent_interface vif;
    uvm_analysis_port#(cmo_path_agent_xaction) mon_item_port;
    // reset边沿递增，旧source/slot的关联不能跨generation复用；不影响框架动态epoch。
    longint unsigned reset_generation;
    bit was_in_reset;
    function new(string name,uvm_component parent);
        super.new(name,parent);
        mon_item_port=new("mon_item_port",this);
        reset_generation=0;
        was_in_reset=0;
    endfunction
    function void build_phase(uvm_phase phase);
        super.build_phase(phase);
        if(!uvm_config_db#(virtual cmo_path_agent_interface)::get(this,"","vif",vif))
            `uvm_fatal("CMO_PATH_VIF","cmo_path_agent缺少内部被动观察interface")
    endfunction
    // 先复制同拍clocking快照，再等待CSR锚点；发布全部事件后同一item的sample即完成水位。
    // 只扫描固定数量的物理口，不扫描历史或主表；关联状态由RM唯一adapter维护。
    task run_phase(uvm_phase phase);
        cmo_path_agent_xaction tr;
        longint unsigned edge_time;
        super.run_phase(phase);
        forever begin
            @vif.mon_cb;
            edge_time=$time;
            tr=cmo_path_agent_xaction::type_id::create("path_sample");
            tr.reset_active=(vif.mon_cb.rst_n!==1'b1 ||
                memblock_sync_pkg::reset_backend_done!==1'b1 ||
                memblock_sync_pkg::l2tlb_reset_active());
            if(tr.reset_active&&!was_in_reset) reset_generation++;
            was_in_reset=tr.reset_active;
            tr.reset_generation=reset_generation;
            if(!tr.reset_active) begin
                for(int i=0;i<CMO_PATH_PROBE_NUM;i++)
                    if(vif.mon_cb.probes[i].valid===1'b1)
                        tr.events.push_back(vif.mon_cb.probes[i]);
            end
            // reset期CSR协调器暂停sample，无需请求不存在的时钟锚点。
            if(tr.reset_active) begin
                tr.reset_active=1;
                tr.events.delete();
                tr.sample_seq=memblock_sync_pkg::dut_sample_seq;
            end else begin
                memblock_sync_pkg::wait_for_l2tlb_sample_anchor(edge_time,tr.sample_seq);
            end
            mon_item_port.write(tr);
        end
    endtask
endclass
`endif
