`ifndef TCNT_TEST_BASE__SV
`define TCNT_TEST_BASE__SV

class no_miscmp extends uvm_report_catcher;
    `uvm_object_utils(no_miscmp)
    function new(string name="donnot_miscmp");
        super.new(name);
    endfunction

    function action_e catch();
        if(get_id()=="MISCMP") begin
            return (get_verbosity() <=UVM_HIGH) ? (CAUGHT) : (THROW);
        end
        return THROW;
    endfunction
endclass

class tcnt_test_base extends uvm_test;
    `uvm_component_utils(tcnt_test_base)
    
    tcnt_report_server_base   log_format;
    no_miscmp               close_miscmp;

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task run_phase(uvm_phase phase);
    extern virtual function void report_phase(uvm_phase phase);
endclass:tcnt_test_base
function tcnt_test_base::new(string name, uvm_component parent);
    super.new(name,parent);
    log_format = new;
    uvm_report_server::set_server(log_format);
    `ifdef USE_CADENCE_TOOL
        void'(cdns_uvm_pkg::uvm_cdns_report_server_setup(1));
    `endif
endfunction:new 
function void tcnt_test_base::build_phase(uvm_phase phase);
    super.build_phase(phase);
    close_miscmp = new("close_miscmp");
    uvm_report_cb::add(null, close_miscmp);
    set_report_max_quit_count(10);
    //uvm_top.set_timeout(100ms,0);
endfunction:build_phase
task tcnt_test_base::run_phase(uvm_phase phase);
    int heatbeat_cnt;
    super.run_phase(phase);
    fork
        while(1) begin
            heatbeat_cnt++;
            #100us;
            `uvm_info(get_type_name(),$sformatf(">>>>>>>>heartbeat[%0d*100us]<<<<<<<<<",heatbeat_cnt),UVM_NONE)
        end
    join_none
    //starting_phase.drop_objection(this);
endtask
function void tcnt_test_base::report_phase(uvm_phase phase);
    uvm_report_server server;
    int err_num;
    super.report_phase(phase);
    server = get_report_server();
    err_num = server.get_severity_count(UVM_ERROR);

    if (err_num != 0) begin
        $display("TEST CASE FAILED");
    end
    else begin
        $display("TEST CASE PASSED");
    end
endfunction

`endif

