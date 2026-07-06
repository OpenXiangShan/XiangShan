`ifndef TCNT_AGENT_CFG_BASE__SV
`define TCNT_AGENT_CFG_BASE__SV

class tcnt_agent_cfg_base extends uvm_object;
    rand tcnt_dec_base::switch_mode_e    sqr_sw; //sequencer switch
    rand tcnt_dec_base::switch_mode_e    drv_sw; //driver switch
    rand tcnt_dec_base::switch_mode_e    mon_sw; //monitor switch
    rand tcnt_dec_base::switch_mode_e    xz_sw ; //check X/Z on bus
    rand tcnt_dec_base::drv_mode_e       drv_mode; //signals rand mode
    rand int    xz_ck_num;
    int channel_id;

    `uvm_object_utils_begin(tcnt_agent_cfg_base)
        `uvm_field_enum(tcnt_dec_base::switch_mode_e,sqr_sw,UVM_ALL_ON)
        `uvm_field_enum(tcnt_dec_base::switch_mode_e,drv_sw,UVM_ALL_ON)
        `uvm_field_enum(tcnt_dec_base::switch_mode_e,mon_sw,UVM_ALL_ON)
        `uvm_field_enum(tcnt_dec_base::switch_mode_e,xz_sw,UVM_ALL_ON)
        `uvm_field_enum(tcnt_dec_base::drv_mode_e,drv_mode,UVM_ALL_ON)
        `uvm_field_int(xz_ck_num,UVM_ALL_ON)
    `uvm_object_utils_end

    extern constraint default_sw_cons;
    extern constraint default_xz_cons;
    extern constraint default_xz_ck_num_cons;
    extern constraint default_drv_mode_cons;

    extern function new(string name="tcnt_agent_cfg_base");
    extern function void pre_randomize();
    extern function void post_randomize();
endclass

//---------------------------- constraint ---------------------------//
constraint tcnt_agent_cfg_base::default_sw_cons{
    this.sqr_sw == tcnt_dec_base::ON;
    this.drv_sw == tcnt_dec_base::ON;
    this.mon_sw == tcnt_dec_base::ON;
}
constraint tcnt_agent_cfg_base::default_xz_cons{
    this.xz_sw == tcnt_dec_base::ON;
}
constraint tcnt_agent_cfg_base::default_xz_ck_num_cons{
    this.xz_ck_num == 10;
}
constraint tcnt_agent_cfg_base::default_drv_mode_cons{
    this.drv_mode inside {tcnt_dec_base::DRV_0   ,
                          tcnt_dec_base::DRV_1   ,
                          tcnt_dec_base::DRV_X   ,
                          tcnt_dec_base::DRV_RAND,
                          tcnt_dec_base::DRV_LST  
                         };
}

//---------------------------- functions ----------------------------//
function tcnt_agent_cfg_base::new(string name="tcnt_agent_cfg_base");
    super.new(name);
endfunction:new

function void tcnt_agent_cfg_base::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize

function void tcnt_agent_cfg_base::post_randomize();
    super.post_randomize();
endfunction:post_randomize

`endif

