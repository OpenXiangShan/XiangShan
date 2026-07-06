//=========================================================
//File name    : gen_wave.sv
//Author       : OpenAI_Codex
//Module name  : gen_wave
//Discribution : gen_wave : generate wave(fsdb)
//Date         : 2026-04-12
//=========================================================
`ifndef GEN_WAVE__SV
`define GEN_WAVE__SV

longint seed_value;
string wave_type;
string fsdb_name="default";
string tc_name="";
string mode="";
int dly_100us_dump_fsdb=0;
string sdf_path;

initial
begin
    void'($value$plusargs("wave_type=%s",wave_type));
    void'($value$plusargs("ntb_random_seed=%d",seed_value));
    void'($value$plusargs("UVM_TESTNAME=%s",tc_name));
    void'($value$plusargs("TEST_MODE=%s",mode));
    if($test$plusargs("gen_wave=rtl")) begin
        fsdb_name = $sformatf("%s/wave/%s_%0d_rtl.fsdb",mode,tc_name,seed_value);
    end
    if($test$plusargs("gen_wave=setup")) begin
        fsdb_name = $sformatf("%s/wave/%s_%0d_setup.fsdb",mode,tc_name,seed_value);
    end
    if($test$plusargs("gen_wave=hold")) begin
        fsdb_name = $sformatf("%s/wave/%s_%0d_hold.fsdb",mode,tc_name,seed_value);
    end
    if($test$plusargs("gen_wave=gate")) begin
        fsdb_name = $sformatf("%s/wave/%s_%0d_gate.fsdb",mode,tc_name,seed_value);
    end
    `ifndef NO_FSDB
    if(wave_type=="fsdb") begin
	    $fsdbDumpfile(fsdb_name);
	    $fsdbDumpvars(0,"top_tb");
	    $fsdbDumpMDA(0,"top_tb");
        if($value$plusargs("dly_100us_dump_fsdb=%0d",dly_100us_dump_fsdb)) begin
            `uvm_info("DUMP_FSDB",$sformatf("DUMP FSDB after %0f ms",dly_100us_dump_fsdb/10),UVM_NONE)
            if(dly_100us_dump_fsdb!=0) begin
                $fsdbDumpoff;
                repeat(dly_100us_dump_fsdb) begin
                    tcnt_realtime::delay_us(100);
                end
                $fsdbDumpon;
            end
        end
    end
    `endif
end

`endif
