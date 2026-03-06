//------------------------------------------------------------------------------
/*
    Copyright all by the Beijing Institute of Open Source Chip

    Filename    : reset_sync
    File-history: 
       (1) Ma Jiaxin created in 20250915
  */
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
`include "common_defines.sv"
`include "router_define.sv"

module reset_sync (
    reset_n_in,
    safeshift,
    reset_out,
    clock
);

//*** INPUT/OUTPUT *************************************************************
    input  wire reset_n_in;
    input  wire safeshift;
    output wire reset_out;

    input  wire clock;
   
//*** WIRE/REG *****************************************************************
    wire    reset_local;
    wire    reset_en;
    reg     reset2_n;
    reg     reset3_n;
    reg     reset4_n;

//*** MAIN BODY ****************************************************************
    assign reset_local = reset_n_in;
    assign reset_en = ~reset4_n;

    sync_dff U_SYNC_DFF(
        .sync_o  (reset2_n   ),                 
        .sync_i  (reset_n_in ),                
        .clk     (clock      ),                 
        .rstn    (reset_local)         
    );

    always @(posedge clock or negedge reset_local) begin
        if (reset_local == 1'b0) 
            reset3_n <= #`NOC_DELAY 1'b0;
        else if (reset_en == 1'b1)
            reset3_n <= #`NOC_DELAY reset2_n;
    end
    
    always @(posedge clock or negedge reset_local) begin
        if (reset_local == 1'b0) 
            reset4_n <= #`NOC_DELAY 1'b0;
        else if (reset_en == 1'b1)
            reset4_n <= #`NOC_DELAY reset3_n;
    end

    assign reset_out = safeshift | reset4_n;

endmodule
