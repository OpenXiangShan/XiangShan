//------------------------------------------------------------------------------
/*
    Copyright all by the Beijing Institute of Open Source Chip

    Filename    : rlink_laint
    File-history: 
       (1) Zhou Shize created in 20241127: Build rlink of Router-Device;
  */
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

`include "common_defines.sv"
`include "router_define.sv"

module rlink_lainit #(
) (
    clock,
    rstn,
    dfx_en,
    
    rxlinkactivereq,
    rxlinkactiveack,
    txlinkactivereq,
    txlinkactiveack,

    flit2send,
    rxcrd_full,
    return_txcrd,
    rxcrd_enable,   

    txla_stop,
    txla_activate,
    txla_run,
    txla_deactivate,
    rxla_stop,
    rxla_activate,
    rxla_run,
    rxla_deactivate,

    rx_link_status,
    tx_link_status
);
    //*** INPUT/OUTPUT *************************************************************
    input wire clock;
    input wire rstn;
    input wire dfx_en;

    input  wire rxlinkactivereq;
    output wire rxlinkactiveack;
    output reg  txlinkactivereq;
    input  wire txlinkactiveack;

    input  wire flit2send;   
    input  wire rxcrd_full;   
    output wire return_txcrd;  
    output wire rxcrd_enable;  

    output reg  txla_stop;
    output reg  txla_activate;
    output reg  txla_run;
    output reg  txla_deactivate;
    output reg  rxla_stop;
    output reg  rxla_activate;
    output reg  rxla_run;
    output reg  rxla_deactivate;

    output reg  [1:0] rx_link_status;
    output reg  [1:0] tx_link_status;

    //*** WIRE/REG *****************************************************************
    reg  rxlinkactiveack_q;
    reg  flit2send_q;

    //*** MAIN BODY ****************************************************************
    ///////
    // TX
    ///////
    always @(posedge clock or negedge rstn) begin 
        if (rstn == 1'b0) 
            flit2send_q  <= #`NOC_DELAY 1'b0;
        else if (rxlinkactivereq)
            flit2send_q  <= #`NOC_DELAY 1'b0;
        else if (flit2send)
            flit2send_q  <= #`NOC_DELAY 1'b1;
    end

    always @(posedge clock or negedge rstn) begin : U_TXLINKACTIVEREQ
        if (rstn == 1'b0) 
            txlinkactivereq  <= #`NOC_DELAY 1'b0;
        else if (txla_stop | txla_run)
            txlinkactivereq  <= #`NOC_DELAY flit2send_q | rxlinkactivereq;
    end

    ///////
    // RX
    ///////
    always @(posedge clock or negedge rstn) begin : U_RXLINKACTIVEACK
        if (rstn == 1'b0) 
            rxlinkactiveack_q <= #`NOC_DELAY 1'b0;
        else if (rxla_activate | rxla_deactivate) 
            rxlinkactiveack_q <= #`NOC_DELAY rxlinkactivereq | ~rxcrd_full;
    end

    assign rxlinkactiveack = rxlinkactiveack_q;

    always @(posedge clock or negedge rstn) begin 
        if (rstn == 1'b0) 
            rxla_stop         <= #`NOC_DELAY 1'b1                               ;
        else 
            rxla_stop         <= #`NOC_DELAY ~rxlinkactivereq & ~rxlinkactiveack;
    end

    always @(posedge clock or negedge rstn) begin 
        if (rstn == 1'b0) 
            rxla_activate     <= #`NOC_DELAY 1'b0                              ;
        else 
            rxla_activate     <= #`NOC_DELAY rxlinkactivereq & ~rxlinkactiveack;
    end

    always @(posedge clock or negedge rstn) begin 
        if (rstn == 1'b0) 
            rxla_run          <= #`NOC_DELAY 1'b0                              ;
        else 
            rxla_run          <= #`NOC_DELAY rxlinkactivereq &  rxlinkactiveack;
    end

    always @(posedge clock or negedge rstn) begin 
        if (rstn == 1'b0) 
            rxla_deactivate   <= #`NOC_DELAY 1'b0                               ;
        else 
            rxla_deactivate   <= #`NOC_DELAY ~rxlinkactivereq &  rxlinkactiveack;
    end

    always @(posedge clock or negedge rstn) begin 
        if (rstn == 1'b0) 
            txla_stop         <= #`NOC_DELAY 1'b1                               ;
        else 
            txla_stop         <= #`NOC_DELAY ~txlinkactivereq & ~txlinkactiveack;
    end

    always @(posedge clock or negedge rstn) begin 
        if (rstn == 1'b0) 
            txla_activate     <= #`NOC_DELAY 1'b0                              ;
        else 
            txla_activate     <= #`NOC_DELAY txlinkactivereq & ~txlinkactiveack;
    end

    always @(posedge clock or negedge rstn) begin 
        if (rstn == 1'b0) 
            txla_run          <= #`NOC_DELAY 1'b0                              ;
        else 
            txla_run          <= #`NOC_DELAY txlinkactivereq &  txlinkactiveack;
    end

    always @(posedge clock or negedge rstn) begin 
        if (rstn == 1'b0) 
            txla_deactivate   <= #`NOC_DELAY 1'b0                               ;
        else 
            txla_deactivate   <= #`NOC_DELAY ~txlinkactivereq &  txlinkactiveack;
    end

    //00:stop 01:activate 10:deactivate 11:run
    always @(posedge clock or negedge rstn) begin 
        if (rstn == 1'b0) 
            rx_link_status   <= #`NOC_DELAY 2'b0                                 ;
        else if(dfx_en == 1)
            rx_link_status   <= #`NOC_DELAY {rxlinkactiveack,rxlinkactivereq}    ;
    end

    always @(posedge clock or negedge rstn) begin 
        if (rstn == 1'b0) 
            tx_link_status   <= #`NOC_DELAY 2'b0                                 ;
        else if(dfx_en == 1)
            tx_link_status   <= #`NOC_DELAY {txlinkactiveack,txlinkactivereq}    ;
    end

    // Other Output signals
    assign return_txcrd    =  txla_deactivate;
    assign rxcrd_enable    =  rxla_run;





endmodule
