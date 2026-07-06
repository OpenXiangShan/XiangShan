`ifndef TCNT_DEC_BASE__SV
`define TCNT_DEC_BASE__SV

package tcnt_dec_base;
    //switch
    typedef enum bit{
        ON  = 0,
        OFF = 1
    } switch_mode_e;
    //driver
    typedef enum int{
        DRV_0    = 0,
        DRV_1    = 1,
        DRV_X    = 2,
        DRV_RAND = 3,
        DRV_LST  = 4,
        DRV_RAND_ONLY_DATA = 5
    } drv_mode_e;
    //scoreboard
    typedef enum int {
        InOrder_RmMustFast_WithoutDrop   = 0,
        InOrder_RmMustFast_WithDrop      ,
        InOrder_DutMaybeFast_WithoutDrop ,
        InOrder_DutMaybeFast_WithDrop    ,
        InOrder_DutMustFast_WithoutDrop  ,
        InOrder_DutMustFast_WithDrop     ,
        OutOrder_DutMustFast_WithoutDrop ,
        OutOrder_DutMustFast_WithDrop    ,
        Disable
    } scb_mode_sel_e;

endpackage

import tcnt_dec_base::*;

`endif

