//------------------------------------------------------------------------------
/*
    Copyright all by the Beijing Institute of Open Source Chip

    Filename    : cdb_dev
    File-history: 
       (1) Zhou Shize created in 20250616: Build cross domain bridge of CHI;
*/
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
`include "common_defines.sv"
`include "router_define.sv"

module cdb_dev
#(
  parameter CDB_REQFIFO_DEPTH_DEV                        = 8    ,
  parameter CDB_RSPFIFO_DEPTH_DEV                        = 8    ,
  parameter CDB_DATFIFO_DEPTH_DEV                        = 8    ,
  parameter CDB_SNPFIFO_DEPTH_DEV                        = 8    ,
  parameter CDB_REQFIFO_DEPTH_ICN                        = 8    ,
  parameter CDB_RSPFIFO_DEPTH_ICN                        = 8    ,
  parameter CDB_DATFIFO_DEPTH_ICN                        = 8    ,
  parameter CDB_SNPFIFO_DEPTH_ICN                        = 8    ,
  parameter CDB_REQBP_FIFO_DEPTH_DEV                     = 4    ,
  parameter CDB_RSPBP_FIFO_DEPTH_DEV                     = 4    ,
  parameter CDB_DATBP_FIFO_DEPTH_DEV                     = 4    ,
  parameter CDB_SNPBP_FIFO_DEPTH_DEV                     = 4    ,
  parameter CDB_REQBP_FIFO_DEPTH_ICN                     = 4    ,
  parameter CDB_RSPBP_FIFO_DEPTH_ICN                     = 4    ,
  parameter CDB_DATBP_FIFO_DEPTH_ICN                     = 4    ,
  parameter CDB_SNPBP_FIFO_DEPTH_ICN                     = 4    ,
  parameter CDB_DEV2ICN_REQ_BYPASS_EN                    = 1    ,
  parameter CDB_DEV2ICN_RSP_BYPASS_EN                    = 1    ,
  parameter CDB_DEV2ICN_SNP_BYPASS_EN                    = 1    ,
  parameter CDB_DEV2ICN_DAT_BYPASS_EN                    = 1    ,
  parameter CDB_ICN2DEV_REQ_BYPASS_EN                    = 1    ,
  parameter CDB_ICN2DEV_RSP_BYPASS_EN                    = 1    ,
  parameter CDB_ICN2DEV_SNP_BYPASS_EN                    = 1    ,
  parameter CDB_ICN2DEV_DAT_BYPASS_EN                    = 1    ,
  parameter CDB_DEV2ICN_REQ_EN                           = 1    ,
  parameter CDB_DEV2ICN_RSP_EN                           = 1    ,
  parameter CDB_DEV2ICN_SNP_EN                           = 1    ,
  parameter CDB_DEV2ICN_DAT_EN                           = 1    ,
  parameter CDB_ICN2DEV_REQ_EN                           = 1    ,
  parameter CDB_ICN2DEV_RSP_EN                           = 1    ,
  parameter CDB_ICN2DEV_SNP_EN                           = 1    ,
  parameter CDB_ICN2DEV_DAT_EN                           = 1    ,
  parameter CDB_REQFLIT_WIDTH                            = `MESH_REQ_FLIT_WIDTH  ,
  parameter CDB_RSPFLIT_WIDTH                            = `MESH_RSP_FLIT_WIDTH  ,
  parameter CDB_SNPFLIT_WIDTH                            = `MESH_SNP_FLIT_WIDTH  ,
  parameter CDB_DATFLIT_WIDTH                            = `MESH_DAT_FLIT_WIDTH
)
(
  clk_dev                   ,
  rstn_dev                  ,
  rxsactive_dev             ,
  txsactive_dev             ,
  rxlinkactivereq_dev       ,
  rxlinkactiveack_dev       ,
  txlinkactivereq_dev       ,
  txlinkactiveack_dev       ,
  rxreq_flitpend_dev        ,
  rxreq_flitv_dev           ,
  rxreq_flit_dev            ,
  rxreq_crdv_dev            ,
  rxrsp_flitpend_dev        ,
  rxrsp_flitv_dev           ,
  rxrsp_flit_dev            ,
  rxrsp_crdv_dev            ,
  rxsnp_flitpend_dev        ,
  rxsnp_flitv_dev           ,
  rxsnp_flit_dev            ,
  rxsnp_crdv_dev            ,
  rxdat_flitpend_dev        ,
  rxdat_flitv_dev           ,
  rxdat_flit_dev            ,
  rxdat_crdv_dev            ,
  txreq_flitpend_dev        ,
  txreq_flitv_dev           ,
  txreq_flit_dev            ,
  txreq_crdv_dev            ,
  txrsp_flitpend_dev        ,
  txrsp_flitv_dev           ,
  txrsp_flit_dev            ,
  txrsp_crdv_dev            ,
  txsnp_flitpend_dev        ,
  txsnp_flitv_dev           ,
  txsnp_flit_dev            ,
  txsnp_crdv_dev            ,
  txdat_flitpend_dev        ,
  txdat_flitv_dev           ,
  txdat_flit_dev            ,
  txdat_crdv_dev            ,
  cdb_fifo_data_dev2icn_req ,
  rptr_r_icn2dev_req        ,
  wptr_r_dev2icn_req        ,
  cdb_fifo_data_icn2dev_req ,
  rptr_r_dev2icn_req        ,
  wptr_r_icn2dev_req        ,
  cdb_fifo_data_dev2icn_rsp ,
  rptr_r_icn2dev_rsp        ,
  wptr_r_dev2icn_rsp        ,
  cdb_fifo_data_icn2dev_rsp ,
  rptr_r_dev2icn_rsp        ,
  wptr_r_icn2dev_rsp        ,
  cdb_fifo_data_dev2icn_snp ,
  rptr_r_icn2dev_snp        ,
  wptr_r_dev2icn_snp        ,
  cdb_fifo_data_icn2dev_snp ,
  rptr_r_dev2icn_snp        ,
  wptr_r_icn2dev_snp        ,
  cdb_fifo_data_dev2icn_dat ,
  rptr_r_icn2dev_dat        ,
  wptr_r_dev2icn_dat        ,
  cdb_fifo_data_icn2dev_dat ,
  rptr_r_dev2icn_dat        ,
  wptr_r_icn2dev_dat        

);

    input  wire                             clk_dev                   ;
    input  wire                             rstn_dev                  ;
    input  wire                             rxsactive_dev             ;
    output wire                             txsactive_dev             ;
    input  wire                             rxlinkactivereq_dev       ;
    output wire                             rxlinkactiveack_dev       ;
    output wire                             txlinkactivereq_dev       ;
    input  wire                             txlinkactiveack_dev       ;
    input  wire                             rxreq_flitpend_dev        ;
    input  wire                             rxreq_flitv_dev           ;
    input  wire [CDB_REQFLIT_WIDTH -1:0]    rxreq_flit_dev            ;
    output wire                             rxreq_crdv_dev            ;
    input  wire                             rxrsp_flitpend_dev        ;
    input  wire                             rxrsp_flitv_dev           ;
    input  wire [CDB_RSPFLIT_WIDTH -1:0]    rxrsp_flit_dev            ;
    output wire                             rxrsp_crdv_dev            ;
    input  wire                             rxsnp_flitpend_dev        ;
    input  wire                             rxsnp_flitv_dev           ;
    input  wire [CDB_SNPFLIT_WIDTH -1:0]    rxsnp_flit_dev            ;
    output wire                             rxsnp_crdv_dev            ;
    input  wire                             rxdat_flitpend_dev        ;
    input  wire                             rxdat_flitv_dev           ;
    input  wire [CDB_DATFLIT_WIDTH -1:0]    rxdat_flit_dev            ;
    output wire                             rxdat_crdv_dev            ;
    output wire                             txreq_flitpend_dev        ;
    output wire                             txreq_flitv_dev           ;
    output wire [CDB_REQFLIT_WIDTH -1:0]    txreq_flit_dev            ;
    input  wire                             txreq_crdv_dev            ;
    output wire                             txrsp_flitpend_dev        ;
    output wire                             txrsp_flitv_dev           ;
    output wire [CDB_RSPFLIT_WIDTH -1:0]    txrsp_flit_dev            ;
    input  wire                             txrsp_crdv_dev            ;
    output wire                             txsnp_flitpend_dev        ;
    output wire                             txsnp_flitv_dev           ;
    output wire [CDB_SNPFLIT_WIDTH -1:0]    txsnp_flit_dev            ;
    input  wire                             txsnp_crdv_dev            ;
    output wire                             txdat_flitpend_dev        ;
    output wire                             txdat_flitv_dev           ;
    output wire [CDB_DATFLIT_WIDTH -1:0]    txdat_flit_dev            ;
    input  wire                             txdat_crdv_dev            ;


    output wire [CDB_REQFLIT_WIDTH*CDB_REQFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_dev2icn_req ;
    input  wire [CDB_REQFIFO_DEPTH_DEV                   -1:0]  rptr_r_icn2dev_req        ;
    output wire [CDB_REQFIFO_DEPTH_DEV                   -1:0]  wptr_r_dev2icn_req        ;
    input  wire [CDB_REQFLIT_WIDTH*CDB_REQFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_icn2dev_req ;
    output wire [CDB_REQFIFO_DEPTH_DEV                   -1:0]  rptr_r_dev2icn_req        ;
    input  wire [CDB_REQFIFO_DEPTH_DEV                   -1:0]  wptr_r_icn2dev_req        ;


    output wire [CDB_RSPFLIT_WIDTH*CDB_RSPFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_dev2icn_rsp ;
    input  wire [CDB_RSPFIFO_DEPTH_DEV                   -1:0]  rptr_r_icn2dev_rsp        ;
    output wire [CDB_RSPFIFO_DEPTH_DEV                   -1:0]  wptr_r_dev2icn_rsp        ;
    input  wire [CDB_RSPFLIT_WIDTH*CDB_RSPFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_icn2dev_rsp ;
    output wire [CDB_RSPFIFO_DEPTH_DEV                   -1:0]  rptr_r_dev2icn_rsp        ;
    input  wire [CDB_RSPFIFO_DEPTH_DEV                   -1:0]  wptr_r_icn2dev_rsp        ;


    output wire [CDB_SNPFLIT_WIDTH*CDB_SNPFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_dev2icn_snp ;
    input  wire [CDB_SNPFIFO_DEPTH_DEV                   -1:0]  rptr_r_icn2dev_snp        ;
    output wire [CDB_SNPFIFO_DEPTH_DEV                   -1:0]  wptr_r_dev2icn_snp        ;
    input  wire [CDB_SNPFLIT_WIDTH*CDB_SNPFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_icn2dev_snp ;
    output wire [CDB_SNPFIFO_DEPTH_DEV                   -1:0]  rptr_r_dev2icn_snp        ;
    input  wire [CDB_SNPFIFO_DEPTH_DEV                   -1:0]  wptr_r_icn2dev_snp        ;


    output wire [CDB_DATFLIT_WIDTH*CDB_DATFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_dev2icn_dat ;
    input  wire [CDB_DATFIFO_DEPTH_DEV                   -1:0]  rptr_r_icn2dev_dat        ;
    output wire [CDB_DATFIFO_DEPTH_DEV                   -1:0]  wptr_r_dev2icn_dat        ;
    input  wire [CDB_DATFLIT_WIDTH*CDB_DATFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_icn2dev_dat ;
    output wire [CDB_DATFIFO_DEPTH_DEV                   -1:0]  rptr_r_dev2icn_dat        ;
    input  wire [CDB_DATFIFO_DEPTH_DEV                   -1:0]  wptr_r_icn2dev_dat        ;


    wire    tx_reqfifo_nempty_icn   ;
    wire    tx_rspfifo_nempty_icn   ;
    wire    tx_snpfifo_nempty_icn   ;
    wire    tx_datfifo_nempty_icn   ;

    wire    rxcrd_full_w_dev        ;
    wire    rxreqcrd_full_dev       ;
    wire    rxrspcrd_full_dev       ;
    wire    rxsnpcrd_full_dev       ;
    wire    rxdatcrd_full_dev       ;


    wire    txla_deactivate_dev     ;
    wire    txla_run_dev            ;
    wire    rxla_run_dev            ;


    assign  rxcrd_full_w_dev    =   rxreqcrd_full_dev & rxrspcrd_full_dev & rxsnpcrd_full_dev & rxdatcrd_full_dev   ;

    rlink_lainit U_HANDSHAKE_DEV(
        .clock               (clk_dev               ),
        .rstn                (rstn_dev              ),
        .dfx_en              (1'b0                  ),
        .rxlinkactivereq     (rxlinkactivereq_dev   ),
        .rxlinkactiveack     (rxlinkactiveack_dev   ),
        .txlinkactivereq     (txlinkactivereq_dev   ),
        .txlinkactiveack     (txlinkactiveack_dev   ),
        .flit2send           (tx_reqfifo_nempty_icn | tx_rspfifo_nempty_icn | tx_snpfifo_nempty_icn | tx_datfifo_nempty_icn),//TODO
        .rxcrd_full          (rxcrd_full_w_dev      ),
        .return_txcrd        (                      ),
        .rxcrd_enable        (                      ),   
        .txla_stop           (                      ),
        .txla_activate       (                      ),
        .txla_run            (txla_run_dev          ),
        .txla_deactivate     (txla_deactivate_dev   ),
        .rxla_stop           (                      ),
        .rxla_activate       (                      ),
        .rxla_run            (rxla_run_dev          ),
        .rxla_deactivate     (                      ),
        .rx_link_status      (                      ),
        .tx_link_status      (                      )
    );



generate if (CDB_DEV2ICN_REQ_EN == 1) begin: GEN_DEV2ICN_REQ_INGRESS
    cdb_ingress_channel #(
    .CHANNEL                                                           ( `CHANNEL_REQ               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_REQFIFO_DEPTH_DEV      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_REQBP_FIFO_DEPTH_DEV   ),
    .CDB_BYPASS                                                        ( CDB_DEV2ICN_REQ_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_REQFLIT_WIDTH          )
    ) U_CDB_DEV2ICN_REQ (
    .clk_in                                                            ( clk_dev                    ),
    .rstn_in                                                           ( rstn_dev                   ),
    .rx_flitpend                                                       ( rxreq_flitpend_dev         ),
    .rx_flitv                                                          ( rxreq_flitv_dev            ),
    .rx_flit                                                           ( rxreq_flit_dev             ),
    .rxcrdv                                                            ( rxreq_crdv_dev             ),
    .rxcrd_full                                                        ( rxreqcrd_full_dev          ),
    .en_crdv                                                           ( rxla_run_dev               ),
    .rptr_r_e2in                                                       ( rptr_r_icn2dev_req         ),
    .wptr_r_in2e                                                       ( wptr_r_dev2icn_req         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_dev2icn_req  )
    );
end else begin: NONE_DEV2ICN_REQ_INGRESS
    assign      rxreq_crdv_dev              =               1'b0                                            ;
    assign      rxreqcrd_full_dev           =               1'b1                                            ;
    assign      tx_reqfifo_nempty_dev       =               1'b0                                            ;
    assign      wptr_r_dev2icn_req          =               {CDB_REQFIFO_DEPTH_DEV{1'b0}}                   ;
    assign      cdb_fifo_data_dev2icn_req   =               {CDB_REQFLIT_WIDTH*CDB_REQFIFO_DEPTH_DEV{1'b0}} ;
end 
endgenerate

generate if (CDB_ICN2DEV_REQ_EN == 1) begin: GEN_ICN2DEV_REQ_EGRESS
    cdb_egress_channel #(
    .CHANNEL                                                           ( `CHANNEL_REQ               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_REQFIFO_DEPTH_ICN      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_REQBP_FIFO_DEPTH_ICN   ),
    .CDB_BYPASS                                                        ( CDB_ICN2DEV_REQ_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_REQFLIT_WIDTH          )
    ) U_CDB_ICN2DEV_REQ (
    .clk_out                                                           ( clk_dev                    ),
    .rstn_out                                                          ( rstn_dev                   ),
    .tx_flitpend                                                       ( txreq_flitpend_dev         ),
    .tx_flitv                                                          ( txreq_flitv_dev            ),
    .tx_flit                                                           ( txreq_flit_dev             ),
    .txcrdv                                                            ( txreq_crdv_dev             ),
    .cdb_fifo_nempty                                                   ( tx_reqfifo_nempty_icn      ),
    .en_flitv                                                          ( txla_run_dev               ),
    .rptr_r_e2in                                                       ( rptr_r_dev2icn_req         ),
    .wptr_r_in2e                                                       ( wptr_r_icn2dev_req         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_icn2dev_req  ),
    .link_deactive                                                     ( txla_deactivate_dev        )
    );
end else begin: NONE_ICN2DEV_REQ_EGRESS
    assign      txreq_flitpend_dev      =      1'b0                         ;
    assign      txreq_flitv_dev         =      1'b0                         ;
    assign      txreq_flit_dev          =      {CDB_REQFLIT_WIDTH{1'b0}}    ;
    assign      tx_reqfifo_nempty_icn   =      1'b0                         ;
    assign      rptr_r_dev2icn_req      =      {CDB_REQFIFO_DEPTH_ICN{1'b0}};
end 
endgenerate



generate if (CDB_DEV2ICN_RSP_EN == 1) begin: GEN_DEV2ICN_RSP_INGRESS
    cdb_ingress_channel #(
    .CHANNEL                                                           ( `CHANNEL_RSP               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_RSPFIFO_DEPTH_DEV      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_RSPBP_FIFO_DEPTH_DEV   ),
    .CDB_BYPASS                                                        ( CDB_DEV2ICN_RSP_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_RSPFLIT_WIDTH          )
    ) U_CDB_DEV2ICN_RSP (
    .clk_in                                                            ( clk_dev                    ),
    .rstn_in                                                           ( rstn_dev                   ),
    .rx_flitpend                                                       ( rxrsp_flitpend_dev         ),
    .rx_flitv                                                          ( rxrsp_flitv_dev            ),
    .rx_flit                                                           ( rxrsp_flit_dev             ),
    .rxcrdv                                                            ( rxrsp_crdv_dev             ),
    .rxcrd_full                                                        ( rxrspcrd_full_dev          ),
    .en_crdv                                                           ( rxla_run_dev               ),
    .rptr_r_e2in                                                       ( rptr_r_icn2dev_rsp         ),
    .wptr_r_in2e                                                       ( wptr_r_dev2icn_rsp         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_dev2icn_rsp  )
    );
end else begin: NONE_DEV2ICN_RSP_INGRESS
    assign      rxrsp_crdv_dev              =               1'b0                                            ;
    assign      rxrspcrd_full_dev           =               1'b1                                            ;
    assign      tx_rspfifo_nempty_dev       =               1'b0                                            ;
    assign      wptr_r_dev2icn_rsp          =               {CDB_RSPFIFO_DEPTH_DEV{1'b0}}                   ;
    assign      cdb_fifo_data_dev2icn_rsp   =               {CDB_RSPFLIT_WIDTH*CDB_RSPFIFO_DEPTH_DEV{1'b0}} ;
end 
endgenerate

generate if (CDB_ICN2DEV_RSP_EN == 1) begin: GEN_ICN2DEV_RSP_EGRESS
    cdb_egress_channel #(
    .CHANNEL                                                           ( `CHANNEL_RSP               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_RSPFIFO_DEPTH_ICN      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_RSPBP_FIFO_DEPTH_ICN   ),
    .CDB_BYPASS                                                        ( CDB_ICN2DEV_RSP_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_RSPFLIT_WIDTH          )
    ) U_CDB_ICN2DEV_RSP (
    .clk_out                                                           ( clk_dev                    ),
    .rstn_out                                                          ( rstn_dev                   ),
    .tx_flitpend                                                       ( txrsp_flitpend_dev         ),
    .tx_flitv                                                          ( txrsp_flitv_dev            ),
    .tx_flit                                                           ( txrsp_flit_dev             ),
    .txcrdv                                                            ( txrsp_crdv_dev             ),
    .cdb_fifo_nempty                                                   ( tx_rspfifo_nempty_icn      ),
    .en_flitv                                                          ( txla_run_dev               ),
    .rptr_r_e2in                                                       ( rptr_r_dev2icn_rsp         ),
    .wptr_r_in2e                                                       ( wptr_r_icn2dev_rsp         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_icn2dev_rsp  ),
    .link_deactive                                                     ( txla_deactivate_dev        )
    );
end else begin: NONE_ICN2DEV_RSP_EGRESS
    assign      txrsp_flitpend_dev      =      1'b0                         ;
    assign      txrsp_flitv_dev         =      1'b0                         ;
    assign      txrsp_flit_dev          =      {CDB_RSPFLIT_WIDTH{1'b0}}    ;
    assign      tx_rspfifo_nempty_icn   =      1'b0                         ;
    assign      rptr_r_dev2icn_rsp      =      {CDB_RSPFIFO_DEPTH_ICN{1'b0}};
end 
endgenerate


generate if (CDB_DEV2ICN_SNP_EN == 1) begin: GEN_DEV2ICN_SNP_INGRESS
    cdb_ingress_channel #(
    .CHANNEL                                                           ( `CHANNEL_SNP               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_SNPFIFO_DEPTH_DEV      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_SNPBP_FIFO_DEPTH_DEV   ),
    .CDB_BYPASS                                                        ( CDB_DEV2ICN_SNP_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_SNPFLIT_WIDTH          )
    ) U_CDB_DEV2ICN_SNP (
    .clk_in                                                            ( clk_dev                    ),
    .rstn_in                                                           ( rstn_dev                   ),
    .rx_flitpend                                                       ( rxsnp_flitpend_dev         ),
    .rx_flitv                                                          ( rxsnp_flitv_dev            ),
    .rx_flit                                                           ( rxsnp_flit_dev             ),
    .rxcrdv                                                            ( rxsnp_crdv_dev             ),
    .rxcrd_full                                                        ( rxsnpcrd_full_dev          ),
    .en_crdv                                                           ( rxla_run_dev               ),
    .rptr_r_e2in                                                       ( rptr_r_icn2dev_snp         ),
    .wptr_r_in2e                                                       ( wptr_r_dev2icn_snp         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_dev2icn_snp  )
    );
end else begin: NONE_DEV2ICN_SNP_INGRESS
    assign      rxsnp_crdv_dev              =               1'b0                                            ;
    assign      rxsnpcrd_full_dev           =               1'b1                                            ;
    assign      tx_snpfifo_nempty_dev       =               1'b0                                            ;
    assign      wptr_r_dev2icn_snp          =               {CDB_SNPFIFO_DEPTH_DEV{1'b0}}                   ;
    assign      cdb_fifo_data_dev2icn_snp   =               {CDB_SNPFLIT_WIDTH*CDB_SNPFIFO_DEPTH_DEV{1'b0}} ;
end 
endgenerate

generate if (CDB_ICN2DEV_SNP_EN == 1) begin: GEN_ICN2DEV_SNP_EGRESS
    cdb_egress_channel #(
    .CHANNEL                                                           ( `CHANNEL_SNP               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_SNPFIFO_DEPTH_ICN      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_SNPBP_FIFO_DEPTH_ICN   ),
    .CDB_BYPASS                                                        ( CDB_ICN2DEV_SNP_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_SNPFLIT_WIDTH          )
    ) U_CDB_ICN2DEV_SNP (
    .clk_out                                                           ( clk_dev                    ),
    .rstn_out                                                          ( rstn_dev                   ),
    .tx_flitpend                                                       ( txsnp_flitpend_dev         ),
    .tx_flitv                                                          ( txsnp_flitv_dev            ),
    .tx_flit                                                           ( txsnp_flit_dev             ),
    .txcrdv                                                            ( txsnp_crdv_dev             ),
    .cdb_fifo_nempty                                                   ( tx_snpfifo_nempty_icn      ),
    .en_flitv                                                          ( txla_run_dev               ),
    .rptr_r_e2in                                                       ( rptr_r_dev2icn_snp         ),
    .wptr_r_in2e                                                       ( wptr_r_icn2dev_snp         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_icn2dev_snp  ),
    .link_deactive                                                     ( txla_deactivate_dev        )
    );
end else begin: NONE_ICN2DEV_SNP_EGRESS
    assign      txsnp_flitpend_dev      =      1'b0                         ;
    assign      txsnp_flitv_dev         =      1'b0                         ;
    assign      txsnp_flit_dev          =      {CDB_SNPFLIT_WIDTH{1'b0}}    ;
    assign      tx_snpfifo_nempty_icn   =      1'b0                         ;
    assign      rptr_r_dev2icn_snp      =      {CDB_SNPFIFO_DEPTH_ICN{1'b0}};
end 
endgenerate


generate if (CDB_DEV2ICN_DAT_EN == 1) begin: GEN_DEV2ICN_DAT_INGRESS
    cdb_ingress_channel #(
    .CHANNEL                                                           ( `CHANNEL_DAT               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_DATFIFO_DEPTH_DEV      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_DATBP_FIFO_DEPTH_DEV   ),
    .CDB_BYPASS                                                        ( CDB_DEV2ICN_DAT_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_DATFLIT_WIDTH          )
    ) U_CDB_DEV2ICN_DAT (
    .clk_in                                                            ( clk_dev                    ),
    .rstn_in                                                           ( rstn_dev                   ),
    .rx_flitpend                                                       ( rxdat_flitpend_dev         ),
    .rx_flitv                                                          ( rxdat_flitv_dev            ),
    .rx_flit                                                           ( rxdat_flit_dev             ),
    .rxcrdv                                                            ( rxdat_crdv_dev             ),
    .rxcrd_full                                                        ( rxdatcrd_full_dev          ),
    .en_crdv                                                           ( rxla_run_dev               ),
    .rptr_r_e2in                                                       ( rptr_r_icn2dev_dat         ),
    .wptr_r_in2e                                                       ( wptr_r_dev2icn_dat         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_dev2icn_dat  )
    );
end else begin: NONE_DEV2ICN_DAT_INGRESS
    assign      rxdat_crdv_dev              =               1'b0                                            ;
    assign      rxdatcrd_full_dev           =               1'b1                                            ;
    assign      tx_datfifo_nempty_dev       =               1'b0                                            ;
    assign      wptr_r_dev2icn_dat          =               {CDB_DATFIFO_DEPTH_DEV{1'b0}}                   ;
    assign      cdb_fifo_data_dev2icn_dat   =               {CDB_DATFLIT_WIDTH*CDB_DATFIFO_DEPTH_DEV{1'b0}} ;
end 
endgenerate

generate if (CDB_ICN2DEV_DAT_EN == 1) begin: GEN_ICN2DEV_DAT_EGRESS
    cdb_egress_channel #(
    .CHANNEL                                                           ( `CHANNEL_DAT               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_DATFIFO_DEPTH_ICN      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_DATBP_FIFO_DEPTH_ICN   ),
    .CDB_BYPASS                                                        ( CDB_ICN2DEV_DAT_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_DATFLIT_WIDTH          )
    ) U_CDB_ICN2DEV_DAT (
    .clk_out                                                           ( clk_dev                    ),
    .rstn_out                                                          ( rstn_dev                   ),
    .tx_flitpend                                                       ( txdat_flitpend_dev         ),
    .tx_flitv                                                          ( txdat_flitv_dev            ),
    .tx_flit                                                           ( txdat_flit_dev             ),
    .txcrdv                                                            ( txdat_crdv_dev             ),
    .cdb_fifo_nempty                                                   ( tx_datfifo_nempty_icn      ),
    .en_flitv                                                          ( txla_run_dev               ),
    .rptr_r_e2in                                                       ( rptr_r_dev2icn_dat         ),
    .wptr_r_in2e                                                       ( wptr_r_icn2dev_dat         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_icn2dev_dat  ),
    .link_deactive                                                     ( txla_deactivate_dev        )
    );
end else begin: NONE_ICN2DEV_DAT_EGRESS
    assign      txdat_flitpend_dev      =      1'b0                         ;
    assign      txdat_flitv_dev         =      1'b0                         ;
    assign      txdat_flit_dev          =      {CDB_DATFLIT_WIDTH{1'b0}}    ;
    assign      tx_datfifo_nempty_icn   =      1'b0                         ;
    assign      rptr_r_dev2icn_dat      =      {CDB_DATFIFO_DEPTH_ICN{1'b0}};
end 
endgenerate


assign txsactive_dev = 1'b1;


endmodule
