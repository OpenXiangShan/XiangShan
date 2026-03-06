//------------------------------------------------------------------------------
/*
    Copyright all by the Beijing Institute of Open Source Chip

    Filename    : cdb_icn
    File-history: 
       (1) Zhou Shize created in 20250616: Build cross domain bridge of CHI;
*/
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
`include "common_defines.sv"
`include "router_define.sv"

module cdb_icn
#(
  parameter CDB_REQFIFO_DEPTH_ICN                        = 8    ,
  parameter CDB_RSPFIFO_DEPTH_ICN                        = 8    ,
  parameter CDB_DATFIFO_DEPTH_ICN                        = 8    ,
  parameter CDB_SNPFIFO_DEPTH_ICN                        = 8    ,
  parameter CDB_REQFIFO_DEPTH_DEV                        = 8    ,
  parameter CDB_RSPFIFO_DEPTH_DEV                        = 8    ,
  parameter CDB_DATFIFO_DEPTH_DEV                        = 8    ,
  parameter CDB_SNPFIFO_DEPTH_DEV                        = 8    ,
  parameter CDB_REQBP_FIFO_DEPTH_ICN                     = 4    ,
  parameter CDB_RSPBP_FIFO_DEPTH_ICN                     = 4    ,
  parameter CDB_DATBP_FIFO_DEPTH_ICN                     = 4    ,
  parameter CDB_SNPBP_FIFO_DEPTH_ICN                     = 4    ,
  parameter CDB_REQBP_FIFO_DEPTH_DEV                     = 4    ,
  parameter CDB_RSPBP_FIFO_DEPTH_DEV                     = 4    ,
  parameter CDB_DATBP_FIFO_DEPTH_DEV                     = 4    ,
  parameter CDB_SNPBP_FIFO_DEPTH_DEV                     = 4    ,
  parameter CDB_ICN2DEV_REQ_BYPASS_EN                    = 1    ,
  parameter CDB_ICN2DEV_RSP_BYPASS_EN                    = 1    ,
  parameter CDB_ICN2DEV_SNP_BYPASS_EN                    = 1    ,
  parameter CDB_ICN2DEV_DAT_BYPASS_EN                    = 1    ,
  parameter CDB_DEV2ICN_REQ_BYPASS_EN                    = 1    ,
  parameter CDB_DEV2ICN_RSP_BYPASS_EN                    = 1    ,
  parameter CDB_DEV2ICN_SNP_BYPASS_EN                    = 1    ,
  parameter CDB_DEV2ICN_DAT_BYPASS_EN                    = 1    ,
  parameter CDB_ICN2DEV_REQ_EN                           = 1    ,
  parameter CDB_ICN2DEV_RSP_EN                           = 1    ,
  parameter CDB_ICN2DEV_SNP_EN                           = 1    ,
  parameter CDB_ICN2DEV_DAT_EN                           = 1    ,
  parameter CDB_DEV2ICN_REQ_EN                           = 1    ,
  parameter CDB_DEV2ICN_RSP_EN                           = 1    ,
  parameter CDB_DEV2ICN_SNP_EN                           = 1    ,
  parameter CDB_DEV2ICN_DAT_EN                           = 1    ,
  parameter CDB_REQFLIT_WIDTH                            = `MESH_REQ_FLIT_WIDTH  ,
  parameter CDB_RSPFLIT_WIDTH                            = `MESH_RSP_FLIT_WIDTH  ,
  parameter CDB_SNPFLIT_WIDTH                            = `MESH_SNP_FLIT_WIDTH  ,
  parameter CDB_DATFLIT_WIDTH                            = `MESH_DAT_FLIT_WIDTH
)
(
  clk_icn                   ,
  rstn_icn                  ,
  rxsactive_icn             ,
  txsactive_icn             ,
  rxlinkactivereq_icn       ,
  rxlinkactiveack_icn       ,
  txlinkactivereq_icn       ,
  txlinkactiveack_icn       ,
  rxreq_flitpend_icn        ,
  rxreq_flitv_icn           ,
  rxreq_flit_icn            ,
  rxreq_crdv_icn            ,
  rxrsp_flitpend_icn        ,
  rxrsp_flitv_icn           ,
  rxrsp_flit_icn            ,
  rxrsp_crdv_icn            ,
  rxsnp_flitpend_icn        ,
  rxsnp_flitv_icn           ,
  rxsnp_flit_icn            ,
  rxsnp_crdv_icn            ,
  rxdat_flitpend_icn        ,
  rxdat_flitv_icn           ,
  rxdat_flit_icn            ,
  rxdat_crdv_icn            ,
  txreq_flitpend_icn        ,
  txreq_flitv_icn           ,
  txreq_flit_icn            ,
  txreq_crdv_icn            ,
  txrsp_flitpend_icn        ,
  txrsp_flitv_icn           ,
  txrsp_flit_icn            ,
  txrsp_crdv_icn            ,
  txsnp_flitpend_icn        ,
  txsnp_flitv_icn           ,
  txsnp_flit_icn            ,
  txsnp_crdv_icn            ,
  txdat_flitpend_icn        ,
  txdat_flitv_icn           ,
  txdat_flit_icn            ,
  txdat_crdv_icn            ,
  cdb_fifo_data_icn2dev_req ,
  rptr_r_dev2icn_req        ,
  wptr_r_icn2dev_req        ,
  cdb_fifo_data_dev2icn_req ,
  rptr_r_icn2dev_req        ,
  wptr_r_dev2icn_req        ,
  cdb_fifo_data_icn2dev_rsp ,
  rptr_r_dev2icn_rsp        ,
  wptr_r_icn2dev_rsp        ,
  cdb_fifo_data_dev2icn_rsp ,
  rptr_r_icn2dev_rsp        ,
  wptr_r_dev2icn_rsp        ,
  cdb_fifo_data_icn2dev_snp ,
  rptr_r_dev2icn_snp        ,
  wptr_r_icn2dev_snp        ,
  cdb_fifo_data_dev2icn_snp ,
  rptr_r_icn2dev_snp        ,
  wptr_r_dev2icn_snp        ,
  cdb_fifo_data_icn2dev_dat ,
  rptr_r_dev2icn_dat        ,
  wptr_r_icn2dev_dat        ,
  cdb_fifo_data_dev2icn_dat ,
  rptr_r_icn2dev_dat        ,
  wptr_r_dev2icn_dat        

);

    input  wire                             clk_icn                   ;
    input  wire                             rstn_icn                  ;
    input  wire                             rxsactive_icn             ;
    output wire                             txsactive_icn             ;
    input  wire                             rxlinkactivereq_icn       ;
    output wire                             rxlinkactiveack_icn       ;
    output wire                             txlinkactivereq_icn       ;
    input  wire                             txlinkactiveack_icn       ;
    input  wire                             rxreq_flitpend_icn        ;
    input  wire                             rxreq_flitv_icn           ;
    input  wire [CDB_REQFLIT_WIDTH -1:0]    rxreq_flit_icn            ;
    output wire                             rxreq_crdv_icn            ;
    input  wire                             rxrsp_flitpend_icn        ;
    input  wire                             rxrsp_flitv_icn           ;
    input  wire [CDB_RSPFLIT_WIDTH -1:0]    rxrsp_flit_icn            ;
    output wire                             rxrsp_crdv_icn            ;
    input  wire                             rxsnp_flitpend_icn        ;
    input  wire                             rxsnp_flitv_icn           ;
    input  wire [CDB_SNPFLIT_WIDTH -1:0]    rxsnp_flit_icn            ;
    output wire                             rxsnp_crdv_icn            ;
    input  wire                             rxdat_flitpend_icn        ;
    input  wire                             rxdat_flitv_icn           ;
    input  wire [CDB_DATFLIT_WIDTH -1:0]    rxdat_flit_icn            ;
    output wire                             rxdat_crdv_icn            ;
    output wire                             txreq_flitpend_icn        ;
    output wire                             txreq_flitv_icn           ;
    output wire [CDB_REQFLIT_WIDTH -1:0]    txreq_flit_icn            ;
    input  wire                             txreq_crdv_icn            ;
    output wire                             txrsp_flitpend_icn        ;
    output wire                             txrsp_flitv_icn           ;
    output wire [CDB_RSPFLIT_WIDTH -1:0]    txrsp_flit_icn            ;
    input  wire                             txrsp_crdv_icn            ;
    output wire                             txsnp_flitpend_icn        ;
    output wire                             txsnp_flitv_icn           ;
    output wire [CDB_SNPFLIT_WIDTH -1:0]    txsnp_flit_icn            ;
    input  wire                             txsnp_crdv_icn            ;
    output wire                             txdat_flitpend_icn        ;
    output wire                             txdat_flitv_icn           ;
    output wire [CDB_DATFLIT_WIDTH -1:0]    txdat_flit_icn            ;
    input  wire                             txdat_crdv_icn            ;


    output wire [CDB_REQFLIT_WIDTH*CDB_REQFIFO_DEPTH_ICN -1:0]  cdb_fifo_data_icn2dev_req ;
    input  wire [CDB_REQFIFO_DEPTH_ICN                   -1:0]  rptr_r_dev2icn_req        ;
    output wire [CDB_REQFIFO_DEPTH_ICN                   -1:0]  wptr_r_icn2dev_req        ;
    input  wire [CDB_REQFLIT_WIDTH*CDB_REQFIFO_DEPTH_ICN -1:0]  cdb_fifo_data_dev2icn_req ;
    output wire [CDB_REQFIFO_DEPTH_ICN                   -1:0]  rptr_r_icn2dev_req        ;
    input  wire [CDB_REQFIFO_DEPTH_ICN                   -1:0]  wptr_r_dev2icn_req        ;


    output wire [CDB_RSPFLIT_WIDTH*CDB_RSPFIFO_DEPTH_ICN -1:0]  cdb_fifo_data_icn2dev_rsp ;
    input  wire [CDB_RSPFIFO_DEPTH_ICN                   -1:0]  rptr_r_dev2icn_rsp        ;
    output wire [CDB_RSPFIFO_DEPTH_ICN                   -1:0]  wptr_r_icn2dev_rsp        ;
    input  wire [CDB_RSPFLIT_WIDTH*CDB_RSPFIFO_DEPTH_ICN -1:0]  cdb_fifo_data_dev2icn_rsp ;
    output wire [CDB_RSPFIFO_DEPTH_ICN                   -1:0]  rptr_r_icn2dev_rsp        ;
    input  wire [CDB_RSPFIFO_DEPTH_ICN                   -1:0]  wptr_r_dev2icn_rsp        ;


    output wire [CDB_SNPFLIT_WIDTH*CDB_SNPFIFO_DEPTH_ICN -1:0]  cdb_fifo_data_icn2dev_snp ;
    input  wire [CDB_SNPFIFO_DEPTH_ICN                   -1:0]  rptr_r_dev2icn_snp        ;
    output wire [CDB_SNPFIFO_DEPTH_ICN                   -1:0]  wptr_r_icn2dev_snp        ;
    input  wire [CDB_SNPFLIT_WIDTH*CDB_SNPFIFO_DEPTH_ICN -1:0]  cdb_fifo_data_dev2icn_snp ;
    output wire [CDB_SNPFIFO_DEPTH_ICN                   -1:0]  rptr_r_icn2dev_snp        ;
    input  wire [CDB_SNPFIFO_DEPTH_ICN                   -1:0]  wptr_r_dev2icn_snp        ;


    output wire [CDB_DATFLIT_WIDTH*CDB_DATFIFO_DEPTH_ICN -1:0]  cdb_fifo_data_icn2dev_dat ;
    input  wire [CDB_DATFIFO_DEPTH_ICN                   -1:0]  rptr_r_dev2icn_dat        ;
    output wire [CDB_DATFIFO_DEPTH_ICN                   -1:0]  wptr_r_icn2dev_dat        ;
    input  wire [CDB_DATFLIT_WIDTH*CDB_DATFIFO_DEPTH_ICN -1:0]  cdb_fifo_data_dev2icn_dat ;
    output wire [CDB_DATFIFO_DEPTH_ICN                   -1:0]  rptr_r_icn2dev_dat        ;
    input  wire [CDB_DATFIFO_DEPTH_ICN                   -1:0]  wptr_r_dev2icn_dat        ;


    wire    tx_reqfifo_nempty_dev   ;
    wire    tx_rspfifo_nempty_dev   ;
    wire    tx_snpfifo_nempty_dev   ;
    wire    tx_datfifo_nempty_dev   ;

    wire    rxcrd_full_w_icn        ;
    wire    rxreqcrd_full_icn       ;
    wire    rxrspcrd_full_icn       ;
    wire    rxsnpcrd_full_icn       ;
    wire    rxdatcrd_full_icn       ;


    wire    txla_deactivate_icn     ;
    wire    txla_run_icn            ;
    wire    rxla_run_icn            ;


    assign  rxcrd_full_w_icn    =   rxreqcrd_full_icn & rxrspcrd_full_icn & rxsnpcrd_full_icn & rxdatcrd_full_icn   ;

    rlink_lainit U_HANDSHAKE_ICN(
        .clock               (clk_icn               ),
        .rstn                (rstn_icn              ),
        .dfx_en              (1'b0                  ),
        .rxlinkactivereq     (rxlinkactivereq_icn   ),
        .rxlinkactiveack     (rxlinkactiveack_icn   ),
        .txlinkactivereq     (txlinkactivereq_icn   ),
        .txlinkactiveack     (txlinkactiveack_icn   ),
        .flit2send           (tx_reqfifo_nempty_dev | tx_rspfifo_nempty_dev | tx_snpfifo_nempty_dev | tx_datfifo_nempty_dev),//TODO
        .rxcrd_full          (rxcrd_full_w_icn      ),
        .return_txcrd        (                      ),
        .rxcrd_enable        (                      ),   
        .txla_stop           (                      ),
        .txla_activate       (                      ),
        .txla_run            (txla_run_icn          ),
        .txla_deactivate     (txla_deactivate_icn   ),
        .rxla_stop           (                      ),
        .rxla_activate       (                      ),
        .rxla_run            (rxla_run_icn          ),
        .rxla_deactivate     (                      ),
        .rx_link_status      (                      ),
        .tx_link_status      (                      )
    );



generate if (CDB_ICN2DEV_REQ_EN == 1) begin: GEN_ICN2DEV_REQ_INGRESS
    cdb_ingress_channel #(
    .CHANNEL                                                           ( `CHANNEL_REQ               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_REQFIFO_DEPTH_ICN      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_REQBP_FIFO_DEPTH_ICN   ),
    .CDB_BYPASS                                                        ( CDB_ICN2DEV_REQ_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_REQFLIT_WIDTH          )
    ) U_CDB_ICN2DEV_REQ (
    .clk_in                                                            ( clk_icn                    ),
    .rstn_in                                                           ( rstn_icn                   ),
    .rx_flitpend                                                       ( rxreq_flitpend_icn         ),
    .rx_flitv                                                          ( rxreq_flitv_icn            ),
    .rx_flit                                                           ( rxreq_flit_icn             ),
    .rxcrdv                                                            ( rxreq_crdv_icn             ),
    .rxcrd_full                                                        ( rxreqcrd_full_icn          ),
    .en_crdv                                                           ( rxla_run_icn               ),
    .rptr_r_e2in                                                       ( rptr_r_dev2icn_req         ),
    .wptr_r_in2e                                                       ( wptr_r_icn2dev_req         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_icn2dev_req  )
    );
end else begin: NONE_ICN2DEV_REQ_INGRESS
    assign      rxreq_crdv_icn              =               1'b0                                            ;
    assign      rxreqcrd_full_icn           =               1'b1                                            ;
    assign      tx_reqfifo_nempty_icn       =               1'b0                                            ;
    assign      wptr_r_icn2dev_req          =               {CDB_REQFIFO_DEPTH_ICN{1'b0}}                   ;
    assign      cdb_fifo_data_icn2dev_req   =               {CDB_REQFLIT_WIDTH*CDB_REQFIFO_DEPTH_ICN{1'b0}} ;
end 
endgenerate

generate if (CDB_DEV2ICN_REQ_EN == 1) begin: GEN_DEV2ICN_REQ_EGRESS
    cdb_egress_channel #(
    .CHANNEL                                                           ( `CHANNEL_REQ               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_REQFIFO_DEPTH_DEV      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_REQBP_FIFO_DEPTH_DEV   ),
    .CDB_BYPASS                                                        ( CDB_DEV2ICN_REQ_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_REQFLIT_WIDTH          )
    ) U_CDB_DEV2ICN_REQ (
    .clk_out                                                           ( clk_icn                    ),
    .rstn_out                                                          ( rstn_icn                   ),
    .tx_flitpend                                                       ( txreq_flitpend_icn         ),
    .tx_flitv                                                          ( txreq_flitv_icn            ),
    .tx_flit                                                           ( txreq_flit_icn             ),
    .txcrdv                                                            ( txreq_crdv_icn             ),
    .cdb_fifo_nempty                                                   ( tx_reqfifo_nempty_dev      ),
    .en_flitv                                                          ( txla_run_icn               ),
    .rptr_r_e2in                                                       ( rptr_r_icn2dev_req         ),
    .wptr_r_in2e                                                       ( wptr_r_dev2icn_req         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_dev2icn_req  ),
    .link_deactive                                                     ( txla_deactivate_icn        )
    );
end else begin: NONE_DEV2ICN_REQ_EGRESS
    assign      txreq_flitpend_icn      =      1'b0                         ;
    assign      txreq_flitv_icn         =      1'b0                         ;
    assign      txreq_flit_icn          =      {CDB_REQFLIT_WIDTH{1'b0}}    ;
    assign      tx_reqfifo_nempty_dev   =      1'b0                         ;
    assign      rptr_r_icn2dev_req      =      {CDB_REQFIFO_DEPTH_DEV{1'b0}};
end 
endgenerate



generate if (CDB_ICN2DEV_RSP_EN == 1) begin: GEN_ICN2DEV_RSP_INGRESS
    cdb_ingress_channel #(
    .CHANNEL                                                           ( `CHANNEL_RSP               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_RSPFIFO_DEPTH_ICN      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_RSPBP_FIFO_DEPTH_ICN   ),
    .CDB_BYPASS                                                        ( CDB_ICN2DEV_RSP_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_RSPFLIT_WIDTH          )
    ) U_CDB_ICN2DEV_RSP (
    .clk_in                                                            ( clk_icn                    ),
    .rstn_in                                                           ( rstn_icn                   ),
    .rx_flitpend                                                       ( rxrsp_flitpend_icn         ),
    .rx_flitv                                                          ( rxrsp_flitv_icn            ),
    .rx_flit                                                           ( rxrsp_flit_icn             ),
    .rxcrdv                                                            ( rxrsp_crdv_icn             ),
    .rxcrd_full                                                        ( rxrspcrd_full_icn          ),
    .en_crdv                                                           ( rxla_run_icn               ),
    .rptr_r_e2in                                                       ( rptr_r_dev2icn_rsp         ),
    .wptr_r_in2e                                                       ( wptr_r_icn2dev_rsp         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_icn2dev_rsp  )
    );
end else begin: NONE_ICN2DEV_RSP_INGRESS
    assign      rxrsp_crdv_icn              =               1'b0                                            ;
    assign      rxrspcrd_full_icn           =               1'b1                                            ;
    assign      tx_rspfifo_nempty_icn       =               1'b0                                            ;
    assign      wptr_r_icn2dev_rsp          =               {CDB_RSPFIFO_DEPTH_ICN{1'b0}}                   ;
    assign      cdb_fifo_data_icn2dev_rsp   =               {CDB_RSPFLIT_WIDTH*CDB_RSPFIFO_DEPTH_ICN{1'b0}} ;
end 
endgenerate

generate if (CDB_DEV2ICN_RSP_EN == 1) begin: GEN_DEV2ICN_RSP_EGRESS
    cdb_egress_channel #(
    .CHANNEL                                                           ( `CHANNEL_RSP               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_RSPFIFO_DEPTH_DEV      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_RSPBP_FIFO_DEPTH_DEV   ),
    .CDB_BYPASS                                                        ( CDB_DEV2ICN_RSP_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_RSPFLIT_WIDTH          )
    ) U_CDB_DEV2ICN_RSP (
    .clk_out                                                           ( clk_icn                    ),
    .rstn_out                                                          ( rstn_icn                   ),
    .tx_flitpend                                                       ( txrsp_flitpend_icn         ),
    .tx_flitv                                                          ( txrsp_flitv_icn            ),
    .tx_flit                                                           ( txrsp_flit_icn             ),
    .txcrdv                                                            ( txrsp_crdv_icn             ),
    .cdb_fifo_nempty                                                   ( tx_rspfifo_nempty_dev      ),
    .en_flitv                                                          ( txla_run_icn               ),
    .rptr_r_e2in                                                       ( rptr_r_icn2dev_rsp         ),
    .wptr_r_in2e                                                       ( wptr_r_dev2icn_rsp         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_dev2icn_rsp  ),
    .link_deactive                                                     ( txla_deactivate_icn        )
    );
end else begin: NONE_DEV2ICN_RSP_EGRESS
    assign      txrsp_flitpend_icn      =      1'b0                         ;
    assign      txrsp_flitv_icn         =      1'b0                         ;
    assign      txrsp_flit_icn          =      {CDB_RSPFLIT_WIDTH{1'b0}}    ;
    assign      tx_rspfifo_nempty_dev   =      1'b0                         ;
    assign      rptr_r_icn2dev_rsp      =      {CDB_RSPFIFO_DEPTH_DEV{1'b0}};
end 
endgenerate


generate if (CDB_ICN2DEV_SNP_EN == 1) begin: GEN_ICN2DEV_SNP_INGRESS
    cdb_ingress_channel #(
    .CHANNEL                                                           ( `CHANNEL_SNP               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_SNPFIFO_DEPTH_ICN      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_SNPBP_FIFO_DEPTH_ICN   ),
    .CDB_BYPASS                                                        ( CDB_ICN2DEV_SNP_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_SNPFLIT_WIDTH          )
    ) U_CDB_ICN2DEV_SNP (
    .clk_in                                                            ( clk_icn                    ),
    .rstn_in                                                           ( rstn_icn                   ),
    .rx_flitpend                                                       ( rxsnp_flitpend_icn         ),
    .rx_flitv                                                          ( rxsnp_flitv_icn            ),
    .rx_flit                                                           ( rxsnp_flit_icn             ),
    .rxcrdv                                                            ( rxsnp_crdv_icn             ),
    .rxcrd_full                                                        ( rxsnpcrd_full_icn          ),
    .en_crdv                                                           ( rxla_run_icn               ),
    .rptr_r_e2in                                                       ( rptr_r_dev2icn_snp         ),
    .wptr_r_in2e                                                       ( wptr_r_icn2dev_snp         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_icn2dev_snp  )
    );
end else begin: NONE_ICN2DEV_SNP_INGRESS
    assign      rxsnp_crdv_icn              =               1'b0                                            ;
    assign      rxsnpcrd_full_icn           =               1'b1                                            ;
    assign      tx_snpfifo_nempty_icn       =               1'b0                                            ;
    assign      wptr_r_icn2dev_snp          =               {CDB_SNPFIFO_DEPTH_ICN{1'b0}}                   ;
    assign      cdb_fifo_data_icn2dev_snp   =               {CDB_SNPFLIT_WIDTH*CDB_SNPFIFO_DEPTH_ICN{1'b0}} ;
end 
endgenerate

generate if (CDB_DEV2ICN_SNP_EN == 1) begin: GEN_DEV2ICN_SNP_EGRESS
    cdb_egress_channel #(
    .CHANNEL                                                           ( `CHANNEL_SNP               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_SNPFIFO_DEPTH_DEV      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_SNPBP_FIFO_DEPTH_DEV   ),
    .CDB_BYPASS                                                        ( CDB_DEV2ICN_SNP_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_SNPFLIT_WIDTH          )
    ) U_CDB_DEV2ICN_SNP (
    .clk_out                                                           ( clk_icn                    ),
    .rstn_out                                                          ( rstn_icn                   ),
    .tx_flitpend                                                       ( txsnp_flitpend_icn         ),
    .tx_flitv                                                          ( txsnp_flitv_icn            ),
    .tx_flit                                                           ( txsnp_flit_icn             ),
    .txcrdv                                                            ( txsnp_crdv_icn             ),
    .cdb_fifo_nempty                                                   ( tx_snpfifo_nempty_dev      ),
    .en_flitv                                                          ( txla_run_icn               ),
    .rptr_r_e2in                                                       ( rptr_r_icn2dev_snp         ),
    .wptr_r_in2e                                                       ( wptr_r_dev2icn_snp         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_dev2icn_snp  ),
    .link_deactive                                                     ( txla_deactivate_icn        )
    );
end else begin: NONE_DEV2ICN_SNP_EGRESS
    assign      txsnp_flitpend_icn      =      1'b0                         ;
    assign      txsnp_flitv_icn         =      1'b0                         ;
    assign      txsnp_flit_icn          =      {CDB_SNPFLIT_WIDTH{1'b0}}    ;
    assign      tx_snpfifo_nempty_dev   =      1'b0                         ;
    assign      rptr_r_icn2dev_snp      =      {CDB_SNPFIFO_DEPTH_DEV{1'b0}};
end 
endgenerate


generate if (CDB_ICN2DEV_DAT_EN == 1) begin: GEN_ICN2DEV_DAT_INGRESS
    cdb_ingress_channel #(
    .CHANNEL                                                           ( `CHANNEL_DAT               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_DATFIFO_DEPTH_ICN      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_DATBP_FIFO_DEPTH_ICN   ),
    .CDB_BYPASS                                                        ( CDB_ICN2DEV_DAT_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_DATFLIT_WIDTH          )
    ) U_CDB_ICN2DEV_DAT (
    .clk_in                                                            ( clk_icn                    ),
    .rstn_in                                                           ( rstn_icn                   ),
    .rx_flitpend                                                       ( rxdat_flitpend_icn         ),
    .rx_flitv                                                          ( rxdat_flitv_icn            ),
    .rx_flit                                                           ( rxdat_flit_icn             ),
    .rxcrdv                                                            ( rxdat_crdv_icn             ),
    .rxcrd_full                                                        ( rxdatcrd_full_icn          ),
    .en_crdv                                                           ( rxla_run_icn               ),
    .rptr_r_e2in                                                       ( rptr_r_dev2icn_dat         ),
    .wptr_r_in2e                                                       ( wptr_r_icn2dev_dat         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_icn2dev_dat  )
    );
end else begin: NONE_ICN2DEV_DAT_INGRESS
    assign      rxdat_crdv_icn              =               1'b0                                            ;
    assign      rxdatcrd_full_icn           =               1'b1                                            ;
    assign      tx_datfifo_nempty_icn       =               1'b0                                            ;
    assign      wptr_r_icn2dev_dat          =               {CDB_DATFIFO_DEPTH_ICN{1'b0}}                   ;
    assign      cdb_fifo_data_icn2dev_dat   =               {CDB_DATFLIT_WIDTH*CDB_DATFIFO_DEPTH_ICN{1'b0}} ;
end 
endgenerate

generate if (CDB_DEV2ICN_DAT_EN == 1) begin: GEN_DEV2ICN_DAT_EGRESS
    cdb_egress_channel #(
    .CHANNEL                                                           ( `CHANNEL_DAT               ),
    .CDB_FIFO_DEPTH                                                    ( CDB_DATFIFO_DEPTH_DEV      ),
    .CDB_BP_FIFO_DEPTH                                                 ( CDB_DATBP_FIFO_DEPTH_DEV   ),
    .CDB_BYPASS                                                        ( CDB_DEV2ICN_DAT_BYPASS_EN  ),
    .CDB_FLIT_WIDTH                                                    ( CDB_DATFLIT_WIDTH          )
    ) U_CDB_DEV2ICN_DAT (
    .clk_out                                                           ( clk_icn                    ),
    .rstn_out                                                          ( rstn_icn                   ),
    .tx_flitpend                                                       ( txdat_flitpend_icn         ),
    .tx_flitv                                                          ( txdat_flitv_icn            ),
    .tx_flit                                                           ( txdat_flit_icn             ),
    .txcrdv                                                            ( txdat_crdv_icn             ),
    .cdb_fifo_nempty                                                   ( tx_datfifo_nempty_dev      ),
    .en_flitv                                                          ( txla_run_icn               ),
    .rptr_r_e2in                                                       ( rptr_r_icn2dev_dat         ),
    .wptr_r_in2e                                                       ( wptr_r_dev2icn_dat         ),
    .cdb_fifo_data_in2e                                                ( cdb_fifo_data_dev2icn_dat  ),
    .link_deactive                                                     ( txla_deactivate_icn        )
    );
end else begin: NONE_DEV2ICN_DAT_EGRESS
    assign      txdat_flitpend_icn      =      1'b0                         ;
    assign      txdat_flitv_icn         =      1'b0                         ;
    assign      txdat_flit_icn          =      {CDB_DATFLIT_WIDTH{1'b0}}    ;
    assign      tx_datfifo_nempty_dev   =      1'b0                         ;
    assign      rptr_r_icn2dev_dat      =      {CDB_DATFIFO_DEPTH_DEV{1'b0}};
end 
endgenerate

assign txsactive_icn = 1'b1;



endmodule
