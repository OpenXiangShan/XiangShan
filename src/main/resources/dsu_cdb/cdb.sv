//------------------------------------------------------------------------------
/*
    Copyright all by the Beijing Institute of Open Source Chip

    Filename    : cdb
    File-history: 
       (1) Zhou Shize created in 20250616: Build cross domain bridge of CHI;
*/
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
`include "common_defines.sv"
`include "router_define.sv"

module cdb
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
  clk_icn                   ,
  rxsactive_dev             ,
  txsactive_dev             ,
  rxsactive_icn             ,
  txsactive_icn             ,
  rxlinkactivereq_dev       ,
  rxlinkactiveack_dev       ,
  txlinkactivereq_dev       ,
  txlinkactiveack_dev       ,
  rxlinkactivereq_icn       ,
  rxlinkactiveack_icn       ,
  txlinkactivereq_icn       ,
  txlinkactiveack_icn       ,
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
  rx_tlrflit_icn            
);

    input  wire                             clk_dev                   ;
    input  wire                             rstn_dev                  ;
    input  wire                             clk_icn                   ;
    input  wire                             rxsactive_dev             ;
    output wire                             txsactive_dev             ;
    input  wire                             rxsactive_icn             ;
    output wire                             txsactive_icn             ;
    input  wire                             rxlinkactivereq_dev       ;
    output wire                             rxlinkactiveack_dev       ;
    output wire                             txlinkactivereq_dev       ;
    input  wire                             txlinkactiveack_dev       ;
    input  wire                             rxlinkactivereq_icn       ;
    output wire                             rxlinkactiveack_icn       ;
    output wire                             txlinkactivereq_icn       ;
    input  wire                             txlinkactiveack_icn       ;
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

    input  wire [`NOC_TLR_FLIT_WIDTH-1:0]   rx_tlrflit_icn            ;


    wire                                                 rstn_icn                  ;

    wire [CDB_REQFLIT_WIDTH*CDB_REQFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_dev2icn_req ;
    wire [CDB_REQFIFO_DEPTH_DEV                   -1:0]  rptr_r_icn2dev_req        ;
    wire [CDB_REQFIFO_DEPTH_DEV                   -1:0]  wptr_r_dev2icn_req        ;
    wire [CDB_REQFLIT_WIDTH*CDB_REQFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_icn2dev_req ;
    wire [CDB_REQFIFO_DEPTH_DEV                   -1:0]  rptr_r_dev2icn_req        ;
    wire [CDB_REQFIFO_DEPTH_DEV                   -1:0]  wptr_r_icn2dev_req        ;


    wire [CDB_RSPFLIT_WIDTH*CDB_RSPFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_dev2icn_rsp ;
    wire [CDB_RSPFIFO_DEPTH_DEV                   -1:0]  rptr_r_icn2dev_rsp        ;
    wire [CDB_RSPFIFO_DEPTH_DEV                   -1:0]  wptr_r_dev2icn_rsp        ;
    wire [CDB_RSPFLIT_WIDTH*CDB_RSPFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_icn2dev_rsp ;
    wire [CDB_RSPFIFO_DEPTH_DEV                   -1:0]  rptr_r_dev2icn_rsp        ;
    wire [CDB_RSPFIFO_DEPTH_DEV                   -1:0]  wptr_r_icn2dev_rsp        ;


    wire [CDB_SNPFLIT_WIDTH*CDB_SNPFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_dev2icn_snp ;
    wire [CDB_SNPFIFO_DEPTH_DEV                   -1:0]  rptr_r_icn2dev_snp        ;
    wire [CDB_SNPFIFO_DEPTH_DEV                   -1:0]  wptr_r_dev2icn_snp        ;
    wire [CDB_SNPFLIT_WIDTH*CDB_SNPFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_icn2dev_snp ;
    wire [CDB_SNPFIFO_DEPTH_DEV                   -1:0]  rptr_r_dev2icn_snp        ;
    wire [CDB_SNPFIFO_DEPTH_DEV                   -1:0]  wptr_r_icn2dev_snp        ;


    wire [CDB_DATFLIT_WIDTH*CDB_DATFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_dev2icn_dat ;
    wire [CDB_DATFIFO_DEPTH_DEV                   -1:0]  rptr_r_icn2dev_dat        ;
    wire [CDB_DATFIFO_DEPTH_DEV                   -1:0]  wptr_r_dev2icn_dat        ;
    wire [CDB_DATFLIT_WIDTH*CDB_DATFIFO_DEPTH_DEV -1:0]  cdb_fifo_data_icn2dev_dat ;
    wire [CDB_DATFIFO_DEPTH_DEV                   -1:0]  rptr_r_dev2icn_dat        ;
    wire [CDB_DATFIFO_DEPTH_DEV                   -1:0]  wptr_r_icn2dev_dat        ;




    cdb_dev #(
    .CDB_REQFIFO_DEPTH_ICN      (CDB_REQFIFO_DEPTH_ICN     ),
    .CDB_RSPFIFO_DEPTH_ICN      (CDB_RSPFIFO_DEPTH_ICN     ),
    .CDB_DATFIFO_DEPTH_ICN      (CDB_DATFIFO_DEPTH_ICN     ),
    .CDB_SNPFIFO_DEPTH_ICN      (CDB_SNPFIFO_DEPTH_ICN     ),
    .CDB_REQFIFO_DEPTH_DEV      (CDB_REQFIFO_DEPTH_DEV     ),
    .CDB_RSPFIFO_DEPTH_DEV      (CDB_RSPFIFO_DEPTH_DEV     ),
    .CDB_DATFIFO_DEPTH_DEV      (CDB_DATFIFO_DEPTH_DEV     ),
    .CDB_SNPFIFO_DEPTH_DEV      (CDB_SNPFIFO_DEPTH_DEV     ),
    .CDB_REQBP_FIFO_DEPTH_ICN   (CDB_REQBP_FIFO_DEPTH_ICN  ),
    .CDB_RSPBP_FIFO_DEPTH_ICN   (CDB_RSPBP_FIFO_DEPTH_ICN  ),
    .CDB_DATBP_FIFO_DEPTH_ICN   (CDB_DATBP_FIFO_DEPTH_ICN  ),
    .CDB_SNPBP_FIFO_DEPTH_ICN   (CDB_SNPBP_FIFO_DEPTH_ICN  ),
    .CDB_REQBP_FIFO_DEPTH_DEV   (CDB_REQBP_FIFO_DEPTH_DEV  ),
    .CDB_RSPBP_FIFO_DEPTH_DEV   (CDB_RSPBP_FIFO_DEPTH_DEV  ),
    .CDB_DATBP_FIFO_DEPTH_DEV   (CDB_DATBP_FIFO_DEPTH_DEV  ),
    .CDB_SNPBP_FIFO_DEPTH_DEV   (CDB_SNPBP_FIFO_DEPTH_DEV  ),
    .CDB_ICN2DEV_REQ_BYPASS_EN  (CDB_ICN2DEV_REQ_BYPASS_EN ),
    .CDB_ICN2DEV_RSP_BYPASS_EN  (CDB_ICN2DEV_RSP_BYPASS_EN ),
    .CDB_ICN2DEV_SNP_BYPASS_EN  (CDB_ICN2DEV_SNP_BYPASS_EN ),
    .CDB_ICN2DEV_DAT_BYPASS_EN  (CDB_ICN2DEV_DAT_BYPASS_EN ),
    .CDB_DEV2ICN_REQ_BYPASS_EN  (CDB_DEV2ICN_REQ_BYPASS_EN ),
    .CDB_DEV2ICN_RSP_BYPASS_EN  (CDB_DEV2ICN_RSP_BYPASS_EN ),
    .CDB_DEV2ICN_SNP_BYPASS_EN  (CDB_DEV2ICN_SNP_BYPASS_EN ),
    .CDB_DEV2ICN_DAT_BYPASS_EN  (CDB_DEV2ICN_DAT_BYPASS_EN ),
    .CDB_ICN2DEV_REQ_EN         (CDB_ICN2DEV_REQ_EN        ),
    .CDB_ICN2DEV_RSP_EN         (CDB_ICN2DEV_RSP_EN        ),
    .CDB_ICN2DEV_SNP_EN         (CDB_ICN2DEV_SNP_EN        ),
    .CDB_ICN2DEV_DAT_EN         (CDB_ICN2DEV_DAT_EN        ),
    .CDB_DEV2ICN_REQ_EN         (CDB_DEV2ICN_REQ_EN        ),
    .CDB_DEV2ICN_RSP_EN         (CDB_DEV2ICN_RSP_EN        ),
    .CDB_DEV2ICN_SNP_EN         (CDB_DEV2ICN_SNP_EN        ),
    .CDB_DEV2ICN_DAT_EN         (CDB_DEV2ICN_DAT_EN        ),
    .CDB_REQFLIT_WIDTH          (CDB_REQFLIT_WIDTH         ),
    .CDB_RSPFLIT_WIDTH          (CDB_RSPFLIT_WIDTH         ),
    .CDB_SNPFLIT_WIDTH          (CDB_SNPFLIT_WIDTH         ),
    .CDB_DATFLIT_WIDTH          (CDB_DATFLIT_WIDTH         )
    ) U_CDB_DEV (
    .clk_dev                    (clk_dev                   ),   
    .rstn_dev                   (rstn_dev                  ),   
    .rxsactive_dev              (rxsactive_dev             ),   
    .txsactive_dev              (txsactive_dev             ),   
    .rxlinkactivereq_dev        (rxlinkactivereq_dev       ),   
    .rxlinkactiveack_dev        (rxlinkactiveack_dev       ),   
    .txlinkactivereq_dev        (txlinkactivereq_dev       ),   
    .txlinkactiveack_dev        (txlinkactiveack_dev       ),   
    .rxreq_flitpend_dev         (rxreq_flitpend_dev        ),   
    .rxreq_flitv_dev            (rxreq_flitv_dev           ),   
    .rxreq_flit_dev             (rxreq_flit_dev            ),   
    .rxreq_crdv_dev             (rxreq_crdv_dev            ),   
    .rxrsp_flitpend_dev         (rxrsp_flitpend_dev        ),   
    .rxrsp_flitv_dev            (rxrsp_flitv_dev           ),   
    .rxrsp_flit_dev             (rxrsp_flit_dev            ),   
    .rxrsp_crdv_dev             (rxrsp_crdv_dev            ),   
    .rxsnp_flitpend_dev         (rxsnp_flitpend_dev        ),   
    .rxsnp_flitv_dev            (rxsnp_flitv_dev           ),   
    .rxsnp_flit_dev             (rxsnp_flit_dev            ),   
    .rxsnp_crdv_dev             (rxsnp_crdv_dev            ),   
    .rxdat_flitpend_dev         (rxdat_flitpend_dev        ),   
    .rxdat_flitv_dev            (rxdat_flitv_dev           ),   
    .rxdat_flit_dev             (rxdat_flit_dev            ),   
    .rxdat_crdv_dev             (rxdat_crdv_dev            ),   
    .txreq_flitpend_dev         (txreq_flitpend_dev        ),   
    .txreq_flitv_dev            (txreq_flitv_dev           ),   
    .txreq_flit_dev             (txreq_flit_dev            ),   
    .txreq_crdv_dev             (txreq_crdv_dev            ),   
    .txrsp_flitpend_dev         (txrsp_flitpend_dev        ),   
    .txrsp_flitv_dev            (txrsp_flitv_dev           ),   
    .txrsp_flit_dev             (txrsp_flit_dev            ),   
    .txrsp_crdv_dev             (txrsp_crdv_dev            ),   
    .txsnp_flitpend_dev         (txsnp_flitpend_dev        ),   
    .txsnp_flitv_dev            (txsnp_flitv_dev           ),   
    .txsnp_flit_dev             (txsnp_flit_dev            ),   
    .txsnp_crdv_dev             (txsnp_crdv_dev            ),   
    .txdat_flitpend_dev         (txdat_flitpend_dev        ),   
    .txdat_flitv_dev            (txdat_flitv_dev           ),   
    .txdat_flit_dev             (txdat_flit_dev            ),   
    .txdat_crdv_dev             (txdat_crdv_dev            ),   
    .cdb_fifo_data_dev2icn_req  (cdb_fifo_data_dev2icn_req ),   
    .rptr_r_icn2dev_req         (rptr_r_icn2dev_req        ),   
    .wptr_r_dev2icn_req         (wptr_r_dev2icn_req        ),   
    .cdb_fifo_data_icn2dev_req  (cdb_fifo_data_icn2dev_req ),   
    .rptr_r_dev2icn_req         (rptr_r_dev2icn_req        ),   
    .wptr_r_icn2dev_req         (wptr_r_icn2dev_req        ),   
    .cdb_fifo_data_dev2icn_rsp  (cdb_fifo_data_dev2icn_rsp ),   
    .rptr_r_icn2dev_rsp         (rptr_r_icn2dev_rsp        ),   
    .wptr_r_dev2icn_rsp         (wptr_r_dev2icn_rsp        ),   
    .cdb_fifo_data_icn2dev_rsp  (cdb_fifo_data_icn2dev_rsp ),   
    .rptr_r_dev2icn_rsp         (rptr_r_dev2icn_rsp        ),   
    .wptr_r_icn2dev_rsp         (wptr_r_icn2dev_rsp        ),   
    .cdb_fifo_data_dev2icn_snp  (cdb_fifo_data_dev2icn_snp ),   
    .rptr_r_icn2dev_snp         (rptr_r_icn2dev_snp        ),   
    .wptr_r_dev2icn_snp         (wptr_r_dev2icn_snp        ),   
    .cdb_fifo_data_icn2dev_snp  (cdb_fifo_data_icn2dev_snp ),   
    .rptr_r_dev2icn_snp         (rptr_r_dev2icn_snp        ),   
    .wptr_r_icn2dev_snp         (wptr_r_icn2dev_snp        ),   
    .cdb_fifo_data_dev2icn_dat  (cdb_fifo_data_dev2icn_dat ),
    .rptr_r_icn2dev_dat         (rptr_r_icn2dev_dat        ),
    .wptr_r_dev2icn_dat         (wptr_r_dev2icn_dat        ),
    .cdb_fifo_data_icn2dev_dat  (cdb_fifo_data_icn2dev_dat ),
    .rptr_r_dev2icn_dat         (rptr_r_dev2icn_dat        ),
    .wptr_r_icn2dev_dat         (wptr_r_icn2dev_dat        )
    );

    cdb_icn #(
    .CDB_REQFIFO_DEPTH_ICN      (CDB_REQFIFO_DEPTH_ICN     ),
    .CDB_RSPFIFO_DEPTH_ICN      (CDB_RSPFIFO_DEPTH_ICN     ),
    .CDB_DATFIFO_DEPTH_ICN      (CDB_DATFIFO_DEPTH_ICN     ),
    .CDB_SNPFIFO_DEPTH_ICN      (CDB_SNPFIFO_DEPTH_ICN     ),
    .CDB_REQFIFO_DEPTH_DEV      (CDB_REQFIFO_DEPTH_DEV     ),
    .CDB_RSPFIFO_DEPTH_DEV      (CDB_RSPFIFO_DEPTH_DEV     ),
    .CDB_DATFIFO_DEPTH_DEV      (CDB_DATFIFO_DEPTH_DEV     ),
    .CDB_SNPFIFO_DEPTH_DEV      (CDB_SNPFIFO_DEPTH_DEV     ),
    .CDB_REQBP_FIFO_DEPTH_ICN   (CDB_REQBP_FIFO_DEPTH_ICN  ),
    .CDB_RSPBP_FIFO_DEPTH_ICN   (CDB_RSPBP_FIFO_DEPTH_ICN  ),
    .CDB_DATBP_FIFO_DEPTH_ICN   (CDB_DATBP_FIFO_DEPTH_ICN  ),
    .CDB_SNPBP_FIFO_DEPTH_ICN   (CDB_SNPBP_FIFO_DEPTH_ICN  ),
    .CDB_REQBP_FIFO_DEPTH_DEV   (CDB_REQBP_FIFO_DEPTH_DEV  ),
    .CDB_RSPBP_FIFO_DEPTH_DEV   (CDB_RSPBP_FIFO_DEPTH_DEV  ),
    .CDB_DATBP_FIFO_DEPTH_DEV   (CDB_DATBP_FIFO_DEPTH_DEV  ),
    .CDB_SNPBP_FIFO_DEPTH_DEV   (CDB_SNPBP_FIFO_DEPTH_DEV  ),
    .CDB_ICN2DEV_REQ_BYPASS_EN  (CDB_ICN2DEV_REQ_BYPASS_EN ),
    .CDB_ICN2DEV_RSP_BYPASS_EN  (CDB_ICN2DEV_RSP_BYPASS_EN ),
    .CDB_ICN2DEV_SNP_BYPASS_EN  (CDB_ICN2DEV_SNP_BYPASS_EN ),
    .CDB_ICN2DEV_DAT_BYPASS_EN  (CDB_ICN2DEV_DAT_BYPASS_EN ),
    .CDB_DEV2ICN_REQ_BYPASS_EN  (CDB_DEV2ICN_REQ_BYPASS_EN ),
    .CDB_DEV2ICN_RSP_BYPASS_EN  (CDB_DEV2ICN_RSP_BYPASS_EN ),
    .CDB_DEV2ICN_SNP_BYPASS_EN  (CDB_DEV2ICN_SNP_BYPASS_EN ),
    .CDB_DEV2ICN_DAT_BYPASS_EN  (CDB_DEV2ICN_DAT_BYPASS_EN ),
    .CDB_ICN2DEV_REQ_EN         (CDB_ICN2DEV_REQ_EN        ),
    .CDB_ICN2DEV_RSP_EN         (CDB_ICN2DEV_RSP_EN        ),
    .CDB_ICN2DEV_SNP_EN         (CDB_ICN2DEV_SNP_EN        ),
    .CDB_ICN2DEV_DAT_EN         (CDB_ICN2DEV_DAT_EN        ),
    .CDB_DEV2ICN_REQ_EN         (CDB_DEV2ICN_REQ_EN        ),
    .CDB_DEV2ICN_RSP_EN         (CDB_DEV2ICN_RSP_EN        ),
    .CDB_DEV2ICN_SNP_EN         (CDB_DEV2ICN_SNP_EN        ),
    .CDB_DEV2ICN_DAT_EN         (CDB_DEV2ICN_DAT_EN        ),
    .CDB_REQFLIT_WIDTH          (CDB_REQFLIT_WIDTH         ),
    .CDB_RSPFLIT_WIDTH          (CDB_RSPFLIT_WIDTH         ),
    .CDB_SNPFLIT_WIDTH          (CDB_SNPFLIT_WIDTH         ),
    .CDB_DATFLIT_WIDTH          (CDB_DATFLIT_WIDTH         )
    ) U_CDB_ICN (
    .clk_icn                    (clk_icn                   ),   
    .rstn_icn                   (rstn_icn                  ),   
    .rxsactive_icn              (rxsactive_icn             ),   
    .txsactive_icn              (txsactive_icn             ),   
    .rxlinkactivereq_icn        (rxlinkactivereq_icn       ),   
    .rxlinkactiveack_icn        (rxlinkactiveack_icn       ),   
    .txlinkactivereq_icn        (txlinkactivereq_icn       ),   
    .txlinkactiveack_icn        (txlinkactiveack_icn       ),   
    .rxreq_flitpend_icn         (rxreq_flitpend_icn        ),   
    .rxreq_flitv_icn            (rxreq_flitv_icn           ),   
    .rxreq_flit_icn             (rxreq_flit_icn            ),   
    .rxreq_crdv_icn             (rxreq_crdv_icn            ),   
    .rxrsp_flitpend_icn         (rxrsp_flitpend_icn        ),   
    .rxrsp_flitv_icn            (rxrsp_flitv_icn           ),   
    .rxrsp_flit_icn             (rxrsp_flit_icn            ),   
    .rxrsp_crdv_icn             (rxrsp_crdv_icn            ),   
    .rxsnp_flitpend_icn         (rxsnp_flitpend_icn        ),   
    .rxsnp_flitv_icn            (rxsnp_flitv_icn           ),   
    .rxsnp_flit_icn             (rxsnp_flit_icn            ),   
    .rxsnp_crdv_icn             (rxsnp_crdv_icn            ),   
    .rxdat_flitpend_icn         (rxdat_flitpend_icn        ),   
    .rxdat_flitv_icn            (rxdat_flitv_icn           ),   
    .rxdat_flit_icn             (rxdat_flit_icn            ),   
    .rxdat_crdv_icn             (rxdat_crdv_icn            ),   
    .txreq_flitpend_icn         (txreq_flitpend_icn        ),   
    .txreq_flitv_icn            (txreq_flitv_icn           ),   
    .txreq_flit_icn             (txreq_flit_icn            ),   
    .txreq_crdv_icn             (txreq_crdv_icn            ),   
    .txrsp_flitpend_icn         (txrsp_flitpend_icn        ),   
    .txrsp_flitv_icn            (txrsp_flitv_icn           ),   
    .txrsp_flit_icn             (txrsp_flit_icn            ),   
    .txrsp_crdv_icn             (txrsp_crdv_icn            ),   
    .txsnp_flitpend_icn         (txsnp_flitpend_icn        ),   
    .txsnp_flitv_icn            (txsnp_flitv_icn           ),   
    .txsnp_flit_icn             (txsnp_flit_icn            ),   
    .txsnp_crdv_icn             (txsnp_crdv_icn            ),   
    .txdat_flitpend_icn         (txdat_flitpend_icn        ),   
    .txdat_flitv_icn            (txdat_flitv_icn           ),   
    .txdat_flit_icn             (txdat_flit_icn            ),   
    .txdat_crdv_icn             (txdat_crdv_icn            ),   
    .cdb_fifo_data_icn2dev_req  (cdb_fifo_data_icn2dev_req ),   
    .rptr_r_dev2icn_req         (rptr_r_dev2icn_req        ),   
    .wptr_r_icn2dev_req         (wptr_r_icn2dev_req        ),   
    .cdb_fifo_data_dev2icn_req  (cdb_fifo_data_dev2icn_req ),   
    .rptr_r_icn2dev_req         (rptr_r_icn2dev_req        ),   
    .wptr_r_dev2icn_req         (wptr_r_dev2icn_req        ),   
    .cdb_fifo_data_icn2dev_rsp  (cdb_fifo_data_icn2dev_rsp ),   
    .rptr_r_dev2icn_rsp         (rptr_r_dev2icn_rsp        ),   
    .wptr_r_icn2dev_rsp         (wptr_r_icn2dev_rsp        ),   
    .cdb_fifo_data_dev2icn_rsp  (cdb_fifo_data_dev2icn_rsp ),   
    .rptr_r_icn2dev_rsp         (rptr_r_icn2dev_rsp        ),   
    .wptr_r_dev2icn_rsp         (wptr_r_dev2icn_rsp        ),   
    .cdb_fifo_data_icn2dev_snp  (cdb_fifo_data_icn2dev_snp ),   
    .rptr_r_dev2icn_snp         (rptr_r_dev2icn_snp        ),   
    .wptr_r_icn2dev_snp         (wptr_r_icn2dev_snp        ),   
    .cdb_fifo_data_dev2icn_snp  (cdb_fifo_data_dev2icn_snp ),   
    .rptr_r_icn2dev_snp         (rptr_r_icn2dev_snp        ),   
    .wptr_r_dev2icn_snp         (wptr_r_dev2icn_snp        ),   
    .cdb_fifo_data_icn2dev_dat  (cdb_fifo_data_icn2dev_dat ),
    .rptr_r_dev2icn_dat         (rptr_r_dev2icn_dat        ),
    .wptr_r_icn2dev_dat         (wptr_r_icn2dev_dat        ),
    .cdb_fifo_data_dev2icn_dat  (cdb_fifo_data_dev2icn_dat ),
    .rptr_r_icn2dev_dat         (rptr_r_icn2dev_dat        ),
    .wptr_r_dev2icn_dat         (wptr_r_dev2icn_dat        )
    );


    reset_sync U_RESET_SYNC (
        .reset_n_in        (rx_tlrflit_icn[`NOC_TLR_FLIT_RESET_RANGE]        ),       
        .safeshift         (rx_tlrflit_icn[`NOC_TLR_FLIT_DFTRSTDISABLE_RANGE]),          
        .reset_out         (rstn_icn                                         ),          
        .clock             (clk_icn                                          )
    );


endmodule
