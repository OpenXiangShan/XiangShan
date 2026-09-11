///*********************************************//
//compony:bosc
//
//Project       :   bosc_iommu
//FileName      :   bosc_iommu.sv
//Version       :   1.0
//Author        :   yangyy
//Description   :
//      xxx     :
//      xxxx    :
//      xx      :
//Increase: 1.1
//
//          1.2
///*********************************************//
//`include "atd_define.sv"
module bosc_iommu(
    input                   iommu_clk                   ,
    input                   iommu_rstn                  ,

    input                   iommu_penable_i         ,
    input                   iommu_pwrite_i          ,
    input    [31:0]         iommu_paddr_i           ,
    input                   iommu_psel_i            ,
    input    [31:0]         iommu_pwdata_i          ,
    output   [31:0]         iommu_prdata_o          ,
    output                  iommu_pready_o          ,
    output                  iommu_pslverr_o         ,

    input   [9:0]           iommu_slv_awid            ,
    input   [39:0]          iommu_slv_awaddr          ,
    input   [7:0]           iommu_slv_awlen           ,
    input   [2:0]           iommu_slv_awsize          ,
    input   [1:0]           iommu_slv_awburst         ,
    input                   iommu_slv_awlock          ,
    input   [3:0]           iommu_slv_awcache         ,
    input   [2:0]           iommu_slv_awprot          ,
    input   [3:0]           iommu_slv_awregion        ,
    input   [53:0]          iommu_slv_awuser          ,
    input                   iommu_slv_awvalid         ,
    input   [9:0]           iommu_slv_arid            ,
    input   [39:0]          iommu_slv_araddr          ,
    input   [7:0]           iommu_slv_arlen           ,
    input   [2:0]           iommu_slv_arsize          ,
    input   [1:0]           iommu_slv_arburst         ,
    input                   iommu_slv_arlock          ,
    input   [3:0]           iommu_slv_arcache         ,
    input   [2:0]           iommu_slv_arprot          ,
    input   [3:0]           iommu_slv_arregion        ,
    input   [53:0]          iommu_slv_aruser          ,
    input                   iommu_slv_arvalid         ,
    input   [255:0]         iommu_slv_wdata           ,
    input   [31:0]          iommu_slv_wstrb           ,
    input                   iommu_slv_wlast           ,
    input                   iommu_slv_wvalid          ,
    input   [7:0]           iommu_slv_wuser           ,
    input                   iommu_slv_rready          ,
    input                   iommu_slv_bready          ,
    input   [3:0]           iommu_slv_awqos           ,
    input   [3:0]           iommu_slv_arqos           ,
    output                  iommu_slv_awready        ,
    output                  iommu_slv_arready        ,
    output                  iommu_slv_wready         ,
    output  [9:0]           iommu_slv_rid            ,
    output  [255:0]         iommu_slv_rdata          ,
    output  [1:0]           iommu_slv_rresp          ,
    output                  iommu_slv_rlast          ,
    output                  iommu_slv_rvalid         ,
    output  [7:0]           iommu_slv_ruser          ,
    output  [9:0]           iommu_slv_bid            ,
    output                  iommu_slv_bvalid         ,
    output  [1:0]           iommu_slv_bresp          ,
    output  [7:0]           iommu_slv_buser          ,

    output  [9:0]           iommu_mst_awid       ,
    output  [39:0]          iommu_mst_awaddr     ,
    output  [7:0]           iommu_mst_awlen      ,
    output  [2:0]           iommu_mst_awsize     ,
    output  [1:0]           iommu_mst_awburst    ,
    output                  iommu_mst_awlock     ,
    output  [3:0]           iommu_mst_awcache    ,
    output  [2:0]           iommu_mst_awprot     ,
    output  [3:0]           iommu_mst_awregion   ,
    output  [7:0]           iommu_mst_awuser     ,
    output  [3:0]           iommu_mst_awqos      ,
    output                  iommu_mst_awvalid    ,
    output  [9:0]           iommu_mst_arid       ,
    output  [39:0]          iommu_mst_araddr     ,
    output  [7:0]           iommu_mst_arlen      ,
    output  [2:0]           iommu_mst_arsize     ,
    output  [1:0]           iommu_mst_arburst    ,
    output                  iommu_mst_arlock     ,
    output  [3:0]           iommu_mst_arcache    ,
    output  [2:0]           iommu_mst_arprot     ,
    output  [3:0]           iommu_mst_arregion   ,
    output  [7:0]           iommu_mst_aruser     ,
    output  [3:0]           iommu_mst_arqos      ,
    output                  iommu_mst_arvalid    ,
    output  [255:0]         iommu_mst_wdata      ,
    output  [31:0]          iommu_mst_wstrb      ,
    output                  iommu_mst_wlast      ,
    output                  iommu_mst_wvalid     ,
    output  [7:0]           iommu_mst_wuser     ,
    output                  iommu_mst_rready     ,
    output                  iommu_mst_bready     ,
    input                   iommu_mst_awready   ,
    input                   iommu_mst_arready   ,
    input                   iommu_mst_wready    ,
    input   [9:0]           iommu_mst_rid       ,
    input   [255:0]         iommu_mst_rdata     ,
    input   [1:0]           iommu_mst_rresp     ,
    input                   iommu_mst_rlast     ,
    input                   iommu_mst_rvalid    ,
    input   [7:0]           iommu_mst_ruser     ,
    input   [9:0]           iommu_mst_bid       ,
    input                   iommu_mst_bvalid    ,
    input   [1:0]           iommu_mst_bresp     ,
    input   [7:0]           iommu_mst_buser     ,

    output  [4:0]           iommu_ds_awid             ,
    output  [56-1:0]        iommu_ds_awaddr           ,
    output  [7:0]           iommu_ds_awlen            ,
    output  [2:0]           iommu_ds_awsize           ,
    output  [1:0]           iommu_ds_awburst          ,
    output                  iommu_ds_awlock           ,
    output  [3:0]           iommu_ds_awcache          ,
    output  [2:0]           iommu_ds_awprot           ,
    output  [3:0]           iommu_ds_awqos            ,
    output                  iommu_ds_awvalid          ,
    output  [5:0]           iommu_ds_arid             ,
    output  [56-1:0]        iommu_ds_araddr           ,
    output  [7:0]           iommu_ds_arlen            ,
    output  [2:0]           iommu_ds_arsize           ,
    output  [1:0]           iommu_ds_arburst          ,
    output                  iommu_ds_arlock           ,
    output  [3:0]           iommu_ds_arcache          ,
    output  [2:0]           iommu_ds_arprot           ,
    output  [3:0]           iommu_ds_arqos            ,
    output                  iommu_ds_arvalid          ,
    output  [255:0]         iommu_ds_wdata            ,
    output  [31:0]          iommu_ds_wstrb            ,
    output                  iommu_ds_wlast            ,
    output                  iommu_ds_wvalid           ,
    output                  iommu_ds_rready           ,
    output                  iommu_ds_bready           ,
    input                   iommu_ds_awready         ,
    input                   iommu_ds_arready         ,
    input                   iommu_ds_wready          ,
    input    [5:0]          iommu_ds_rid             ,
    input    [255:0]        iommu_ds_rdata           ,
    input    [1:0]          iommu_ds_rresp           ,
    input                   iommu_ds_rlast           ,
    input                   iommu_ds_rvalid          ,
    input    [4:0]          iommu_ds_bid             ,
    input                   iommu_ds_bvalid          ,
    input    [1:0]          iommu_ds_bresp
);


//    localparam   BUS_ADDR_WIDTH              = 6'd40;
//    localparam   BUS_DATA_WIDTH              = 9'd256;
//    localparam   BUS_SIZE_WIDTH              = $clog2(BUS_DATA_WIDTH);
//    localparam   BUS_STRB_WIDTH              = BUS_DATA_WIDTH/8;
//    localparam   BUS_ID_WIDTH                = 3'd4;
//  localparam   BUS_USER_WIDTH              = 4'd8;
    localparam   CTIF_DATA_WIDTH             = 64;                   // can not change
    localparam   CTIF_STRB_WIDTH             = CTIF_DATA_WIDTH/8;
    localparam   CTIF_ID_WIDTH               = 4;
    localparam   CTIF_DEST_WIDTH             = 4;
    localparam   CTIF_USER_WIDTH             = 1;


    logic                           tc_c2t_rvalid;
    logic                           tc_c2t_rready;
    logic  [CTIF_DATA_WIDTH-1:0]    tc_c2t_rdata;
    logic  [CTIF_STRB_WIDTH-1:0]    tc_c2t_rstrb;
    logic  [CTIF_STRB_WIDTH-1:0]    tc_c2t_rkeep;
    logic                           tc_c2t_rlast;
    logic  [CTIF_ID_WIDTH-1:0]      tc_c2t_rid;
    logic  [CTIF_DEST_WIDTH-1:0]    tc_c2t_rdest;
    logic  [CTIF_USER_WIDTH-1:0]    tc_c2t_ruser;
    logic                           tc_t2c_tvalid;
    logic                           tc_t2c_tready;
    logic   [CTIF_DATA_WIDTH-1:0]   tc_t2c_tdata;
    logic   [CTIF_STRB_WIDTH-1:0]   tc_t2c_tstrb;
    logic   [CTIF_STRB_WIDTH-1:0]   tc_t2c_tkeep;
    logic                           tc_t2c_tlast;
    logic   [CTIF_ID_WIDTH-1:0]     tc_t2c_tid;
    logic   [CTIF_DEST_WIDTH-1:0]   tc_t2c_tdest;
    logic   [CTIF_USER_WIDTH-1:0]   tc_t2c_tuser;

    logic [63:0] iommu_mst_araddr_mux, iommu_mst_awaddr_mux;
//  assign iommu_mst_araddr = iommu_mst_araddr_mux[39:0];
//  assign iommu_mst_awaddr = iommu_mst_awaddr_mux[39:0];

    // SYS-CFG-APB
    logic hw_bypass_en;
    logic mmio_did_pid_en;
    logic device_dma_or_trafficgen;
    logic monitor_src_sel;

//=== TRAFFIC-GEN {{{
//    wire  [31:0]          TRAFFIC_M_AXI_0_araddr  ;
//    wire  [1 :0]          TRAFFIC_M_AXI_0_arburst ;
//    wire  [3 :0]          TRAFFIC_M_AXI_0_arcache ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_arid    ;
//    wire  [7 :0]          TRAFFIC_M_AXI_0_arlen   ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_arlock  ;
//    wire  [2 :0]          TRAFFIC_M_AXI_0_arprot  ;
//    wire  [3 :0]          TRAFFIC_M_AXI_0_arqos   ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_arready ;
//    wire  [2 :0]          TRAFFIC_M_AXI_0_arsize  ;
//    wire  [7 :0]          TRAFFIC_M_AXI_0_aruser  ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_arvalid ;
//    wire  [31:0]          TRAFFIC_M_AXI_0_awaddr  ;
//    wire  [1 :0]          TRAFFIC_M_AXI_0_awburst ;
//    wire  [2 :0]          TRAFFIC_M_AXI_0_awcache ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_awid    ;
//    wire  [7 :0]          TRAFFIC_M_AXI_0_awlen   ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_awlock  ;
//    wire  [2 :0]          TRAFFIC_M_AXI_0_awprot  ;
//    wire  [3 :0]          TRAFFIC_M_AXI_0_awqos   ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_awready ;
//    wire  [2 :0]          TRAFFIC_M_AXI_0_awsize  ;
//    wire  [7 :0]          TRAFFIC_M_AXI_0_awuser  ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_awvalid ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_bid     ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_bready  ;
//    wire  [1 :0]          TRAFFIC_M_AXI_0_bresp   ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_bvalid  ;
//    wire  [31:0]          TRAFFIC_M_AXI_0_rdata   ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_rid     ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_rlast   ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_rready  ;
//    wire  [1 :0]          TRAFFIC_M_AXI_0_rresp   ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_rvalid  ;
//    wire  [31:0]          TRAFFIC_M_AXI_0_wdata   ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_wlast   ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_wready  ;
//    wire  [3 :0]          TRAFFIC_M_AXI_0_wstrb   ;
//    wire  [0 :0]          TRAFFIC_M_AXI_0_wvalid  ;
//
//    assign TRAFFIC_M_AXI_0_araddr  = 'd0;
//    assign TRAFFIC_M_AXI_0_arburst = 'd0;
//    assign TRAFFIC_M_AXI_0_arcache = 'd0;
//    assign TRAFFIC_M_AXI_0_arid    = 'd0;
//    assign TRAFFIC_M_AXI_0_arlen   = 'd0;
//    assign TRAFFIC_M_AXI_0_arlock  = 'd0;
//    assign TRAFFIC_M_AXI_0_arprot  = 'd0;
//    assign TRAFFIC_M_AXI_0_arqos   = 'd0;
//    //assign TRAFFIC_M_AXI_0_arready = 'd0;
//    assign TRAFFIC_M_AXI_0_arsize  = 'd0;
//    assign TRAFFIC_M_AXI_0_aruser  = 'd0;
//    assign TRAFFIC_M_AXI_0_arvalid = 'd0;
//    assign TRAFFIC_M_AXI_0_awaddr  = 'd0;
//    assign TRAFFIC_M_AXI_0_awburst = 'd0;
//    assign TRAFFIC_M_AXI_0_awcache = 'd0;
//    assign TRAFFIC_M_AXI_0_awid    = 'd0;
//    assign TRAFFIC_M_AXI_0_awlen   = 'd0;
//    assign TRAFFIC_M_AXI_0_awlock  = 'd0;
//    assign TRAFFIC_M_AXI_0_awprot  = 'd0;
//    assign TRAFFIC_M_AXI_0_awqos   = 'd0;
//    //assign TRAFFIC_M_AXI_0_awready = 'd0;
//    assign TRAFFIC_M_AXI_0_awsize  = 'd0;
//    assign TRAFFIC_M_AXI_0_awuser  = 'd0;
//    assign TRAFFIC_M_AXI_0_awvalid = 'd0;
//    assign TRAFFIC_M_AXI_0_bid     = 'd0;
//    assign TRAFFIC_M_AXI_0_bready  = 'd0;
//    assign TRAFFIC_M_AXI_0_bresp   = 'd0;
//    assign TRAFFIC_M_AXI_0_bvalid  = 'd0;
//    assign TRAFFIC_M_AXI_0_rdata   = 'd0;
//    assign TRAFFIC_M_AXI_0_rid     = 'd0;
//    assign TRAFFIC_M_AXI_0_rlast   = 'd0;
//    assign TRAFFIC_M_AXI_0_rready  = 'd0;
//    assign TRAFFIC_M_AXI_0_rresp   = 'd0;
//    assign TRAFFIC_M_AXI_0_rvalid  = 'd0;
//    assign TRAFFIC_M_AXI_0_wdata   = 'd0;
//    assign TRAFFIC_M_AXI_0_wlast   = 'd0;
//    //assign TRAFFIC_M_AXI_0_wready  = 'd0;
//    assign TRAFFIC_M_AXI_0_wstrb   = 'd0;
//    assign TRAFFIC_M_AXI_0_wvalid  = 'd0;

wire  [3:0]         TRAFFIC_M_AXI_0_awid      ;
wire  [39:0]        TRAFFIC_M_AXI_0_awaddr    ;
wire  [7:0]         TRAFFIC_M_AXI_0_awlen     ;
wire  [2:0]         TRAFFIC_M_AXI_0_awsize    ;
wire    [1:0]       TRAFFIC_M_AXI_0_awburst   ;
wire                TRAFFIC_M_AXI_0_awlock    ;
wire    [3:0]       TRAFFIC_M_AXI_0_awcache   ;
wire  [2:0]         TRAFFIC_M_AXI_0_awprot    ;
wire  [3:0]         TRAFFIC_M_AXI_0_awregion  ;
wire  [53:0]        TRAFFIC_M_AXI_0_awuser    ;
wire  [3:0]         TRAFFIC_M_AXI_0_awqos     ;
wire                TRAFFIC_M_AXI_0_awvalid   ;
wire  [3:0]         TRAFFIC_M_AXI_0_arid      ;
wire  [39:0]        TRAFFIC_M_AXI_0_araddr    ;
wire  [7:0]         TRAFFIC_M_AXI_0_arlen     ;
wire  [2:0]         TRAFFIC_M_AXI_0_arsize    ;
wire  [1:0]         TRAFFIC_M_AXI_0_arburst   ;
wire                TRAFFIC_M_AXI_0_arlock    ;
wire  [3:0]         TRAFFIC_M_AXI_0_arcache   ;
wire  [2:0]         TRAFFIC_M_AXI_0_arprot    ;
wire  [3:0]         TRAFFIC_M_AXI_0_arregion  ;
wire  [53:0]        TRAFFIC_M_AXI_0_aruser    ;
wire    [3:0]       TRAFFIC_M_AXI_0_arqos     ;
wire                TRAFFIC_M_AXI_0_arvalid   ;
wire  [255:0]       TRAFFIC_M_AXI_0_wdata     ;
wire  [31:0]        TRAFFIC_M_AXI_0_wstrb     ;
wire                TRAFFIC_M_AXI_0_wlast     ;
wire                TRAFFIC_M_AXI_0_wvalid    ;
wire  [7:0]         TRAFFIC_M_AXI_0_wuser     ;
wire                TRAFFIC_M_AXI_0_rready    ;
wire                TRAFFIC_M_AXI_0_bready    ;
wire                TRAFFIC_M_AXI_0_awready   ;
wire                TRAFFIC_M_AXI_0_arready   ;
wire                TRAFFIC_M_AXI_0_wready    ;
wire  [3:0]         TRAFFIC_M_AXI_0_rid       ;
wire  [255:0]       TRAFFIC_M_AXI_0_rdata     ;
wire  [1:0]         TRAFFIC_M_AXI_0_rresp     ;
wire                TRAFFIC_M_AXI_0_rlast     ;
wire                TRAFFIC_M_AXI_0_rvalid    ;
wire  [7:0]         TRAFFIC_M_AXI_0_ruser     ;
wire  [3:0]         TRAFFIC_M_AXI_0_bid       ;
wire                TRAFFIC_M_AXI_0_bvalid    ;
wire  [1:0]         TRAFFIC_M_AXI_0_bresp     ;
wire  [7:0]         TRAFFIC_M_AXI_0_buser     ;


reg [5:0] [31:0]    axi4_ar_cfg   ;
reg                 axi4_ar_start  ;
reg [5:0] [31:0]    axi4_aw_cfg   ;
reg                 axi4_aw_start  ;

wire [31:0] axi4_traffic_rlt;

axi4_traffic U_axi4_traffic_gen(
/*    input                 */  .clk            (iommu_clk      ),
/*    input                 */  .rstn           (iommu_rstn     ),
/*    input   [31:0]        */  .axi4_ar_cfg0   (axi4_ar_cfg[0]   ) ,
/*    input   [31:0]        */  .axi4_ar_cfg1   (axi4_ar_cfg[1]   ) ,
/*    input   [31:0]        */  .axi4_ar_cfg2   (axi4_ar_cfg[2]   ) ,
/*    input   [31:0]        */  .axi4_ar_cfg3   (axi4_ar_cfg[3]   ) ,
/*    input   [31:0]        */  .axi4_ar_cfg4   (axi4_ar_cfg[4]   ) ,
/*    input   [31:0]        */  .axi4_ar_cfg5   (axi4_ar_cfg[5]   ) ,
/*    input                 */  .axi4_ar_start  (axi4_ar_start  )  ,
/*    input   [31:0]        */  .axi4_aw_cfg0   (axi4_aw_cfg[0]   ) ,
/*    input   [31:0]        */  .axi4_aw_cfg1   (axi4_aw_cfg[1]   ) ,
/*    input   [31:0]        */  .axi4_aw_cfg2   (axi4_aw_cfg[2]   ) ,
/*    input   [31:0]        */  .axi4_aw_cfg3   (axi4_aw_cfg[3]   ) ,
/*    input   [31:0]        */  .axi4_aw_cfg4   (axi4_aw_cfg[4]   ) ,
/*    input   [31:0]        */  .axi4_aw_cfg5   (axi4_aw_cfg[5]   ) ,
/*    input                 */  .axi4_aw_start  (axi4_aw_start  )  ,

                                .axi4_traffic_rlt(axi4_traffic_rlt),

/*    output  [3:0]         */  .axi4_awid      (TRAFFIC_M_AXI_0_awid[3:0]      ) ,
/*    output  [39:0]        */  .axi4_awaddr    (TRAFFIC_M_AXI_0_awaddr    ) ,
/*    output  [7:0]         */  .axi4_awlen     (TRAFFIC_M_AXI_0_awlen     ) ,
/*    output  [2:0]         */  .axi4_awsize    (TRAFFIC_M_AXI_0_awsize    ) ,
/*    output    [1:0]       */  .axi4_awburst   (TRAFFIC_M_AXI_0_awburst   ) ,
/*    output                */  .axi4_awlock    (TRAFFIC_M_AXI_0_awlock    ) ,
/*    output    [3:0]       */  .axi4_awcache   (TRAFFIC_M_AXI_0_awcache   ) ,
/*    output  [2:0]         */  .axi4_awprot    (TRAFFIC_M_AXI_0_awprot    ) ,
/*    output  [3:0]         */  .axi4_awregion  (TRAFFIC_M_AXI_0_awregion  ) ,
/*    output  [53:0]        */  .axi4_awuser    (TRAFFIC_M_AXI_0_awuser    ) ,
/*    output  [3:0]         */  .axi4_awqos     (TRAFFIC_M_AXI_0_awqos     ) ,
/*    output                */  .axi4_awvalid   (TRAFFIC_M_AXI_0_awvalid   ) ,
/*    output  [3:0]         */  .axi4_arid      (TRAFFIC_M_AXI_0_arid[3:0]      ) ,
/*    output  [39:0]        */  .axi4_araddr    (TRAFFIC_M_AXI_0_araddr    ) ,
/*    output  [7:0]         */  .axi4_arlen     (TRAFFIC_M_AXI_0_arlen     ) ,
/*    output  [2:0]         */  .axi4_arsize    (TRAFFIC_M_AXI_0_arsize    ) ,
/*    output  [1:0]         */  .axi4_arburst   (TRAFFIC_M_AXI_0_arburst   ) ,
/*    output                */  .axi4_arlock    (TRAFFIC_M_AXI_0_arlock    ) ,
/*    output  [3:0]         */  .axi4_arcache   (TRAFFIC_M_AXI_0_arcache   ) ,
/*    output  [2:0]         */  .axi4_arprot    (TRAFFIC_M_AXI_0_arprot    ) ,
/*    output  [3:0]         */  .axi4_arregion  (TRAFFIC_M_AXI_0_arregion  ) ,
/*    output  [53:0]        */  .axi4_aruser    (TRAFFIC_M_AXI_0_aruser    ) ,
/*    output    [3:0]       */  .axi4_arqos     (TRAFFIC_M_AXI_0_arqos     ) ,
/*    output                */  .axi4_arvalid   (TRAFFIC_M_AXI_0_arvalid   ) ,
/*    output  [255:0]       */  .axi4_wdata     (TRAFFIC_M_AXI_0_wdata     ) ,
/*    output  [31:0]        */  .axi4_wstrb     (TRAFFIC_M_AXI_0_wstrb     ) ,
/*    output                */  .axi4_wlast     (TRAFFIC_M_AXI_0_wlast     ) ,
/*    output                */  .axi4_wvalid    (TRAFFIC_M_AXI_0_wvalid    ) ,
/*    output  [7:0]         */  .axi4_wuser     (TRAFFIC_M_AXI_0_wuser     )   ,
/*    output                */  .axi4_rready    (TRAFFIC_M_AXI_0_rready    ) ,
/*    output                */  .axi4_bready    (TRAFFIC_M_AXI_0_bready    ) ,
/*    input                 */  .axi4_awready   (TRAFFIC_M_AXI_0_awready   ) ,
/*    input                 */  .axi4_arready   (TRAFFIC_M_AXI_0_arready   ) ,
/*    input                 */  .axi4_wready    (TRAFFIC_M_AXI_0_wready    )    ,
/*    input   [3:0]         */  .axi4_rid       (TRAFFIC_M_AXI_0_rid[3:0]       ) ,
/*    input   [255:0]       */  .axi4_rdata     (TRAFFIC_M_AXI_0_rdata     )   ,
/*    input   [1:0]         */  .axi4_rresp     (TRAFFIC_M_AXI_0_rresp     )   ,
/*    input                 */  .axi4_rlast     (TRAFFIC_M_AXI_0_rlast     )   ,
/*    input                 */  .axi4_rvalid    (TRAFFIC_M_AXI_0_rvalid    )    ,
/*    input   [7:0]         */  .axi4_ruser     (TRAFFIC_M_AXI_0_ruser     )   ,
/*    input   [3:0]         */  .axi4_bid       (TRAFFIC_M_AXI_0_bid[3:0]       ) ,
/*    input                 */  .axi4_bvalid    (TRAFFIC_M_AXI_0_bvalid    )    ,
/*    input   [1:0]         */  .axi4_bresp     (TRAFFIC_M_AXI_0_bresp     )   ,
/*    input   [7:0]         */  .axi4_buser     (TRAFFIC_M_AXI_0_buser     )
);



wire [31:0]           MONITOR_AXI_0_araddr    ;
wire [1 :0]           MONITOR_AXI_0_arburst   ;
wire [3 :0]           MONITOR_AXI_0_arcache   ;
wire [9 :0]           MONITOR_AXI_0_arid      ;
wire [7 :0]           MONITOR_AXI_0_arlen     ;
wire [0 :0]           MONITOR_AXI_0_arlock    ;
wire [2 :0]           MONITOR_AXI_0_arprot    ;
wire [0 :0]           MONITOR_AXI_0_arready   ;
wire [2 :0]           MONITOR_AXI_0_arsize    ;
wire [0 :0]           MONITOR_AXI_0_arvalid   ;
wire [31:0]           MONITOR_AXI_0_awaddr    ;
wire [1 :0]           MONITOR_AXI_0_awburst   ;
wire [3 :0]           MONITOR_AXI_0_awcache   ;
wire [9 :0]           MONITOR_AXI_0_awid      ;
wire [7 :0]           MONITOR_AXI_0_awlen     ;
wire [0 :0]           MONITOR_AXI_0_awlock    ;
wire [2 :0]           MONITOR_AXI_0_awprot    ;
wire [0 :0]           MONITOR_AXI_0_awready   ;
wire [2 :0]           MONITOR_AXI_0_awsize    ;
wire [0 :0]           MONITOR_AXI_0_awvalid   ;
wire [9 :0]           MONITOR_AXI_0_bid       ;
wire [0 :0]           MONITOR_AXI_0_bready    ;
wire [1 :0]           MONITOR_AXI_0_bresp     ;
wire [0 :0]           MONITOR_AXI_0_bvalid    ;
wire [31:0]           MONITOR_AXI_0_rdata     ;
wire [9 :0]           MONITOR_AXI_0_rid       ;
wire [0 :0]           MONITOR_AXI_0_rlast     ;
wire [0 :0]           MONITOR_AXI_0_rready    ;
wire [1 :0]           MONITOR_AXI_0_rresp     ;
wire [0 :0]           MONITOR_AXI_0_rvalid    ;
wire [31:0]           MONITOR_AXI_0_wdata     ;
wire [0 :0]           MONITOR_AXI_0_wlast     ;
wire [0 :0]           MONITOR_AXI_0_wready    ;
wire [3 :0]           MONITOR_AXI_0_wstrb     ;
wire [0 :0]           MONITOR_AXI_0_wvalid    ;

reg monitor_capture_event_0 ;
wire monitor_interrupt_0    ;
reg monitor_reset_event_0   ;
reg traffic_start_0         ;
reg traffic_stop_0          ;

assign monitor_interrupt_0 = 1'b0;

//}}}

//=== DEVICE SROUCE SEL {{{
    logic [9:0]             src_slv_awid            ;
    logic [39:0]            src_slv_awaddr          ;
    logic [7:0]             src_slv_awlen           ;
    logic [2:0]             src_slv_awsize          ;
    logic [1:0]             src_slv_awburst         ;
    logic                   src_slv_awlock          ;
    logic [3:0]             src_slv_awcache         ;
    logic [2:0]             src_slv_awprot          ;
    logic [3:0]             src_slv_awregion        ;
    logic [53:0]            src_slv_awuser          ;
    logic                   src_slv_awvalid         ;
    logic [9:0]             src_slv_arid            ;
    logic [39:0]            src_slv_araddr          ;
    logic [7:0]             src_slv_arlen           ;
    logic [2:0]             src_slv_arsize          ;
    logic [1:0]             src_slv_arburst         ;
    logic                   src_slv_arlock          ;
    logic [3:0]             src_slv_arcache         ;
    logic [2:0]             src_slv_arprot          ;
    logic [3:0]             src_slv_arregion        ;
    logic [53:0]            src_slv_aruser          ;
    logic                   src_slv_arvalid         ;
    logic [255:0]           src_slv_wdata           ;
    logic [31:0]            src_slv_wstrb           ;
    logic                   src_slv_wlast           ;
    logic                   src_slv_wvalid          ;
    logic [7:0]             src_slv_wuser           ;
    logic                   src_slv_rready          ;
    logic                   src_slv_bready          ;
    logic [3:0]             src_slv_awqos           ;
    logic [3:0]             src_slv_arqos           ;
    logic                   src_slv_awready         ;
    logic                   src_slv_arready         ;
    logic                   src_slv_wready          ;
    logic [9:0]             src_slv_rid             ;
    logic [255:0]           src_slv_rdata           ;
    logic [1:0]             src_slv_rresp           ;
    logic                   src_slv_rlast           ;
    logic                   src_slv_rvalid          ;
    logic [7:0]             src_slv_ruser           ;
    logic [9:0]             src_slv_bid             ;
    logic                   src_slv_bvalid          ;
    logic [1:0]             src_slv_bresp           ;
    logic [7:0]             src_slv_buser           ;

/*    logic [9:0]    */    assign src_slv_awid       = device_dma_or_trafficgen ? {6'd0, TRAFFIC_M_AXI_0_awid}              : iommu_slv_awid    ;
/*    logic [39:0]   */    assign src_slv_awaddr     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_awaddr            : iommu_slv_awaddr  ;
/*    logic [7:0]    */    assign src_slv_awlen      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_awlen             : iommu_slv_awlen   ;
/*    logic [2:0]    */    assign src_slv_awsize     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_awsize            : iommu_slv_awsize  ;
/*    logic [1:0]    */    assign src_slv_awburst    = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_awburst           : iommu_slv_awburst ;
/*    logic          */    assign src_slv_awlock     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_awlock            : iommu_slv_awlock  ;
/*    logic [3:0]    */    assign src_slv_awcache    = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_awcache           : iommu_slv_awcache ;
/*    logic [2:0]    */    assign src_slv_awprot     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_awprot            : iommu_slv_awprot  ;
/*    logic [3:0]    */    assign src_slv_awregion   = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_awregion          : iommu_slv_awregion;
/*    logic [53:0]   */    assign src_slv_awuser     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_awuser            : iommu_slv_awuser  ;
/*    logic          */    assign src_slv_awvalid    = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_awvalid           : iommu_slv_awvalid ;
/*    logic [9:0]    */    assign src_slv_arid       = device_dma_or_trafficgen ? {6'd0, TRAFFIC_M_AXI_0_arid}              : iommu_slv_arid    ;
/*    logic [39:0]   */    assign src_slv_araddr     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_araddr            : iommu_slv_araddr  ;
/*    logic [7:0]    */    assign src_slv_arlen      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_arlen             : iommu_slv_arlen   ;
/*    logic [2:0]    */    assign src_slv_arsize     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_arsize            : iommu_slv_arsize  ;
/*    logic [1:0]    */    assign src_slv_arburst    = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_arburst           : iommu_slv_arburst ;
/*    logic          */    assign src_slv_arlock     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_arlock            : iommu_slv_arlock  ;
/*    logic [3:0]    */    assign src_slv_arcache    = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_arcache           : iommu_slv_arcache ;
/*    logic [2:0]    */    assign src_slv_arprot     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_arprot            : iommu_slv_arprot  ;
/*    logic [3:0]    */    assign src_slv_arregion   = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_arregion          : iommu_slv_arregion;
/*    logic [53:0]   */    assign src_slv_aruser     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_aruser            : iommu_slv_aruser  ;
/*    logic          */    assign src_slv_arvalid    = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_arvalid           : iommu_slv_arvalid ;
/*    logic [255:0]  */    assign src_slv_wdata      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_wdata             : iommu_slv_wdata   ;
/*    logic [31:0]   */    assign src_slv_wstrb      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_wstrb             : iommu_slv_wstrb   ;
/*    logic          */    assign src_slv_wlast      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_wlast             : iommu_slv_wlast   ;
/*    logic          */    assign src_slv_wvalid     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_wvalid            : iommu_slv_wvalid  ;
/*    logic [7:0]    */    assign src_slv_wuser      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_wuser             : iommu_slv_wuser   ;
/*    logic          */    assign src_slv_rready     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_rready            : iommu_slv_rready  ;
/*    logic          */    assign src_slv_bready     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_bready            : iommu_slv_bready  ;
/*    logic [3:0]    */    assign src_slv_awqos      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_awqos             : iommu_slv_awqos   ;
/*    logic [3:0]    */    assign src_slv_arqos      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_arqos             : iommu_slv_arqos   ;

/*    logic          */    //assign src_slv_awready    = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_awready : iommu_slv_awready ;
/*    logic          */    //assign src_slv_arready    = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_arready : iommu_slv_arready ;
/*    logic          */    //assign src_slv_wready     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_wready  : iommu_slv_wready  ;
/*    logic          */    assign TRAFFIC_M_AXI_0_awready   = device_dma_or_trafficgen ? src_slv_awready : 1'b1;
/*    logic          */    assign TRAFFIC_M_AXI_0_arready   = device_dma_or_trafficgen ? src_slv_arready : 1'b1;
/*    logic          */    assign TRAFFIC_M_AXI_0_wready    = device_dma_or_trafficgen ? src_slv_wready  : 1'b1;
/*    logic          */    assign iommu_slv_awready         = ~device_dma_or_trafficgen ? src_slv_awready : 1'b1;
/*    logic          */    assign iommu_slv_arready         = ~device_dma_or_trafficgen ? src_slv_arready : 1'b1;
/*    logic          */    assign iommu_slv_wready          = ~device_dma_or_trafficgen ? src_slv_wready  : 1'b1;

/*    logic [3:0]    */    //assign src_slv_rid        = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_rid     : iommu_slv_rid     ;
/*    logic [255:0]  */    //assign src_slv_rdata      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_rdata   : iommu_slv_rdata   ;
/*    logic [1:0]    */    //assign src_slv_rresp      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_rresp   : iommu_slv_rresp   ;
/*    logic          */    //assign src_slv_rlast      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_rlast   : iommu_slv_rlast   ;
/*    logic          */    //assign src_slv_rvalid     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_rvalid  : iommu_slv_rvalid  ;
/*    logic [7:0]    */    //assign src_slv_ruser      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_ruser   : iommu_slv_ruser   ;
/*    logic [3:0]    */    //assign src_slv_bid        = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_bid     : iommu_slv_bid     ;
/*    logic          */    //assign src_slv_bvalid     = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_bvalid  : iommu_slv_bvalid  ;
/*    logic [1:0]    */    //assign src_slv_bresp      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_bresp   : iommu_slv_bresp   ;
/*    logic [7:0]    */    //assign src_slv_buser      = device_dma_or_trafficgen ? TRAFFIC_M_AXI_0_buser   : iommu_slv_buser   ;

/*    output  [0 :0] */    assign  TRAFFIC_M_AXI_0_bid      = device_dma_or_trafficgen ? src_slv_bid[3:0]            : 'd0;
/*    output  [1 :0] */    assign  TRAFFIC_M_AXI_0_bresp    = device_dma_or_trafficgen ? src_slv_bresp [1 :0]   : 'd0;
/*    output  [0 :0] */    assign  TRAFFIC_M_AXI_0_bvalid   = device_dma_or_trafficgen ? src_slv_bvalid         : 'd0;
/*    output  [31:0] */    assign  TRAFFIC_M_AXI_0_rdata    = device_dma_or_trafficgen ? src_slv_rdata [255:0]  : 'd0;
/*    output  [0 :0] */    assign  TRAFFIC_M_AXI_0_rid      = device_dma_or_trafficgen ? src_slv_rid[3:0]            : 'd0;
/*    output  [0 :0] */    assign  TRAFFIC_M_AXI_0_rlast    = device_dma_or_trafficgen ? src_slv_rlast          : 'd0;
/*    output  [1 :0] */    assign  TRAFFIC_M_AXI_0_rresp    = device_dma_or_trafficgen ? src_slv_rresp [1 :0]   : 'd0;
/*    output  [0 :0] */    assign  TRAFFIC_M_AXI_0_rvalid   = device_dma_or_trafficgen ? src_slv_rvalid         : 'd0;
                           assign  TRAFFIC_M_AXI_0_ruser    = device_dma_or_trafficgen ? src_slv_ruser [7:0]    : 'd0;

/*    logic [3:0]    */    assign iommu_slv_rid             = ~device_dma_or_trafficgen ? src_slv_rid    : 'd0;
/*    logic [255:0]  */    assign iommu_slv_rdata           = ~device_dma_or_trafficgen ? src_slv_rdata  : 'd0;
/*    logic [1:0]    */    assign iommu_slv_rresp           = ~device_dma_or_trafficgen ? src_slv_rresp  : 'd0;
/*    logic          */    assign iommu_slv_rlast           = ~device_dma_or_trafficgen ? src_slv_rlast  : 'd0;
/*    logic          */    assign iommu_slv_rvalid          = ~device_dma_or_trafficgen ? src_slv_rvalid : 'd0;
/*    logic [7:0]    */    assign iommu_slv_ruser           = ~device_dma_or_trafficgen ? src_slv_ruser  : 'd0;
/*    logic [3:0]    */    assign iommu_slv_bid             = ~device_dma_or_trafficgen ? src_slv_bid    : 'd0;
/*    logic          */    assign iommu_slv_bvalid          = ~device_dma_or_trafficgen ? src_slv_bvalid : 'd0;
/*    logic [1:0]    */    assign iommu_slv_bresp           = ~device_dma_or_trafficgen ? src_slv_bresp  : 'd0;
/*    logic [7:0]    */    assign iommu_slv_buser           = ~device_dma_or_trafficgen ? src_slv_buser  : 'd0;

//}}}

//=== SYS CFG APB {{{
reg                iommu_sf_rstn;

wire [30:0]         iommu_test_paddr        ;
wire                iommu_test_penable      ;
reg [31:0]          iommu_test_prdata       ;
wire [0:0]          iommu_test_pready       ;
wire [0:0]          iommu_test_psel         ;
wire [0:0]          iommu_test_pslverr      ;
wire [31:0]         iommu_test_pwdata       ;
wire [0:0]          iommu_test_pwrite       ;

wire                iommu_inst_penable;
wire                iommu_inst_psel;
wire [31:0]         iommu_inst_prdata;


logic                                       axi_mon_penable_i ;
logic                                       axi_mon_pwrite_i  ;
logic [31:0]                                axi_mon_paddr_i   ;
logic                                       axi_mon_psel_i    ;
logic [31:0]                                axi_mon_pwdata_i  ;
logic [31:0]                                axi_mon_prdata_o  ;
logic                                       axi_mon_pready_o  ;
logic                                       axi_mon_pslverr_o ;
assign axi_mon_paddr_i          = iommu_paddr_i;
assign axi_mon_penable_i        = iommu_penable_i   & (iommu_paddr_i[15:12]=='hf);
assign axi_mon_psel_i           = iommu_psel_i      & (iommu_paddr_i[15:12]=='hf);
assign axi_mon_pwdata_i         = iommu_pwdata_i;
assign axi_mon_pwrite_i         = iommu_pwrite_i;

assign iommu_test_paddr         = iommu_paddr_i;
assign iommu_test_penable       = iommu_penable_i   & (iommu_paddr_i[15:12]=='d1);
assign iommu_inst_penable       = iommu_penable_i   & (iommu_paddr_i[15:12]==0);
assign iommu_test_pready        = 'd0;
assign iommu_test_psel          = iommu_psel_i      & (iommu_paddr_i[15:12]=='d1);
assign iommu_inst_psel          = iommu_psel_i      & (iommu_paddr_i[15:12]==0);
assign iommu_test_pslverr       ='d0;
assign iommu_test_pwdata        = iommu_pwdata_i;
assign iommu_test_pwrite        = iommu_pwrite_i;
//assign iommu_prdata_o           = iommu_paddr_i[15:12]=='d0 ? iommu_inst_prdata : iommu_test_prdata;
assign iommu_prdata_o           = iommu_paddr_i[15:12]=='d0 ? iommu_inst_prdata :
                                  iommu_paddr_i[15:12]=='d1 ? iommu_test_prdata :
                                  iommu_paddr_i[15:12]=='hf ? axi_mon_prdata_o  : 32'hfffe2202;

    // addr:'d0
    // [0]: hw_bypass_en. 1:enable, 0:disable; default:1
    // [1]: mmio_did_pid_en. 1:enable, 0:disable; default:0
    // [2]: device_dma_or_trafficgen. 1: axi-traffic-gen 0: dma
    // [3]: monitor_src_sel 1:out MST port 0:muxed in SLV port
    // [30:4]: reserved
    always@(posedge iommu_clk or negedge iommu_rstn) begin
        if(~iommu_rstn) begin
            hw_bypass_en            <= 1'b0;
            mmio_did_pid_en         <= 1'b0;
            device_dma_or_trafficgen<= 'd0;
            monitor_src_sel         <= 'd0;
            iommu_sf_rstn           <= 1'b1;
        end
        else begin
            iommu_sf_rstn <= 1'b1;
            if(iommu_test_penable & iommu_test_pwrite & iommu_test_psel & iommu_test_paddr[11:0]=='d0) begin
                hw_bypass_en            <= iommu_test_pwdata[0];
                mmio_did_pid_en         <= iommu_test_pwdata[1];
                device_dma_or_trafficgen<= iommu_test_pwdata[2];
                monitor_src_sel         <= iommu_test_pwdata[3];
                iommu_sf_rstn           <= iommu_test_pwdata[31];
            end
        end
    end

    // addr: 'd16
    // [0] : monitor_capture_event_0    ,
    // [1] : monitor_reset_event_0      ,
    // [2] : traffic_start_0            ,
    // [3] : traffic_stop_0             ,
    // [4] : monitor_interrupt_0, RO
    // [31:5]: reserved

//    // tmp auto triger for traffic-gen
//    reg [15:0] autocnt;
//    always@(posedge iommu_clk or negedge iommu_rstn) begin
//        if(~iommu_rstn) begin
//            autocnt <= 'd0;
//        end
//        else begin
//            autocnt <= autocnt + 'd1;
//        end
//    end

    always@(posedge iommu_clk or negedge iommu_rstn) begin
        if(~iommu_rstn) begin
            monitor_capture_event_0         <= 1'b0;
            monitor_reset_event_0           <= 1'b0;
            traffic_start_0                 <= 'd0;
            traffic_stop_0                  <= 'd0;
        end
        else begin
            if(iommu_test_penable & iommu_test_pwrite & iommu_test_psel & iommu_test_paddr[11:0]=='d16) begin
                monitor_capture_event_0     <= iommu_test_pwdata[0];
                monitor_reset_event_0       <= iommu_test_pwdata[1];
                traffic_start_0             <= iommu_test_pwdata[2];
                traffic_stop_0              <= iommu_test_pwdata[3];
            end
            else begin
                monitor_capture_event_0     <= 1'b0;
                monitor_reset_event_0       <= 1'b0;
                traffic_start_0             <= 1'b0;
                traffic_stop_0              <= 1'b0;
            end
//            // tmp auto triger for traffic-gen
//            else if(autocnt[7]==1'b1) begin
//                traffic_start_0 <= 1'b1;
//            end
//            else if(autocnt[7:0]==8'b1000_0001) begin
//                traffic_start_0 <= 1'b0;
//            end
//            else if(autocnt[15]==1'b1) begin
//                traffic_start_0 <= 1'b0;
//                traffic_stop_0  <= 1'b1;
//            end
        end
    end

    logic [7:0] [23:0]  did;
    logic [7:0] [19:0]  pid;
    logic [7:0] [0:0]   pid_v;
    logic [7:0] [0:0]   transed;
    logic [7:0] [31:0]  addr_31_0_start;
    logic [7:0] [31:0]  addr_63_32_start;
    logic [7:0] [31:0]  addr_31_0_end;
    logic [7:0] [31:0]  addr_63_32_end;
    // addr:'d128 + 0  + 4 * i -> did[i]
    //      'd128 + 32 + 4 * i -> pid[i]
    //      'd128 + 64 + 8 * i -> {addr_63_32_start[i], addr_31_0_start[i]}
    //      'd128 + 128+ 8 * i -> {addr_63_32_end[i],   addr_31_0_end[i]}
genvar i;
generate
    for(i=0; i<8; i++) begin
        always@(posedge iommu_clk or negedge iommu_rstn) begin
            if(~iommu_rstn) begin
                did[i]  <= 'd0;
            end
            else begin
                if(iommu_test_penable & iommu_test_pwrite & iommu_test_psel & iommu_test_paddr[11:0]==('d128+'d0+'d4*i)) begin
                    did[i]  <= iommu_test_pwdata[23:0];
                end
            end
        end

        always@(posedge iommu_clk or negedge iommu_rstn) begin
            if(~iommu_rstn) begin
                pid[i]  <= 'd0;
                pid_v[i]<= 'b0;
                transed[i]<= 1'b0;
            end
            else begin
                if(iommu_test_penable & iommu_test_pwrite & iommu_test_psel & iommu_test_paddr[11:0]==('d128+'d32+'d4*i)) begin
                    pid[i]  <= iommu_test_pwdata[19:0];
                    pid_v[i]<= iommu_test_pwdata[20];
                    transed[i]<= iommu_test_pwdata[21];
                end
            end
        end

        always@(posedge iommu_clk or negedge iommu_rstn) begin
            if(~iommu_rstn) begin
                addr_31_0_start[i]  <= 32'hffff_ffff;
            end
            else begin
                if(iommu_test_penable & iommu_test_pwrite & iommu_test_psel & iommu_test_paddr[11:0]==('d128+'d64+'d8*i)) begin
                    addr_31_0_start[i]  <= iommu_test_pwdata[31:0];
                end
            end
        end

        always@(posedge iommu_clk or negedge iommu_rstn) begin
            if(~iommu_rstn) begin
                addr_63_32_start[i] <= 32'hffff_ffff;
            end
            else begin
                if(iommu_test_penable & iommu_test_pwrite & iommu_test_psel & iommu_test_paddr[11:0]==('d128+'d64+'d8*i+'d4)) begin
                    addr_63_32_start[i]  <= iommu_test_pwdata[31:0];
                end
            end
        end

        always@(posedge iommu_clk or negedge iommu_rstn) begin
            if(~iommu_rstn) begin
                addr_31_0_end[i]    <= 32'hffff_ffff;
            end
            else begin
                if(iommu_test_penable & iommu_test_pwrite & iommu_test_psel & iommu_test_paddr[11:0]==('d128+'d128+'d8*i)) begin
                    addr_31_0_end[i]  <= iommu_test_pwdata[31:0];
                end
            end
        end

        always@(posedge iommu_clk or negedge iommu_rstn) begin
            if(~iommu_rstn) begin
                addr_63_32_end[i]   <= 32'hffff_ffff;
            end
            else begin
                if(iommu_test_penable & iommu_test_pwrite & iommu_test_psel & iommu_test_paddr[11:0]==('d128+'d128+'d8*i+'d4)) begin
                    addr_63_32_end[i]  <= iommu_test_pwdata[31:0];
                end
            end
        end


    end
endgenerate


    // addr: 'd512 + 0 + j*4 -> axi4_ar_cfg<i>
    //[5:0] [31:0]        axi4_ar_cfg   ;
genvar j;
generate
    for(j=0; j<6; j++) begin
        always@(posedge iommu_clk or negedge iommu_rstn) begin
            if(~iommu_rstn) begin
                axi4_ar_cfg[j]  <= 'd0;
            end
            else begin
                if(iommu_test_penable & iommu_test_pwrite & iommu_test_psel & iommu_test_paddr[11:0]==('d512+'d0+'d4*j)) begin
                    axi4_ar_cfg[j]  <= iommu_test_pwdata[31:0];
                end
            end
        end
    end
endgenerate

    // addr: 'd512 + 'd32 + k*4 -> axi4_aw_cfg<i>
    //[5:0] [31:0]        axi4_aw_cfg   ;
genvar k;
generate
    for(k=0; k<6; k++) begin
        always@(posedge iommu_clk or negedge iommu_rstn) begin
            if(~iommu_rstn) begin
                axi4_aw_cfg[k]  <= 'd0;
            end
            else begin
                if(iommu_test_penable & iommu_test_pwrite & iommu_test_psel & iommu_test_paddr[11:0]==('d512+'d32+'d4*k)) begin
                    axi4_aw_cfg[k]  <= iommu_test_pwdata[31:0];
                end
            end
        end
    end
endgenerate

    // addr: 'd512 + 'd64
    // [0] : axi4_ar_start  ;
    // [1] : axi4_aw_start  ;

    always@(posedge iommu_clk or negedge iommu_rstn) begin
        if(~iommu_rstn) begin
            axi4_ar_start   <= 1'b0;
            axi4_aw_start   <= 1'b0;
        end
        else begin
            axi4_ar_start <= 1'b0;
            axi4_aw_start <= 1'b0;
            if(iommu_test_penable & iommu_test_pwrite & iommu_test_psel & iommu_test_paddr[11:0]==('d512+'d64)) begin
                axi4_ar_start   <= iommu_test_pwdata[0];
                axi4_aw_start   <= iommu_test_pwdata[1];
            end
        end
    end






    assign iommu_test_pready = 1'b1;
    assign iommu_test_pslverr= 1'b0;
    always@(posedge iommu_clk or negedge iommu_rstn) begin
        if(~iommu_rstn)
            iommu_test_prdata <= 'd0;
        else begin
            if(~iommu_test_pwrite & iommu_test_psel) begin
                case(iommu_test_paddr[11:0])
                'd0:    iommu_test_prdata <= {
                                                iommu_sf_rstn,
                                                27'd0,
                                                monitor_src_sel,
                                                device_dma_or_trafficgen,
                                                mmio_did_pid_en,
                                                hw_bypass_en
                                                };
                'd16:   iommu_test_prdata <= {
                                                27'd0,
                                                monitor_interrupt_0,
                                                traffic_stop_0,
                                                traffic_start_0,
                                                monitor_reset_event_0,
                                                monitor_capture_event_0
                                                };
                'd128:  iommu_test_prdata <= {8'd0, did[0]};
                'd132:  iommu_test_prdata <= {8'd0, did[1]};
                'd136:  iommu_test_prdata <= {8'd0, did[2]};
                'd140:  iommu_test_prdata <= {8'd0, did[3]};
                'd144:  iommu_test_prdata <= {8'd0, did[4]};
                'd148:  iommu_test_prdata <= {8'd0, did[5]};
                'd152:  iommu_test_prdata <= {8'd0, did[6]};
                'd156:  iommu_test_prdata <= {8'd0, did[7]};

                'd160:  iommu_test_prdata <= {10'd0, transed[0], pid_v[0], pid[0]};
                'd164:  iommu_test_prdata <= {10'd0, transed[1], pid_v[1], pid[1]};
                'd168:  iommu_test_prdata <= {10'd0, transed[2], pid_v[2], pid[2]};
                'd172:  iommu_test_prdata <= {10'd0, transed[3], pid_v[3], pid[3]};
                'd176:  iommu_test_prdata <= {10'd0, transed[4], pid_v[4], pid[4]};
                'd180:  iommu_test_prdata <= {10'd0, transed[5], pid_v[5], pid[5]};
                'd184:  iommu_test_prdata <= {10'd0, transed[6], pid_v[6], pid[6]};
                'd188:  iommu_test_prdata <= {10'd0, transed[7], pid_v[7], pid[7]};

                'd192:  iommu_test_prdata <= addr_31_0_start[0];
                'd200:  iommu_test_prdata <= addr_31_0_start[1];
                'd208:  iommu_test_prdata <= addr_31_0_start[2];
                'd216:  iommu_test_prdata <= addr_31_0_start[3];
                'd224:  iommu_test_prdata <= addr_31_0_start[4];
                'd232:  iommu_test_prdata <= addr_31_0_start[5];
                'd240:  iommu_test_prdata <= addr_31_0_start[6];
                'd248:  iommu_test_prdata <= addr_31_0_start[7];

                'd196:  iommu_test_prdata <= addr_63_32_start[0];
                'd204:  iommu_test_prdata <= addr_63_32_start[1];
                'd212:  iommu_test_prdata <= addr_63_32_start[2];
                'd220:  iommu_test_prdata <= addr_63_32_start[3];
                'd228:  iommu_test_prdata <= addr_63_32_start[4];
                'd236:  iommu_test_prdata <= addr_63_32_start[5];
                'd244:  iommu_test_prdata <= addr_63_32_start[6];
                'd252:  iommu_test_prdata <= addr_63_32_start[7];

                'd256:  iommu_test_prdata <= addr_31_0_end[0];
                'd264:  iommu_test_prdata <= addr_31_0_end[1];
                'd272:  iommu_test_prdata <= addr_31_0_end[2];
                'd280:  iommu_test_prdata <= addr_31_0_end[3];
                'd288:  iommu_test_prdata <= addr_31_0_end[4];
                'd296:  iommu_test_prdata <= addr_31_0_end[5];
                'd304:  iommu_test_prdata <= addr_31_0_end[6];
                'd312:  iommu_test_prdata <= addr_31_0_end[7];

                'd260:  iommu_test_prdata <= addr_63_32_end[0];
                'd268:  iommu_test_prdata <= addr_63_32_end[1];
                'd276:  iommu_test_prdata <= addr_63_32_end[2];
                'd284:  iommu_test_prdata <= addr_63_32_end[3];
                'd292:  iommu_test_prdata <= addr_63_32_end[4];
                'd300:  iommu_test_prdata <= addr_63_32_end[5];
                'd308:  iommu_test_prdata <= addr_63_32_end[6];
                'd316:  iommu_test_prdata <= addr_63_32_end[7];

                'd512:  iommu_test_prdata <= axi4_ar_cfg[0];
                'd516:  iommu_test_prdata <= axi4_ar_cfg[1];
                'd520:  iommu_test_prdata <= axi4_ar_cfg[2];
                'd524:  iommu_test_prdata <= axi4_ar_cfg[3];
                'd528:  iommu_test_prdata <= axi4_ar_cfg[4];
                'd532:  iommu_test_prdata <= axi4_ar_cfg[5];


                'd544:  iommu_test_prdata <= axi4_aw_cfg[0];
                'd548:  iommu_test_prdata <= axi4_aw_cfg[1];
                'd552:  iommu_test_prdata <= axi4_aw_cfg[2];
                'd556:  iommu_test_prdata <= axi4_aw_cfg[3];
                'd560:  iommu_test_prdata <= axi4_aw_cfg[4];
                'd564:  iommu_test_prdata <= axi4_aw_cfg[5];

                'd576:  iommu_test_prdata <= {30'd0, axi4_aw_start, axi4_ar_start};
                'd580:  iommu_test_prdata <= axi4_traffic_rlt;
                default:iommu_test_prdata <= 'd0;
                endcase
            end
        end
    end

//}}}

//=== IOMMU BYPASS SIGNAL AND CONNECTION {{{
    logic  [9:0]            bypass_slv_awid      ;
    logic  [39:0]           bypass_slv_awaddr    ;
    logic  [7:0]            bypass_slv_awlen     ;
    logic  [2:0]            bypass_slv_awsize    ;
    logic  [1:0]            bypass_slv_awburst   ;
    logic                   bypass_slv_awlock    ;
    logic  [3:0]            bypass_slv_awcache   ;
    logic  [2:0]            bypass_slv_awprot    ;
    logic  [3:0]            bypass_slv_awregion  ;
    logic  [53:0]           bypass_slv_awuser      ;
    logic                   bypass_slv_awvalid   ;
    logic  [9:0]            bypass_slv_arid      ;
    logic  [39:0]           bypass_slv_araddr    ;
    logic  [7:0]            bypass_slv_arlen     ;
    logic  [2:0]            bypass_slv_arsize    ;
    logic  [1:0]            bypass_slv_arburst   ;
    logic                   bypass_slv_arlock    ;
    logic  [3:0]            bypass_slv_arcache   ;
    logic  [2:0]            bypass_slv_arprot    ;
    logic  [3:0]            bypass_slv_arregion  ;
    logic  [53:0]           bypass_slv_aruser      ;
    logic                   bypass_slv_arvalid   ;
    logic  [255:0]          bypass_slv_wdata     ;
    logic  [31:0]           bypass_slv_wstrb     ;
    logic                   bypass_slv_wlast     ;
    logic                   bypass_slv_wvalid    ;
    logic  [7:0]            bypass_slv_wuser       ;
    logic                   bypass_slv_rready    ;
    logic                   bypass_slv_bready    ;
    logic  [3:0]            bypass_slv_awqos     ;
    logic  [3:0]            bypass_slv_arqos     ;
    logic                   bypass_slv_awready   ;
    logic                   bypass_slv_arready   ;
    logic                   bypass_slv_wready    ;
    logic  [9:0]            bypass_slv_rid       ;
    logic  [255:0]          bypass_slv_rdata     ;
    logic  [1:0]            bypass_slv_rresp     ;
    logic                   bypass_slv_rlast     ;
    logic                   bypass_slv_rvalid    ;
    logic  [7:0]            bypass_slv_ruser     ;
    logic  [9:0]            bypass_slv_bid       ;
    logic                   bypass_slv_bvalid    ;
    logic  [1:0]            bypass_slv_bresp     ;
    logic  [7:0]            bypass_slv_buser     ;


    logic  [9:0]            acd_slv_awid      ;
    logic  [39:0]           acd_slv_awaddr    ;
    logic  [7:0]            acd_slv_awlen     ;
    logic  [2:0]            acd_slv_awsize    ;
    logic  [1:0]            acd_slv_awburst   ;
    logic                   acd_slv_awlock    ;
    logic  [3:0]            acd_slv_awcache   ;
    logic  [2:0]            acd_slv_awprot    ;
    logic  [3:0]            acd_slv_awregion  ;
    logic  [53:0]           acd_slv_awuser     ;
    logic                   acd_slv_awvalid   ;
    logic  [9:0]            acd_slv_arid      ;
    logic  [39:0]           acd_slv_araddr    ;
    logic  [7:0]            acd_slv_arlen     ;
    logic  [2:0]            acd_slv_arsize    ;
    logic  [1:0]            acd_slv_arburst   ;
    logic                   acd_slv_arlock    ;
    logic  [3:0]            acd_slv_arcache   ;
    logic  [2:0]            acd_slv_arprot    ;
    logic  [3:0]            acd_slv_arregion  ;
    logic  [53:0]           acd_slv_aruser     ;
    logic                   acd_slv_arvalid   ;
    logic  [255:0]          acd_slv_wdata     ;
    logic  [31:0]           acd_slv_wstrb     ;
    logic                   acd_slv_wlast     ;
    logic                   acd_slv_wvalid    ;
    logic  [7:0]            acd_slv_wuser      ;
    logic                   acd_slv_rready    ;
    logic                   acd_slv_bready    ;
    logic  [3:0]            acd_slv_awqos     ;
    logic  [3:0]            acd_slv_arqos     ;
    logic                   acd_slv_awready   ;
    logic                   acd_slv_arready   ;
    logic                   acd_slv_wready    ;
    logic  [9:0]            acd_slv_rid       ;
    logic  [255:0]          acd_slv_rdata     ;
    logic  [1:0]            acd_slv_rresp     ;
    logic                   acd_slv_rlast     ;
    logic                   acd_slv_rvalid    ;
    logic  [7:0]            acd_slv_ruser     ;
    logic  [9:0]            acd_slv_bid       ;
    logic                   acd_slv_bvalid    ;
    logic  [1:0]            acd_slv_bresp     ;
    logic  [7:0]            acd_slv_buser     ;

    assign bypass_slv_awid        = src_slv_awid        ;
    assign bypass_slv_awaddr      = src_slv_awaddr      ;
    assign bypass_slv_awlen       = src_slv_awlen       ;
    assign bypass_slv_awsize      = src_slv_awsize      ;
    assign bypass_slv_awburst     = src_slv_awburst     ;
    assign bypass_slv_awlock      = src_slv_awlock      ;
    assign bypass_slv_awcache     = src_slv_awcache     ;
    assign bypass_slv_awprot      = src_slv_awprot      ;
    assign bypass_slv_awregion    = src_slv_awregion    ;
    assign bypass_slv_awuser      = src_slv_awuser      ;
    assign bypass_slv_awvalid     = src_slv_awvalid     & hw_bypass_en;
    assign bypass_slv_arid        = src_slv_arid        ;
    assign bypass_slv_araddr      = src_slv_araddr      ;
    assign bypass_slv_arlen       = src_slv_arlen       ;
    assign bypass_slv_arsize      = src_slv_arsize      ;
    assign bypass_slv_arburst     = src_slv_arburst     ;
    assign bypass_slv_arlock      = src_slv_arlock      ;
    assign bypass_slv_arcache     = src_slv_arcache     ;
    assign bypass_slv_arprot      = src_slv_arprot      ;
    assign bypass_slv_arregion    = src_slv_arregion    ;
    assign bypass_slv_aruser      = src_slv_aruser      ;
    assign bypass_slv_arvalid     = src_slv_arvalid     & hw_bypass_en;
    assign bypass_slv_wdata       = src_slv_wdata       ;
    assign bypass_slv_wstrb       = src_slv_wstrb       ;
    assign bypass_slv_wlast       = src_slv_wlast       ;
    assign bypass_slv_wvalid      = src_slv_wvalid      & hw_bypass_en;
    assign bypass_slv_wuser       = src_slv_wuser       ;
    assign bypass_slv_rready      = src_slv_rready      & hw_bypass_en;
    assign bypass_slv_bready      = src_slv_bready      & hw_bypass_en;
    assign bypass_slv_awqos       = src_slv_awqos       ;
    assign bypass_slv_arqos       = src_slv_arqos       ;
//    assign bypass_slv_awready     = src_slv_awready     ;
//    assign bypass_slv_arready     = src_slv_arready     ;
//    assign bypass_slv_wready      = src_slv_wready      ;
//    assign bypass_slv_rid         = src_slv_rid         ;
//    assign bypass_slv_rdata       = src_slv_rdata       ;
//    assign bypass_slv_rresp       = src_slv_rresp       ;
//    assign bypass_slv_rlast       = src_slv_rlast       ;
//    assign bypass_slv_rvalid      = src_slv_rvalid      & hw_bypass_en;
//    assign bypass_slv_ruser       = src_slv_ruser       ;
//    assign bypass_slv_bid         = src_slv_bid         ;
//    assign bypass_slv_bvalid      = src_slv_bvalid      & hw_bypass_en;
//    assign bypass_slv_bresp       = src_slv_bresp       ;
//    assign bypass_slv_buser       = src_slv_buser       ;
    logic [23:0] ar_mmio_did;
    logic [19:0] ar_mmio_pid;
    logic        ar_mmio_pid_v;
    logic        ar_mmio_transed;
    logic [23:0] aw_mmio_did;
    logic [19:0] aw_mmio_pid;
    logic        aw_mmio_pid_v;
    logic        aw_mmio_transed;

    assign       aw_mmio_did =  (acd_slv_awaddr>={addr_63_32_start[0], addr_31_0_start[0]} & acd_slv_awaddr<{addr_63_32_end[0], addr_31_0_end[0]}) ? did[0] :
                                (acd_slv_awaddr>={addr_63_32_start[1], addr_31_0_start[1]} & acd_slv_awaddr<{addr_63_32_end[1], addr_31_0_end[1]}) ? did[1] :
                                (acd_slv_awaddr>={addr_63_32_start[2], addr_31_0_start[2]} & acd_slv_awaddr<{addr_63_32_end[2], addr_31_0_end[2]}) ? did[2] :
                                (acd_slv_awaddr>={addr_63_32_start[3], addr_31_0_start[3]} & acd_slv_awaddr<{addr_63_32_end[3], addr_31_0_end[3]}) ? did[3] :
                                (acd_slv_awaddr>={addr_63_32_start[4], addr_31_0_start[4]} & acd_slv_awaddr<{addr_63_32_end[4], addr_31_0_end[4]}) ? did[4] :
                                (acd_slv_awaddr>={addr_63_32_start[5], addr_31_0_start[5]} & acd_slv_awaddr<{addr_63_32_end[5], addr_31_0_end[5]}) ? did[5] :
                                (acd_slv_awaddr>={addr_63_32_start[6], addr_31_0_start[6]} & acd_slv_awaddr<{addr_63_32_end[6], addr_31_0_end[6]}) ? did[6] :
                                (acd_slv_awaddr>={addr_63_32_start[7], addr_31_0_start[7]} & acd_slv_awaddr<{addr_63_32_end[7], addr_31_0_end[7]}) ? did[7] : src_slv_awuser[31:8];

    assign       aw_mmio_pid =  (acd_slv_awaddr>={addr_63_32_start[0], addr_31_0_start[0]} & acd_slv_awaddr<{addr_63_32_end[0], addr_31_0_end[0]}) ? pid[0] :
                                (acd_slv_awaddr>={addr_63_32_start[1], addr_31_0_start[1]} & acd_slv_awaddr<{addr_63_32_end[1], addr_31_0_end[1]}) ? pid[1] :
                                (acd_slv_awaddr>={addr_63_32_start[2], addr_31_0_start[2]} & acd_slv_awaddr<{addr_63_32_end[2], addr_31_0_end[2]}) ? pid[2] :
                                (acd_slv_awaddr>={addr_63_32_start[3], addr_31_0_start[3]} & acd_slv_awaddr<{addr_63_32_end[3], addr_31_0_end[3]}) ? pid[3] :
                                (acd_slv_awaddr>={addr_63_32_start[4], addr_31_0_start[4]} & acd_slv_awaddr<{addr_63_32_end[4], addr_31_0_end[4]}) ? pid[4] :
                                (acd_slv_awaddr>={addr_63_32_start[5], addr_31_0_start[5]} & acd_slv_awaddr<{addr_63_32_end[5], addr_31_0_end[5]}) ? pid[5] :
                                (acd_slv_awaddr>={addr_63_32_start[6], addr_31_0_start[6]} & acd_slv_awaddr<{addr_63_32_end[6], addr_31_0_end[6]}) ? pid[6] :
                                (acd_slv_awaddr>={addr_63_32_start[7], addr_31_0_start[7]} & acd_slv_awaddr<{addr_63_32_end[7], addr_31_0_end[7]}) ? pid[7] : src_slv_awuser[51:32];

    assign       aw_mmio_pid_v =(acd_slv_awaddr>={addr_63_32_start[0], addr_31_0_start[0]} & acd_slv_awaddr<{addr_63_32_end[0], addr_31_0_end[0]}) ? pid_v[0] :
                                (acd_slv_awaddr>={addr_63_32_start[1], addr_31_0_start[1]} & acd_slv_awaddr<{addr_63_32_end[1], addr_31_0_end[1]}) ? pid_v[1] :
                                (acd_slv_awaddr>={addr_63_32_start[2], addr_31_0_start[2]} & acd_slv_awaddr<{addr_63_32_end[2], addr_31_0_end[2]}) ? pid_v[2] :
                                (acd_slv_awaddr>={addr_63_32_start[3], addr_31_0_start[3]} & acd_slv_awaddr<{addr_63_32_end[3], addr_31_0_end[3]}) ? pid_v[3] :
                                (acd_slv_awaddr>={addr_63_32_start[4], addr_31_0_start[4]} & acd_slv_awaddr<{addr_63_32_end[4], addr_31_0_end[4]}) ? pid_v[4] :
                                (acd_slv_awaddr>={addr_63_32_start[5], addr_31_0_start[5]} & acd_slv_awaddr<{addr_63_32_end[5], addr_31_0_end[5]}) ? pid_v[5] :
                                (acd_slv_awaddr>={addr_63_32_start[6], addr_31_0_start[6]} & acd_slv_awaddr<{addr_63_32_end[6], addr_31_0_end[6]}) ? pid_v[6] :
                                (acd_slv_awaddr>={addr_63_32_start[7], addr_31_0_start[7]} & acd_slv_awaddr<{addr_63_32_end[7], addr_31_0_end[7]}) ? pid_v[7] : src_slv_awuser[52];

    assign      aw_mmio_transed=(acd_slv_awaddr>={addr_63_32_start[0], addr_31_0_start[0]} & acd_slv_awaddr<{addr_63_32_end[0], addr_31_0_end[0]}) ? transed[0] :
                                (acd_slv_awaddr>={addr_63_32_start[1], addr_31_0_start[1]} & acd_slv_awaddr<{addr_63_32_end[1], addr_31_0_end[1]}) ? transed[1] :
                                (acd_slv_awaddr>={addr_63_32_start[2], addr_31_0_start[2]} & acd_slv_awaddr<{addr_63_32_end[2], addr_31_0_end[2]}) ? transed[2] :
                                (acd_slv_awaddr>={addr_63_32_start[3], addr_31_0_start[3]} & acd_slv_awaddr<{addr_63_32_end[3], addr_31_0_end[3]}) ? transed[3] :
                                (acd_slv_awaddr>={addr_63_32_start[4], addr_31_0_start[4]} & acd_slv_awaddr<{addr_63_32_end[4], addr_31_0_end[4]}) ? transed[4] :
                                (acd_slv_awaddr>={addr_63_32_start[5], addr_31_0_start[5]} & acd_slv_awaddr<{addr_63_32_end[5], addr_31_0_end[5]}) ? transed[5] :
                                (acd_slv_awaddr>={addr_63_32_start[6], addr_31_0_start[6]} & acd_slv_awaddr<{addr_63_32_end[6], addr_31_0_end[6]}) ? transed[6] :
                                (acd_slv_awaddr>={addr_63_32_start[7], addr_31_0_start[7]} & acd_slv_awaddr<{addr_63_32_end[7], addr_31_0_end[7]}) ? transed[7] : src_slv_awuser[53];


    assign       ar_mmio_did =  (acd_slv_araddr>={addr_63_32_start[0], addr_31_0_start[0]} & acd_slv_araddr<{addr_63_32_end[0], addr_31_0_end[0]}) ? did[0] :
                                (acd_slv_araddr>={addr_63_32_start[1], addr_31_0_start[1]} & acd_slv_araddr<{addr_63_32_end[1], addr_31_0_end[1]}) ? did[1] :
                                (acd_slv_araddr>={addr_63_32_start[2], addr_31_0_start[2]} & acd_slv_araddr<{addr_63_32_end[2], addr_31_0_end[2]}) ? did[2] :
                                (acd_slv_araddr>={addr_63_32_start[3], addr_31_0_start[3]} & acd_slv_araddr<{addr_63_32_end[3], addr_31_0_end[3]}) ? did[3] :
                                (acd_slv_araddr>={addr_63_32_start[4], addr_31_0_start[4]} & acd_slv_araddr<{addr_63_32_end[4], addr_31_0_end[4]}) ? did[4] :
                                (acd_slv_araddr>={addr_63_32_start[5], addr_31_0_start[5]} & acd_slv_araddr<{addr_63_32_end[5], addr_31_0_end[5]}) ? did[5] :
                                (acd_slv_araddr>={addr_63_32_start[6], addr_31_0_start[6]} & acd_slv_araddr<{addr_63_32_end[6], addr_31_0_end[6]}) ? did[6] :
                                (acd_slv_araddr>={addr_63_32_start[7], addr_31_0_start[7]} & acd_slv_araddr<{addr_63_32_end[7], addr_31_0_end[7]}) ? did[7] : src_slv_aruser[31:8];

    assign       ar_mmio_pid =  (acd_slv_araddr>={addr_63_32_start[0], addr_31_0_start[0]} & acd_slv_araddr<{addr_63_32_end[0], addr_31_0_end[0]}) ? pid[0] :
                                (acd_slv_araddr>={addr_63_32_start[1], addr_31_0_start[1]} & acd_slv_araddr<{addr_63_32_end[1], addr_31_0_end[1]}) ? pid[1] :
                                (acd_slv_araddr>={addr_63_32_start[2], addr_31_0_start[2]} & acd_slv_araddr<{addr_63_32_end[2], addr_31_0_end[2]}) ? pid[2] :
                                (acd_slv_araddr>={addr_63_32_start[3], addr_31_0_start[3]} & acd_slv_araddr<{addr_63_32_end[3], addr_31_0_end[3]}) ? pid[3] :
                                (acd_slv_araddr>={addr_63_32_start[4], addr_31_0_start[4]} & acd_slv_araddr<{addr_63_32_end[4], addr_31_0_end[4]}) ? pid[4] :
                                (acd_slv_araddr>={addr_63_32_start[5], addr_31_0_start[5]} & acd_slv_araddr<{addr_63_32_end[5], addr_31_0_end[5]}) ? pid[5] :
                                (acd_slv_araddr>={addr_63_32_start[6], addr_31_0_start[6]} & acd_slv_araddr<{addr_63_32_end[6], addr_31_0_end[6]}) ? pid[6] :
                                (acd_slv_araddr>={addr_63_32_start[7], addr_31_0_start[7]} & acd_slv_araddr<{addr_63_32_end[7], addr_31_0_end[7]}) ? pid[7] : src_slv_aruser[51:32];

    assign       ar_mmio_pid_v =(acd_slv_araddr>={addr_63_32_start[0], addr_31_0_start[0]} & acd_slv_araddr<{addr_63_32_end[0], addr_31_0_end[0]}) ? pid_v[0] :
                                (acd_slv_araddr>={addr_63_32_start[1], addr_31_0_start[1]} & acd_slv_araddr<{addr_63_32_end[1], addr_31_0_end[1]}) ? pid_v[1] :
                                (acd_slv_araddr>={addr_63_32_start[2], addr_31_0_start[2]} & acd_slv_araddr<{addr_63_32_end[2], addr_31_0_end[2]}) ? pid_v[2] :
                                (acd_slv_araddr>={addr_63_32_start[3], addr_31_0_start[3]} & acd_slv_araddr<{addr_63_32_end[3], addr_31_0_end[3]}) ? pid_v[3] :
                                (acd_slv_araddr>={addr_63_32_start[4], addr_31_0_start[4]} & acd_slv_araddr<{addr_63_32_end[4], addr_31_0_end[4]}) ? pid_v[4] :
                                (acd_slv_araddr>={addr_63_32_start[5], addr_31_0_start[5]} & acd_slv_araddr<{addr_63_32_end[5], addr_31_0_end[5]}) ? pid_v[5] :
                                (acd_slv_araddr>={addr_63_32_start[6], addr_31_0_start[6]} & acd_slv_araddr<{addr_63_32_end[6], addr_31_0_end[6]}) ? pid_v[6] :
                                (acd_slv_araddr>={addr_63_32_start[7], addr_31_0_start[7]} & acd_slv_araddr<{addr_63_32_end[7], addr_31_0_end[7]}) ? pid_v[7] : src_slv_aruser[52];

    assign      ar_mmio_transed=(acd_slv_araddr>={addr_63_32_start[0], addr_31_0_start[0]} & acd_slv_araddr<{addr_63_32_end[0], addr_31_0_end[0]}) ? transed[0] :
                                (acd_slv_araddr>={addr_63_32_start[1], addr_31_0_start[1]} & acd_slv_araddr<{addr_63_32_end[1], addr_31_0_end[1]}) ? transed[1] :
                                (acd_slv_araddr>={addr_63_32_start[2], addr_31_0_start[2]} & acd_slv_araddr<{addr_63_32_end[2], addr_31_0_end[2]}) ? transed[2] :
                                (acd_slv_araddr>={addr_63_32_start[3], addr_31_0_start[3]} & acd_slv_araddr<{addr_63_32_end[3], addr_31_0_end[3]}) ? transed[3] :
                                (acd_slv_araddr>={addr_63_32_start[4], addr_31_0_start[4]} & acd_slv_araddr<{addr_63_32_end[4], addr_31_0_end[4]}) ? transed[4] :
                                (acd_slv_araddr>={addr_63_32_start[5], addr_31_0_start[5]} & acd_slv_araddr<{addr_63_32_end[5], addr_31_0_end[5]}) ? transed[5] :
                                (acd_slv_araddr>={addr_63_32_start[6], addr_31_0_start[6]} & acd_slv_araddr<{addr_63_32_end[6], addr_31_0_end[6]}) ? transed[6] :
                                (acd_slv_araddr>={addr_63_32_start[7], addr_31_0_start[7]} & acd_slv_araddr<{addr_63_32_end[7], addr_31_0_end[7]}) ? transed[7] : src_slv_aruser[53];


    assign acd_slv_awid                 = src_slv_awid        ;
    assign acd_slv_awaddr               = src_slv_awaddr      ;
    assign acd_slv_awlen                = src_slv_awlen       ;
    assign acd_slv_awsize               = src_slv_awsize      ;
    assign acd_slv_awburst              = src_slv_awburst     ;
    assign acd_slv_awlock               = src_slv_awlock      ;
    assign acd_slv_awcache              = src_slv_awcache     ;
    assign acd_slv_awprot               = src_slv_awprot      ;
    assign acd_slv_awregion             = src_slv_awregion    ;
    assign acd_slv_awuser               = mmio_did_pid_en ? {aw_mmio_transed, aw_mmio_pid_v, aw_mmio_pid, aw_mmio_did, src_slv_awuser[7:0]} : src_slv_awuser      ;
    assign acd_slv_awvalid              = src_slv_awvalid     & ~hw_bypass_en;
    assign acd_slv_arid                 = src_slv_arid        ;
    assign acd_slv_araddr               = src_slv_araddr      ;
    assign acd_slv_arlen                = src_slv_arlen       ;
    assign acd_slv_arsize               = src_slv_arsize      ;
    assign acd_slv_arburst              = src_slv_arburst     ;
    assign acd_slv_arlock               = src_slv_arlock      ;
    assign acd_slv_arcache              = src_slv_arcache     ;
    assign acd_slv_arprot               = src_slv_arprot      ;
    assign acd_slv_arregion             = src_slv_arregion    ;
    assign acd_slv_aruser               = mmio_did_pid_en ? {ar_mmio_transed, ar_mmio_pid_v, ar_mmio_pid, ar_mmio_did, src_slv_aruser[7:0]} : src_slv_aruser      ;
    assign acd_slv_arvalid              = src_slv_arvalid     & ~hw_bypass_en;
    assign acd_slv_wdata                = src_slv_wdata       ;
    assign acd_slv_wstrb                = src_slv_wstrb       ;
    assign acd_slv_wlast                = src_slv_wlast       ;
    assign acd_slv_wvalid               = src_slv_wvalid      & ~hw_bypass_en;
    assign acd_slv_wuser                = src_slv_wuser       ;
    assign acd_slv_rready               = src_slv_rready      & ~hw_bypass_en;
    assign acd_slv_bready               = src_slv_bready      & ~hw_bypass_en;
    assign acd_slv_awqos                = src_slv_awqos       ;
    assign acd_slv_arqos                = src_slv_arqos       ;
//    assign acd_slv_awready              = src_slv_awready     ;
//    assign acd_slv_arready              = src_slv_arready     ;
//    assign acd_slv_wready               = src_slv_wready      ;
//    assign acd_slv_rid                  = src_slv_rid         ;
//    assign acd_slv_rdata                = src_slv_rdata       ;
//    assign acd_slv_rresp                = src_slv_rresp       ;
//    assign acd_slv_rlast                = src_slv_rlast       ;
//    assign acd_slv_rvalid               = src_slv_rvalid      & ~hw_bypass_en;
//    assign acd_slv_ruser                = src_slv_ruser       ;
//    assign acd_slv_bid                  = src_slv_bid         ;
//    assign acd_slv_bvalid               = src_slv_bvalid      & ~hw_bypass_en;
//    assign acd_slv_bresp                = src_slv_bresp       ;
//    assign acd_slv_buser                = src_slv_buser       ;

    assign src_slv_awready            = hw_bypass_en ? bypass_slv_awready   : acd_slv_awready   ;
    assign src_slv_arready            = hw_bypass_en ? bypass_slv_arready   : acd_slv_arready   ;
    assign src_slv_wready             = hw_bypass_en ? bypass_slv_wready    : acd_slv_wready    ;
    assign src_slv_rid                = hw_bypass_en ? bypass_slv_rid       : acd_slv_rid       ;
    assign src_slv_rdata              = hw_bypass_en ? bypass_slv_rdata     : acd_slv_rdata     ;
    assign src_slv_rresp              = hw_bypass_en ? bypass_slv_rresp     : acd_slv_rresp     ;
    assign src_slv_rlast              = hw_bypass_en ? bypass_slv_rlast     : acd_slv_rlast     ;
    assign src_slv_rvalid             = hw_bypass_en ? bypass_slv_rvalid    : acd_slv_rvalid    ;
    assign src_slv_ruser              = hw_bypass_en ? bypass_slv_ruser     : acd_slv_ruser     ;
    assign src_slv_bid                = hw_bypass_en ? bypass_slv_bid       : acd_slv_bid       ;
    assign src_slv_bvalid             = hw_bypass_en ? bypass_slv_bvalid    : acd_slv_bvalid    ;
    assign src_slv_bresp              = hw_bypass_en ? bypass_slv_bresp     : acd_slv_bresp     ;
    assign src_slv_buser              = hw_bypass_en ? bypass_slv_buser     : acd_slv_buser     ;

    logic [9:0]             acd_mst_awid      ;
    logic [39:0]            acd_mst_awaddr    ;
    logic [7:0]             acd_mst_awlen     ;
    logic [2:0]             acd_mst_awsize    ;
    logic [1:0]             acd_mst_awburst   ;
    logic                   acd_mst_awlock    ;
    logic [3:0]             acd_mst_awcache   ;
    logic [2:0]             acd_mst_awprot    ;
    logic [3:0]             acd_mst_awregion  ;
    logic [7:0]             acd_mst_awuser    ;
    logic [3:0]             acd_mst_awqos     ;
    logic                   acd_mst_awvalid   ;
    logic [9:0]             acd_mst_arid      ;
    logic [39:0]            acd_mst_araddr    ;
    logic [7:0]             acd_mst_arlen     ;
    logic [2:0]             acd_mst_arsize    ;
    logic [1:0]             acd_mst_arburst   ;
    logic                   acd_mst_arlock    ;
    logic [3:0]             acd_mst_arcache   ;
    logic [2:0]             acd_mst_arprot    ;
    logic [3:0]             acd_mst_arregion  ;
    logic [7:0]             acd_mst_aruser    ;
    logic [3:0]             acd_mst_arqos     ;
    logic                   acd_mst_arvalid   ;
    logic [255:0]           acd_mst_wdata     ;
    logic [31:0]            acd_mst_wstrb     ;
    logic                   acd_mst_wlast     ;
    logic                   acd_mst_wvalid    ;
    logic [7:0]             acd_mst_wuser     ;
    logic                   acd_mst_rready    ;
    logic                   acd_mst_bready    ;
    logic                   acd_mst_awready   ;
    logic                   acd_mst_arready   ;
    logic                   acd_mst_wready    ;
    logic [9:0]             acd_mst_rid       ;
    logic [255:0]           acd_mst_rdata     ;
    logic [1:0]             acd_mst_rresp     ;
    logic                   acd_mst_rlast     ;
    logic                   acd_mst_rvalid    ;
    logic [7:0]             acd_mst_ruser     ;
    logic [9:0]             acd_mst_bid       ;
    logic                   acd_mst_bvalid    ;
    logic [1:0]             acd_mst_bresp     ;
    logic [7:0]             acd_mst_buser     ;

    assign acd_mst_araddr = iommu_mst_araddr_mux[39:0];
    assign acd_mst_awaddr = iommu_mst_awaddr_mux[39:0];

    assign iommu_mst_awid       = hw_bypass_en ? bypass_slv_awid       : acd_mst_awid       ;
    assign iommu_mst_awaddr     = hw_bypass_en ? bypass_slv_awaddr     : acd_mst_awaddr     ;
    assign iommu_mst_awlen      = hw_bypass_en ? bypass_slv_awlen      : acd_mst_awlen      ;
    assign iommu_mst_awsize     = hw_bypass_en ? bypass_slv_awsize     : acd_mst_awsize     ;
    assign iommu_mst_awburst    = hw_bypass_en ? bypass_slv_awburst    : acd_mst_awburst    ;
    assign iommu_mst_awlock     = hw_bypass_en ? bypass_slv_awlock     : acd_mst_awlock     ;
    assign iommu_mst_awcache    = hw_bypass_en ? bypass_slv_awcache    : acd_mst_awcache    ;
    assign iommu_mst_awprot     = hw_bypass_en ? bypass_slv_awprot     : acd_mst_awprot     ;
    assign iommu_mst_awregion   = hw_bypass_en ? bypass_slv_awregion   : acd_mst_awregion   ;
    assign iommu_mst_awuser     = hw_bypass_en ? bypass_slv_awuser     : acd_mst_awuser     ;
    assign iommu_mst_awqos      = hw_bypass_en ? bypass_slv_awqos      : acd_mst_awqos      ;
    assign iommu_mst_awvalid    = hw_bypass_en ? bypass_slv_awvalid    : acd_mst_awvalid    ;
    assign iommu_mst_arid       = hw_bypass_en ? bypass_slv_arid       : acd_mst_arid       ;
    assign iommu_mst_araddr     = hw_bypass_en ? bypass_slv_araddr     : acd_mst_araddr     ;
    assign iommu_mst_arlen      = hw_bypass_en ? bypass_slv_arlen      : acd_mst_arlen      ;
    assign iommu_mst_arsize     = hw_bypass_en ? bypass_slv_arsize     : acd_mst_arsize     ;
    assign iommu_mst_arburst    = hw_bypass_en ? bypass_slv_arburst    : acd_mst_arburst    ;
    assign iommu_mst_arlock     = hw_bypass_en ? bypass_slv_arlock     : acd_mst_arlock     ;
    assign iommu_mst_arcache    = hw_bypass_en ? bypass_slv_arcache    : acd_mst_arcache    ;
    assign iommu_mst_arprot     = hw_bypass_en ? bypass_slv_arprot     : acd_mst_arprot     ;
    assign iommu_mst_arregion   = hw_bypass_en ? bypass_slv_arregion   : acd_mst_arregion   ;
    assign iommu_mst_aruser     = hw_bypass_en ? bypass_slv_aruser     : acd_mst_aruser     ;
    assign iommu_mst_arqos      = hw_bypass_en ? bypass_slv_arqos      : acd_mst_arqos      ;
    assign iommu_mst_arvalid    = hw_bypass_en ? bypass_slv_arvalid    : acd_mst_arvalid    ;
    assign iommu_mst_wdata      = hw_bypass_en ? bypass_slv_wdata      : acd_mst_wdata      ;
    assign iommu_mst_wstrb      = hw_bypass_en ? bypass_slv_wstrb      : acd_mst_wstrb      ;
    assign iommu_mst_wlast      = hw_bypass_en ? bypass_slv_wlast      : acd_mst_wlast      ;
    assign iommu_mst_wvalid     = hw_bypass_en ? bypass_slv_wvalid     : acd_mst_wvalid     ;
    assign iommu_mst_wuser      = hw_bypass_en ? bypass_slv_wuser      : acd_mst_wuser      ;
    assign iommu_mst_rready     = hw_bypass_en ? bypass_slv_rready     : acd_mst_rready     ;
    assign iommu_mst_bready     = hw_bypass_en ? bypass_slv_bready     : acd_mst_bready     ;
//    assign iommu_mst_awready    = hw_bypass_en ? bypass_slv_awready    : acd_mst_awready    ;
//    assign iommu_mst_arready    = hw_bypass_en ? bypass_slv_arready    : acd_mst_arready    ;
//    assign iommu_mst_wready     = hw_bypass_en ? bypass_slv_wready     : acd_mst_wready     ;
//    assign iommu_mst_rid        = hw_bypass_en ? bypass_slv_rid        : acd_mst_rid        ;
//    assign iommu_mst_rdata      = hw_bypass_en ? bypass_slv_rdata      : acd_mst_rdata      ;
//    assign iommu_mst_rresp      = hw_bypass_en ? bypass_slv_rresp      : acd_mst_rresp      ;
//    assign iommu_mst_rlast      = hw_bypass_en ? bypass_slv_rlast      : acd_mst_rlast      ;
//    assign iommu_mst_rvalid     = hw_bypass_en ? bypass_slv_rvalid     : acd_mst_rvalid     ;
//    assign iommu_mst_ruser      = hw_bypass_en ? bypass_slv_ruser      : acd_mst_ruser      ;
//    assign iommu_mst_bid        = hw_bypass_en ? bypass_slv_bid        : acd_mst_bid        ;
//    assign iommu_mst_bvalid     = hw_bypass_en ? bypass_slv_bvalid     : acd_mst_bvalid     ;
//    assign iommu_mst_bresp      = hw_bypass_en ? bypass_slv_bresp      : acd_mst_bresp      ;
//    assign iommu_mst_buser      = hw_bypass_en ? bypass_slv_buser      : acd_mst_buser      ;

    assign bypass_slv_awready   = iommu_mst_awready & hw_bypass_en;
    assign acd_mst_awready      = iommu_mst_awready & ~hw_bypass_en;
    assign bypass_slv_arready   = iommu_mst_arready & hw_bypass_en;
    assign acd_mst_arready      = iommu_mst_arready & ~hw_bypass_en;
    assign bypass_slv_wready    = iommu_mst_wready  & hw_bypass_en;
    assign acd_mst_wready       = iommu_mst_wready  & ~hw_bypass_en;

    assign bypass_slv_rid       = iommu_mst_rid    ;
    assign bypass_slv_rdata     = iommu_mst_rdata  ;
    assign bypass_slv_rresp     = iommu_mst_rresp  ;
    assign bypass_slv_rlast     = iommu_mst_rlast  ;
    assign bypass_slv_rvalid    = iommu_mst_rvalid  & hw_bypass_en;
    assign bypass_slv_ruser     = iommu_mst_ruser  ;
    assign bypass_slv_bid       = iommu_mst_bid    ;
    assign bypass_slv_bvalid    = iommu_mst_bvalid  & hw_bypass_en;
    assign bypass_slv_bresp     = iommu_mst_bresp  ;
    assign bypass_slv_buser     = iommu_mst_buser  ;

    assign acd_mst_rid          = iommu_mst_rid    ;
    assign acd_mst_rdata        = iommu_mst_rdata  ;
    assign acd_mst_rresp        = iommu_mst_rresp  ;
    assign acd_mst_rlast        = iommu_mst_rlast  ;
    assign acd_mst_rvalid       = iommu_mst_rvalid  & ~hw_bypass_en;
    assign acd_mst_ruser        = iommu_mst_ruser  ;
    assign acd_mst_bid          = iommu_mst_bid    ;
    assign acd_mst_bvalid       = iommu_mst_bvalid  & ~hw_bypass_en;
    assign acd_mst_bresp        = iommu_mst_bresp  ;
    assign acd_mst_buser        = iommu_mst_buser  ;

//}}}

//=== MONITOR CONN {{{
/*    output [31:0] */         assign MONITOR_AXI_0_araddr    = monitor_src_sel ? iommu_mst_araddr [31:0] : src_slv_araddr [31:0];
/*    output [1 :0] */         assign MONITOR_AXI_0_arburst   = monitor_src_sel ? iommu_mst_arburst[1 :0] : src_slv_arburst[1 :0];
/*    output [3 :0] */         assign MONITOR_AXI_0_arcache   = monitor_src_sel ? iommu_mst_arcache[3 :0] : src_slv_arcache[3 :0];
/*    output [0 :0] */         assign MONITOR_AXI_0_arid      = monitor_src_sel ? iommu_mst_arid          : src_slv_arid   ;
/*    output [7 :0] */         assign MONITOR_AXI_0_arlen     = monitor_src_sel ? iommu_mst_arlen  [7 :0] : src_slv_arlen  [7 :0];
/*    output [0 :0] */         assign MONITOR_AXI_0_arlock    = monitor_src_sel ? iommu_mst_arlock        : src_slv_arlock ;
/*    output [2 :0] */         assign MONITOR_AXI_0_arprot    = monitor_src_sel ? iommu_mst_arprot [2 :0] : src_slv_arprot [2 :0];
/*    output [0 :0] */         assign MONITOR_AXI_0_arready   = monitor_src_sel ? iommu_mst_arready       : src_slv_arready;
/*    output [2 :0] */         assign MONITOR_AXI_0_arsize    = monitor_src_sel ? iommu_mst_arsize [2 :0] : src_slv_arsize [2 :0];
/*    output [0 :0] */         assign MONITOR_AXI_0_arvalid   = monitor_src_sel ? iommu_mst_arvalid       : src_slv_arvalid;
/*    output [31:0] */         assign MONITOR_AXI_0_awaddr    = monitor_src_sel ? iommu_mst_awaddr [31:0] : src_slv_awaddr [31:0];
/*    output [1 :0] */         assign MONITOR_AXI_0_awburst   = monitor_src_sel ? iommu_mst_awburst[1 :0] : src_slv_awburst[1 :0];
/*    output [3 :0] */         assign MONITOR_AXI_0_awcache   = monitor_src_sel ? iommu_mst_awcache[3 :0] : src_slv_awcache[3 :0];
/*    output [0 :0] */         assign MONITOR_AXI_0_awid      = monitor_src_sel ? iommu_mst_awid          : src_slv_awid   ;
/*    output [7 :0] */         assign MONITOR_AXI_0_awlen     = monitor_src_sel ? iommu_mst_awlen  [7 :0] : src_slv_awlen  [7 :0];
/*    output [0 :0] */         assign MONITOR_AXI_0_awlock    = monitor_src_sel ? iommu_mst_awlock        : src_slv_awlock ;
/*    output [2 :0] */         assign MONITOR_AXI_0_awprot    = monitor_src_sel ? iommu_mst_awprot [2 :0] : src_slv_awprot [2 :0];
/*    output [0 :0] */         assign MONITOR_AXI_0_awready   = monitor_src_sel ? iommu_mst_awready       : src_slv_awready;
/*    output [2 :0] */         assign MONITOR_AXI_0_awsize    = monitor_src_sel ? iommu_mst_awsize [2 :0] : src_slv_awsize [2 :0];
/*    output [0 :0] */         assign MONITOR_AXI_0_awvalid   = monitor_src_sel ? iommu_mst_awvalid       : src_slv_awvalid;
/*    output [0 :0] */         assign MONITOR_AXI_0_bid       = monitor_src_sel ? iommu_mst_bid           : src_slv_bid    ;
/*    output [0 :0] */         assign MONITOR_AXI_0_bready    = monitor_src_sel ? iommu_mst_bready        : src_slv_bready ;
/*    output [1 :0] */         assign MONITOR_AXI_0_bresp     = monitor_src_sel ? iommu_mst_bresp  [1 :0] : src_slv_bresp  [1 :0];
/*    output [0 :0] */         assign MONITOR_AXI_0_bvalid    = monitor_src_sel ? iommu_mst_bvalid        : src_slv_bvalid ;
/*    output [31:0] */         assign MONITOR_AXI_0_rdata     = monitor_src_sel ? iommu_mst_rdata  [31:0] : src_slv_rdata  [31:0];
/*    output [0 :0] */         assign MONITOR_AXI_0_rid       = monitor_src_sel ? iommu_mst_rid           : src_slv_rid    ;
/*    output [0 :0] */         assign MONITOR_AXI_0_rlast     = monitor_src_sel ? iommu_mst_rlast         : src_slv_rlast  ;
/*    output [0 :0] */         assign MONITOR_AXI_0_rready    = monitor_src_sel ? iommu_mst_rready        : src_slv_rready ;
/*    output [1 :0] */         assign MONITOR_AXI_0_rresp     = monitor_src_sel ? iommu_mst_rresp  [1 :0] : src_slv_rresp  [1 :0];
/*    output [0 :0] */         assign MONITOR_AXI_0_rvalid    = monitor_src_sel ? iommu_mst_rvalid        : src_slv_rvalid ;
/*    output [31:0] */         assign MONITOR_AXI_0_wdata     = monitor_src_sel ? iommu_mst_wdata  [31:0] : src_slv_wdata  [31:0];
/*    output [0 :0] */         assign MONITOR_AXI_0_wlast     = monitor_src_sel ? iommu_mst_wlast         : src_slv_wlast  ;
/*    output [0 :0] */         assign MONITOR_AXI_0_wready    = monitor_src_sel ? iommu_mst_wready        : src_slv_wready ;
/*    output [3 :0] */         assign MONITOR_AXI_0_wstrb     = monitor_src_sel ? iommu_mst_wstrb  [3 :0] : src_slv_wstrb  [3 :0];
/*    output [0 :0] */         assign MONITOR_AXI_0_wvalid    = monitor_src_sel ? iommu_mst_wvalid        : src_slv_wvalid ;

    iommu_axi_mon #(
    /*parameter int unsigned */ .BUS_ADDR_WIDTH             (40), // = 64,
    /*parameter int unsigned */ .BUS_DATA_WIDTH             (256), // = 128,
    /*parameter int unsigned */ .BUS_SIZE_WIDTH             (3), // = 3,
    /*parameter int unsigned */ .BUS_ID_WIDTH               (10), // = 8,
    /*parameter int unsigned */ .BUS_USER_WIDTH             (8), // = 8,
    /*parameter int unsigned */ .AWRAM_IDX_WIDTH            (6), // = 7,
    /*parameter int unsigned */ .ARRAM_IDX_WIDTH            (6), // = 7,
    /*parameter int unsigned */ .WRAM_IDX_WIDTH             (8), // = 7,
    /*parameter int unsigned */ .BRAM_IDX_WIDTH             (6), // = 7,
    /*parameter int unsigned */ .RRAM_IDX_WIDTH             (8), // = 7,
    /*parameter int unsigned */ .SPARE_PARAM                (0)  // = 0
    ) U_axi_mon(
    /*input logic                                       */  .clk                        (iommu_clk                      ),
    /*input logic                                       */  .rstn                       (iommu_rstn                     ),
    /*input  logic [BUS_ID_WIDTH-1:0]                   */  .slv_awid_i                 (src_slv_awid                   ),
    /*input  logic [BUS_ADDR_WIDTH-1:0]                 */  .slv_awaddr_i               (src_slv_awaddr                 ),
    /*input  logic [ 7:0]                               */  .slv_awlen_i                (src_slv_awlen                  ),
    /*input  logic [BUS_SIZE_WIDTH-1:0]                 */  .slv_awsize_i               (src_slv_awsize                 ),
    /*input  logic [ 1:0]                               */  .slv_awburst_i              (src_slv_awburst                ),
    /*input  logic                                      */  .slv_awlock_i               (src_slv_awlock                 ),
    /*input  logic [ 3:0]                               */  .slv_awcache_i              (src_slv_awcache                ),
    /*input  logic [ 2:0]                               */  .slv_awprot_i               (src_slv_awprot                 ),
    /*input  logic [ 3:0]                               */  .slv_awregion_i             (src_slv_awregion               ),
    /*input  logic [BUS_USER_WIDTH-1:0]                 */  .slv_awuser_i               (src_slv_awuser                 ),
    /*input  logic [ 3:0]                               */  .slv_awqos_i                (src_slv_awqos                  ),
    /*input  logic                                      */  .slv_awvalid_i              (src_slv_awvalid                ),
    /*input  logic                                      */  .slv_awready_i              (src_slv_awready                ),
    /*input  logic [23:0]                               */  .slv_aw_device_id_i         (acd_slv_awuser[31:8]   ),
    /*input  logic [19:0]                               */  .slv_aw_process_id_i        (acd_slv_awuser[51:32]  ),
    /*input  logic                                      */  .slv_aw_process_id_valid_i  (acd_slv_awuser[52]     ),
    /*input  logic                                      */  .slv_aw_is_translated_i     (acd_slv_awuser[53]     ),
    /*input  logic [BUS_DATA_WIDTH-1:0]                 */  .slv_wdata_i                (src_slv_wdata                  ),
    /*input  logic [BUS_STRB_WIDTH-1:0]                 */  .slv_wstrb_i                (src_slv_wstrb                  ),
    /*input  logic                                      */  .slv_wlast_i                (src_slv_wlast                  ),
    /*input  logic [BUS_USER_WIDTH-1:0]                 */  .slv_wuser_i                (src_slv_wuser                  ),
    /*input  logic                                      */  .slv_wvalid_i               (src_slv_wvalid                 ),
    /*input  logic                                      */  .slv_wready_i               (src_slv_wready                 ),
    /*input  logic [BUS_ID_WIDTH-1:0]                   */  .slv_bid_i                  (src_slv_bid                    ),
    /*input  logic [ 1:0]                               */  .slv_bresp_i                (src_slv_bresp                  ),
    /*input  logic                                      */  .slv_bvalid_i               (src_slv_bvalid                 ),
    /*input  logic [BUS_USER_WIDTH-1:0]                 */  .slv_buser_i                (src_slv_buser                  ),
    /*input  logic                                      */  .slv_bready_i               (src_slv_bready                 ),
    /*input  logic [BUS_ID_WIDTH-1:0]                   */  .slv_arid_i                 (src_slv_arid                   ),
    /*input  logic [BUS_ADDR_WIDTH-1:0]                 */  .slv_araddr_i               (src_slv_araddr                 ),
    /*input  logic [ 7:0]                               */  .slv_arlen_i                (src_slv_arlen                  ),
    /*input  logic [BUS_SIZE_WIDTH-1:0]                 */  .slv_arsize_i               (src_slv_arsize                 ),
    /*input  logic [ 1:0]                               */  .slv_arburst_i              (src_slv_arburst                ),
    /*input  logic                                      */  .slv_arlock_i               (src_slv_arlock                 ),
    /*input  logic [ 3:0]                               */  .slv_arcache_i              (src_slv_arcache                ),
    /*input  logic [ 2:0]                               */  .slv_arprot_i               (src_slv_arprot                 ),
    /*input  logic [ 3:0]                               */  .slv_arregion_i             (src_slv_arregion               ),
    /*input  logic [ BUS_USER_WIDTH-1:0]                */  .slv_aruser_i               (src_slv_aruser                 ),
    /*input  logic [ 3:0]                               */  .slv_arqos_i                (src_slv_arqos                  ),
    /*input  logic                                      */  .slv_arvalid_i              (src_slv_arvalid                ),
    /*input  logic                                      */  .slv_arready_i              (src_slv_arready                ),
    /*input  logic [23:0]                               */  .slv_ar_device_id_i         (acd_slv_aruser[31:8]   ),
    /*input  logic [19:0]                               */  .slv_ar_process_id_i        (acd_slv_aruser[51:32]  ),
    /*input  logic                                      */  .slv_ar_process_id_valid_i  (acd_slv_aruser[52]     ),
    /*input  logic                                      */  .slv_ar_is_translated_i     (acd_slv_aruser[53]     ),
    /*input  logic [BUS_ID_WIDTH-1:0]                   */  .slv_rid_i                  (src_slv_rid                    ),
    /*input  logic [BUS_DATA_WIDTH-1:0]                 */  .slv_rdata_i                (src_slv_rdata                  ),
    /*input  logic [ 1:0]                               */  .slv_rresp_i                (src_slv_rresp                  ),
    /*input  logic                                      */  .slv_rlast_i                (src_slv_rlast                  ),
    /*input  logic [BUS_USER_WIDTH-1:0]                 */  .slv_ruser_i                (src_slv_ruser                  ),
    /*input  logic                                      */  .slv_rvalid_i               (src_slv_rvalid                 ),
    /*input  logic                                      */  .slv_rready_i               (src_slv_rready                 ),
    /*input  logic [BUS_ID_WIDTH-1:0]                   */  .mst_awid_i                 (iommu_mst_awid                   ),
    /*input  logic [BUS_ADDR_WIDTH-1:0]                 */  .mst_awaddr_i               (iommu_mst_awaddr                 ),
    /*input  logic [ 7:0]                               */  .mst_awlen_i                (iommu_mst_awlen                  ),
    /*input  logic [BUS_SIZE_WIDTH-1:0]                 */  .mst_awsize_i               (iommu_mst_awsize                 ),
    /*input  logic [ 1:0]                               */  .mst_awburst_i              (iommu_mst_awburst                ),
    /*input  logic                                      */  .mst_awlock_i               (iommu_mst_awlock                 ),
    /*input  logic [ 3:0]                               */  .mst_awcache_i              (iommu_mst_awcache                ),
    /*input  logic [ 2:0]                               */  .mst_awprot_i               (iommu_mst_awprot                 ),
    /*input  logic [ 3:0]                               */  .mst_awregion_i             (iommu_mst_awregion               ),
    /*input  logic [BUS_USER_WIDTH-1:0]                 */  .mst_awuser_i               (iommu_mst_awuser                 ),
    /*input  logic [ 3:0]                               */  .mst_awqos_i                (iommu_mst_awqos                  ),
    /*input  logic                                      */  .mst_awvalid_i              (iommu_mst_awvalid                ),
    /*input  logic                                      */  .mst_awready_i              (iommu_mst_awready                ),
    /*input  logic [BUS_DATA_WIDTH-1:0]                 */  .mst_wdata_i                (iommu_mst_wdata                  ),
    /*input  logic [BUS_STRB_WIDTH-1:0]                 */  .mst_wstrb_i                (iommu_mst_wstrb                  ),
    /*input  logic                                      */  .mst_wlast_i                (iommu_mst_wlast                  ),
    /*input  logic [BUS_USER_WIDTH-1:0]                 */  .mst_wuser_i                (iommu_mst_wuser                  ),
    /*input  logic                                      */  .mst_wvalid_i               (iommu_mst_wvalid                 ),
    /*input  logic                                      */  .mst_wready_i               (iommu_mst_wready                 ),
    /*input  logic [BUS_ID_WIDTH-1:0]                   */  .mst_bid_i                  (iommu_mst_bid                    ),
    /*input  logic [ 1:0]                               */  .mst_bresp_i                (iommu_mst_bresp                  ),
    /*input  logic                                      */  .mst_bvalid_i               (iommu_mst_bvalid                 ),
    /*input  logic [BUS_USER_WIDTH-1:0]                 */  .mst_buser_i                (iommu_mst_buser                  ),
    /*input  logic                                      */  .mst_bready_i               (iommu_mst_bready                 ),
    /*input  logic [BUS_ID_WIDTH-1:0]                   */  .mst_arid_i                 (iommu_mst_arid                   ),
    /*input  logic [BUS_ADDR_WIDTH-1:0]                 */  .mst_araddr_i               (iommu_mst_araddr                 ),
    /*input  logic [ 7:0]                               */  .mst_arlen_i                (iommu_mst_arlen                  ),
    /*input  logic [BUS_SIZE_WIDTH-1:0]                 */  .mst_arsize_i               (iommu_mst_arsize                 ),
    /*input  logic [ 1:0]                               */  .mst_arburst_i              (iommu_mst_arburst                ),
    /*input  logic                                      */  .mst_arlock_i               (iommu_mst_arlock                 ),
    /*input  logic [ 3:0]                               */  .mst_arcache_i              (iommu_mst_arcache                ),
    /*input  logic [ 2:0]                               */  .mst_arprot_i               (iommu_mst_arprot                 ),
    /*input  logic [ 3:0]                               */  .mst_arregion_i             (iommu_mst_arregion               ),
    /*input  logic [ BUS_USER_WIDTH-1:0]                */  .mst_aruser_i               (iommu_mst_aruser                 ),
    /*input  logic [ 3:0]                               */  .mst_arqos_i                (iommu_mst_arqos                  ),
    /*input  logic                                      */  .mst_arvalid_i              (iommu_mst_arvalid                ),
    /*input  logic                                      */  .mst_arready_i              (iommu_mst_arready                ),
    /*input  logic [BUS_ID_WIDTH-1:0]                   */  .mst_rid_i                  (iommu_mst_rid                    ),
    /*input  logic [BUS_DATA_WIDTH-1:0]                 */  .mst_rdata_i                (iommu_mst_rdata                  ),
    /*input  logic [ 1:0]                               */  .mst_rresp_i                (iommu_mst_rresp                  ),
    /*input  logic                                      */  .mst_rlast_i                (iommu_mst_rlast                  ),
    /*input  logic [BUS_USER_WIDTH-1:0]                 */  .mst_ruser_i                (iommu_mst_ruser                  ),
    /*input  logic                                      */  .mst_rvalid_i               (iommu_mst_rvalid                 ),
    /*input  logic                                      */  .mst_rready_i               (iommu_mst_rready                 ),
    /*input  logic                                      */  .penable_i                  (axi_mon_penable_i                  ),
    /*input  logic                                      */  .pwrite_i                   (axi_mon_pwrite_i                   ),
    /*input  logic [31:0]                               */  .paddr_i                    (axi_mon_paddr_i                    ),
    /*input  logic                                      */  .psel_i                     (axi_mon_psel_i                     ),
    /*input  logic [31:0]                               */  .pwdata_i                   (axi_mon_pwdata_i                   ),
    /*output logic [31:0]                               */  .prdata_o                   (axi_mon_prdata_o                   ),
    /*output logic                                      */  .pready_o                   (axi_mon_pready_o                   ),
    /*output logic                                      */  .pslverr_o                  (axi_mon_pslverr_o                  ),
    /*input  logic                                      */  .spare_i                    (0)
);



//}}}


//=== ACD {{{
iommu_acd #(
//    .BUS_ADDR_WIDTH             (6'd40),
    .BUS_DATA_WIDTH             (9'd256),
    .BUS_SIZE_WIDTH             (2'd3),
    .BUS_STRB_WIDTH             (6'd32),
    .BUS_ID_WIDTH               ('d10),
    .BUS_USER_WIDTH             (4'd8),
    .CTIF_DATA_WIDTH            (7'd64),
    .CTIF_STRB_WIDTH            (4'd8),
    .CTIF_ID_WIDTH              (3'd4),
    .CTIF_DEST_WIDTH            (3'd4),
    .CTIF_USER_WIDTH            (1'b1)
)
u0_iommu_acd
(
    .clk                            (iommu_clk),
    .rstn                           (iommu_rstn & iommu_sf_rstn),
    .slv_awid_i                     (acd_slv_awid),
    .slv_awaddr_i                   ({24'd0, acd_slv_awaddr}),
    .slv_awlen_i                    (acd_slv_awlen),
    .slv_awsize_i                   (acd_slv_awsize),
    .slv_awburst_i                  (acd_slv_awburst),
    .slv_awlock_i                   (acd_slv_awlock),
    .slv_awcache_i                  (acd_slv_awcache),
    .slv_awprot_i                   (acd_slv_awprot),
    .slv_awregion_i                 (acd_slv_awregion),
    .slv_awuser_i                   (acd_slv_awuser[7:0]),
    .slv_awqos_i                    (acd_slv_awqos),
    .slv_awvalid_i                  (acd_slv_awvalid),
    .slv_awready_o                  (acd_slv_awready),
    .slv_aw_device_id_i             (acd_slv_awuser[31:8]),
    .slv_aw_process_id_i            (acd_slv_awuser[51:32]),
    .slv_aw_process_id_valid_i      (acd_slv_awuser[52]),
    .slv_aw_is_translated_i         (acd_slv_awuser[53]),
    .slv_wdata_i                    (acd_slv_wdata),
    .slv_wstrb_i                    (acd_slv_wstrb),
    .slv_wlast_i                    (acd_slv_wlast),
    .slv_wuser_i                    (acd_slv_wuser),
    .slv_wvalid_i                   (acd_slv_wvalid),
    .slv_wready_o                   (acd_slv_wready),
    .slv_bid_o                      (acd_slv_bid),
    .slv_bresp_o                    (acd_slv_bresp),
    .slv_bvalid_o                   (acd_slv_bvalid),
    .slv_buser_o                    (acd_slv_buser),
    .slv_bready_i                   (acd_slv_bready),
    .slv_arid_i                     (acd_slv_arid),
    .slv_araddr_i                   ({24'd0, acd_slv_araddr}),
    .slv_arlen_i                    (acd_slv_arlen),
    .slv_arsize_i                   (acd_slv_arsize),
    .slv_arburst_i                  (acd_slv_arburst),
    .slv_arlock_i                   (acd_slv_arlock),
    .slv_arcache_i                  (acd_slv_arcache),
    .slv_arprot_i                   (acd_slv_arprot),
    .slv_arregion_i                 (acd_slv_arregion),
    .slv_aruser_i                   (acd_slv_aruser[7:0]),
    .slv_arqos_i                    (acd_slv_arqos),
    .slv_arvalid_i                  (acd_slv_arvalid),
    .slv_arready_o                  (acd_slv_arready),
    .slv_ar_device_id_i             (acd_slv_aruser[31:8]),
    .slv_ar_process_id_i            (acd_slv_aruser[51:32]),
    .slv_ar_process_id_valid_i      (acd_slv_aruser[52]),
    .slv_ar_is_translated_i         (acd_slv_aruser[53]),
    .slv_rid_o                      (acd_slv_rid),
    .slv_rdata_o                    (acd_slv_rdata),
    .slv_rresp_o                    (acd_slv_rresp),
    .slv_rlast_o                    (acd_slv_rlast),
    .slv_ruser_o                    (acd_slv_ruser),
    .slv_rvalid_o                   (acd_slv_rvalid),
    .slv_rready_i                   (acd_slv_rready),
    .mst_awid_o                     (acd_mst_awid),
    .mst_awaddr_o                   (iommu_mst_awaddr_mux),
    .mst_awlen_o                    (acd_mst_awlen),
    .mst_awsize_o                   (acd_mst_awsize),
    .mst_awburst_o                  (acd_mst_awburst),
    .mst_awlock_o                   (acd_mst_awlock),
    .mst_awcache_o                  (acd_mst_awcache),
    .mst_awprot_o                   (acd_mst_awprot),
    .mst_awregion_o                 (acd_mst_awregion),
    .mst_awuser_o                   (acd_mst_awuser),
    .mst_awqos_o                    (acd_mst_awqos),
    .mst_awvalid_o                  (acd_mst_awvalid),
    .mst_awready_i                  (acd_mst_awready),
    .mst_wdata_o                    (acd_mst_wdata),
    .mst_wstrb_o                    (acd_mst_wstrb),
    .mst_wlast_o                    (acd_mst_wlast),
    .mst_wuser_o                    (acd_mst_wuser),
    .mst_wvalid_o                   (acd_mst_wvalid),
    .mst_wready_i                   (acd_mst_wready),
    .mst_bid_i                      (acd_mst_bid),
    .mst_bresp_i                    (acd_mst_bresp),
    .mst_bvalid_i                   (acd_mst_bvalid),
    .mst_buser_i                    (acd_mst_buser),
    .mst_bready_o                   (acd_mst_bready),
    .mst_arid_o                     (acd_mst_arid),
    .mst_araddr_o                   (iommu_mst_araddr_mux),
    .mst_arlen_o                    (acd_mst_arlen),
    .mst_arsize_o                   (acd_mst_arsize),
    .mst_arburst_o                  (acd_mst_arburst),
    .mst_arlock_o                   (acd_mst_arlock),
    .mst_arcache_o                  (acd_mst_arcache),
    .mst_arprot_o                   (acd_mst_arprot),
    .mst_arregion_o                 (acd_mst_arregion),
    .mst_aruser_o                   (acd_mst_aruser),
    .mst_arqos_o                    (acd_mst_arqos),
    .mst_arvalid_o                  (acd_mst_arvalid),
    .mst_arready_i                  (acd_mst_arready),
    .mst_rid_i                      (acd_mst_rid),
    .mst_rdata_i                    (acd_mst_rdata),
    .mst_rresp_i                    (acd_mst_rresp),
    .mst_rlast_i                    (acd_mst_rlast),
    .mst_ruser_i                    (acd_mst_ruser),
    .mst_rvalid_i                   (acd_mst_rvalid),
    .mst_rready_o                   (acd_mst_rready),
    .c2t_tvalid_o                   (tc_c2t_rvalid),
    .c2t_tready_i                   (tc_c2t_rready),
    .c2t_tdata_o                    (tc_c2t_rdata ),
    .c2t_tstrb_o                    (tc_c2t_rstrb ),
    .c2t_tkeep_o                    (tc_c2t_rkeep ),
    .c2t_tlast_o                    (tc_c2t_rlast ),
    .c2t_tid_o                      (tc_c2t_rid   ),
    .c2t_tdest_o                    (tc_c2t_rdest ),
    .c2t_tuser_o                    (tc_c2t_ruser ),
    .t2c_tvalid_i                   (tc_t2c_tvalid),
    .t2c_tready_o                   (tc_t2c_tready),
    .t2c_tdata_i                    (tc_t2c_tdata ),
    .t2c_tstrb_i                    (tc_t2c_tstrb ),
    .t2c_tkeep_i                    (tc_t2c_tkeep ),
    .t2c_tlast_i                    (tc_t2c_tlast ),
    .t2c_tid_i                      (tc_t2c_tid   ),
    .t2c_tdest_i                    (tc_t2c_tdest ),
    .t2c_tuser_i                    (tc_t2c_tuser ),
    .acd_tid_i                      (4'ha       ),
    .acd_tdest_i                    (4'hb       ),
    .spare_in(1'b0)
);
//}}}

//=== ATD {{{
iommu_atd #(
//    .RISCV_VLEN           (6'd39),//39
    .RISCV_GLEN         (6'd50),//sv48x4
    .RISCV_PLEN         (6'd56)
)
u1_iommu_atd
(
    .iommu_clk                      (iommu_clk),
    .iommu_rstn                     (iommu_rstn & iommu_sf_rstn),

//    .iommu_penable_i              (iommu_penable_i),
    .iommu_penable_i                (iommu_inst_penable),
//    .iommu_psel_i                 (iommu_psel_i),
    .iommu_psel_i                   (iommu_inst_psel),
    .iommu_pwrite_i                 (iommu_pwrite_i),
    .iommu_paddr_i                  (iommu_paddr_i),
    .iommu_pwdata_i                 (iommu_pwdata_i),
//    .iommu_prdata_o                   (iommu_prdata_o),
    .iommu_prdata_o                 (iommu_inst_prdata),
    .iommu_pready_o                 (iommu_pready_o),
    .iommu_pslverr_o                (iommu_pslverr_o),

    //input from acd
    .atd_c2t_rvalid_i               (tc_c2t_rvalid),
    .atd_c2t_rready_o               (tc_c2t_rready),
    .atd_c2t_rdata_i                (tc_c2t_rdata),
    .atd_c2t_rstrb_i                (tc_c2t_rstrb),
    .atd_c2t_rkeep_i                (tc_c2t_rkeep),
    .atd_c2t_rlast_i                (tc_c2t_rlast),
    .atd_c2t_rid_i                  (tc_c2t_rid ),
    .atd_c2t_rdest_i                (tc_c2t_rdest),
    .atd_c2t_ruser_i                (tc_c2t_ruser[0]),

    //output to acd
    .atd_t2c_tvalid_o               (tc_t2c_tvalid),
    .atd_t2c_tready_i               (tc_t2c_tready),
    .atd_t2c_tdata_o                (tc_t2c_tdata),
    .atd_t2c_tstrb_o                (tc_t2c_tstrb),
    .atd_t2c_tkeep_o                (tc_t2c_tkeep),
    .atd_t2c_tlast_o                (tc_t2c_tlast),
    .atd_t2c_tid_o                  (tc_t2c_tid ),
    .atd_t2c_tdest_o                (tc_t2c_tdest),
    .atd_t2c_tuser_o                (tc_t2c_tuser[0]),

    //pte_axi4
    .atd_axi4_arvalid               (iommu_ds_arvalid),
    .atd_axi4_arready               (iommu_ds_arready),
    .atd_axi4_arid                  (iommu_ds_arid),
    .atd_axi4_araddr                (iommu_ds_araddr),
    .atd_axi4_arlen                 (iommu_ds_arlen),
    .atd_axi4_arsize                (iommu_ds_arsize),
    .atd_axi4_arburst               (iommu_ds_arburst),
    .atd_axi4_arlock                (iommu_ds_arlock),
    .atd_axi4_arcache               (iommu_ds_arcache),
    .atd_axi4_arprot                (iommu_ds_arprot),
    .atd_axi4_arqos                 (iommu_ds_arqos),
    .atd_axi4_rvalid                (iommu_ds_rvalid),
    .atd_axi4_rready                (iommu_ds_rready),
    .atd_axi4_rlast                 (iommu_ds_rlast),
    .atd_axi4_rid                   (iommu_ds_rid),
    .atd_axi4_rresp                 (iommu_ds_rresp),
    .atd_axi4_rdata                 (iommu_ds_rdata),
    .atd_axi4_awvalid               (iommu_ds_awvalid),
    .atd_axi4_awready               (iommu_ds_awready),
    .atd_axi4_awid                  (iommu_ds_awid),
    .atd_axi4_awaddr                (iommu_ds_awaddr),
    .atd_axi4_awlen                 (iommu_ds_awlen),
    .atd_axi4_awsize                (iommu_ds_awsize),
    .atd_axi4_awburst               (iommu_ds_awburst),
    .atd_axi4_awlock                (iommu_ds_awlock),
    .atd_axi4_awcache               (iommu_ds_awcache),
    .atd_axi4_awprot                (iommu_ds_awprot),
    .atd_axi4_awqos                 (iommu_ds_awqos),
    .atd_axi4_wvalid                (iommu_ds_wvalid),
    .atd_axi4_wready                (iommu_ds_wready),
    .atd_axi4_wlast                 (iommu_ds_wlast),
    .atd_axi4_wstrb                 (iommu_ds_wstrb),
    .atd_axi4_wdata                 (iommu_ds_wdata),
    .atd_axi4_bvalid                (iommu_ds_bvalid),
    .atd_axi4_bready                (iommu_ds_bready),
    .atd_axi4_bid                   (iommu_ds_bid),
    .atd_axi4_bresp                 (iommu_ds_bresp)
);
//}}}


endmodule
