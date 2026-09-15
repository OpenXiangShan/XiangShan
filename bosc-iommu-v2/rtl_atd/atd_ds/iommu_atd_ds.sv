///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd_ds.sv
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


module iommu_atd_ds
(
    input logic                     iommu_clk,
    input logic                     iommu_rstn,

    //cdw
    input                           cdw_arvalid,
    output                          cdw_arready,
    input  [3:0]                    cdw_arid,
    input  [56-1:0]                 cdw_araddr,
    input  [7:0]                    cdw_arlen,
    output                          cdw_rvalid,
    input                           cdw_rready,
    output                          cdw_rlast,
    output [3:0]                    cdw_rid,
    output [1:0]                    cdw_rresp,
    output [255:0]                  cdw_rdata,

    //ptw
    input                           ptw_arvalid,
    output                          ptw_arready,
    input  [3:0]                    ptw_arid,
    input  [56-1:0]                 ptw_araddr,
    input  [7:0]                    ptw_arlen,
    output                          ptw_rvalid,
    input                           ptw_rready,
    output                          ptw_rlast,
    output [3:0]                    ptw_rid,
    output [1:0]                    ptw_rresp,
    output [255:0]                  ptw_rdata,

    //ace5-liteDVM
    output                          cq_acvalid,
    input                           cq_acready,
    output  [52-1:0]                cq_acaddr,
    output  [3:0]                   cq_acvmidext,
    output  [3:0]                   cq_acsnoop,
    output  [2:0]                   cq_acprot,
    input                           cq_crvalid,
    output                          cq_crready,
    input  [4:0]                    cq_crresp,
    input  [1:0]                    cq_ardomain,
    input  [3:0]                    cq_arsnoop,
    input   [1:0]                   cq_arbar,
    //cq
    input                           cq_arvalid,
    output                          cq_arready,
    input  [3:0]                    cq_arid,
    input  [56-1:0]                 cq_araddr,
    input  [7:0]                    cq_arlen,
    output                          cq_rvalid,
    input                           cq_rready,
    output                          cq_rlast,
    output [3:0]                    cq_rid,
    output [1:0]                    cq_rresp,
    output [255:0]                  cq_rdata,

    //fq
    input                           fq_awvalid,
    output                          fq_awready,
    input  [3:0]                    fq_awid,
    input  [56-1:0]                 fq_awaddr,
    input  [7:0]                    fq_awlen,
    input                           fq_wvalid,
    output                          fq_wready,
    input                           fq_wlast,
    input  [31:0]                   fq_wstrb,
    input  [255:0]                  fq_wdata,
    output                          fq_bvalid,
    input                           fq_bready,
    output [3:0]                    fq_bid,
    output [1:0]                    fq_bresp,

    //pq
    input                           pq_awvalid,
    output                          pq_awready,
    input  [3:0]                    pq_awid,
    input  [56-1:0]                 pq_awaddr,
    input  [7:0]                    pq_awlen,
    input                           pq_wvalid,
    output                          pq_wready,
    input                           pq_wlast,
    input  [31:0]                   pq_wstrb,
    input  [255:0]                  pq_wdata,
    output                          pq_bvalid,
    input                           pq_bready,
    output [3:0]                    pq_bid,
    output [1:0]                    pq_bresp,

    //msi
    input  [5:0]                    msi_awatop,
    input                           msi_awvalid,
    output                          msi_awready,
    input  [3:0]                    msi_awid,
    input  [56-1:0]                 msi_awaddr,
    input  [7:0]                    msi_awlen,
    input  [2:0]                    msi_awsize,
    input  [3:0]                    msi_awcache,
    input                           msi_wvalid,
    output                          msi_wready,
    input                           msi_wlast,
    input  [31:0]                   msi_wstrb,
    input  [255:0]                  msi_wdata,
    output                          msi_bvalid,
    input                           msi_bready,
    output [3:0]                    msi_bid,
    output [1:0]                    msi_bresp,

    //ptw
    input                           ptw_awvalid,
    output                          ptw_awready,
    input  [3:0]                    ptw_awid,
    input  [56-1:0]                 ptw_awaddr,
    input  [7:0]                    ptw_awlen,
    input                           ptw_wvalid,
    output                          ptw_wready,
    input                           ptw_wlast,
    input  [31:0]                   ptw_wstrb,
    input  [255:0]                  ptw_wdata,
    output                          ptw_bvalid,
    input                           ptw_bready,
    output [3:0]                    ptw_bid,
    output [1:0]                    ptw_bresp,

    //ace5-liteDVM
    input                           ds_ace5_acvalid,
    output                          ds_ace5_acready,
    input  [52-1:0]                 ds_ace5_acaddr,
    input  [3:0]                    ds_ace5_acvmidext,
    input  [3:0]                    ds_ace5_acsnoop,
    input  [2:0]                    ds_ace5_acprot,
    output                          ds_ace5_crvalid,
    input                           ds_ace5_crready,
    output  [4:0]                   ds_ace5_crresp,
    //pte_axi4
    output  [1:0]                   ds_ace5_ardomain,
    output  [3:0]                   ds_ace5_arsnoop,
    output  [1:0]                   ds_ace5_arbar,
    output                          ds_ace5_arvalid,
    input                           ds_ace5_arready,
    output [5:0]                    ds_ace5_arid,
    output [56-1:0]                 ds_ace5_araddr,
    output [7:0]                    ds_ace5_arlen,
    output [2:0]                    ds_ace5_arsize,
    output [1:0]                    ds_ace5_arburst,
    output                          ds_ace5_arlock,
    output [3:0]                    ds_ace5_arcache,
    output [2:0]                    ds_ace5_arprot,
    output [3:0]                    ds_ace5_arqos,

    input                           ds_ace5_rvalid,
    output                          ds_ace5_rready,
    input                           ds_ace5_rlast,
    input  [5:0]                    ds_ace5_rid,
    input  [1:0]                    ds_ace5_rresp,
    input  [255:0]                  ds_ace5_rdata,

    output [5:0]                    ds_ace5_awatop,
    output  [1:0]                   ds_ace5_awdomain,
    output                          ds_ace5_awvalid,
    input                           ds_ace5_awready,
    output [5:0]                    ds_ace5_awid,
    output [56-1:0]                 ds_ace5_awaddr,
    output [7:0]                    ds_ace5_awlen,
    output [2:0]                    ds_ace5_awsize,
    output [1:0]                    ds_ace5_awburst,
    output                          ds_ace5_awlock,
    output [3:0]                    ds_ace5_awcache,
    output [2:0]                    ds_ace5_awprot,
    output [3:0]                    ds_ace5_awqos,

    output                          ds_ace5_wvalid,
    input                           ds_ace5_wready,
    output                          ds_ace5_wlast,
    output [31:0]                   ds_ace5_wstrb,
    output [255:0]                  ds_ace5_wdata,
    input                           ds_ace5_bvalid,
    output                          ds_ace5_bready,
    input  [5:0]                    ds_ace5_bid,
    input  [1:0]                    ds_ace5_bresp
);


    assign cq_acvalid       = ds_ace5_acvalid;
    assign cq_acaddr        = ds_ace5_acaddr;
    assign cq_acvmidext     = ds_ace5_acvmidext;
    assign cq_acsnoop       = ds_ace5_acsnoop;
    assign cq_acprot        = ds_ace5_acprot;
    assign ds_ace5_acready  = cq_acready;

    assign ds_ace5_crvalid  = cq_crvalid;
    assign ds_ace5_crresp   = cq_crresp;
    assign cq_crready       = ds_ace5_crready;


logic [63+8:0]          cdw_arbus;
logic [63+8:0]          ptw_arbus;
logic [63+8:0]          cq_arbus;
logic [63+8:0]          ds_ace5_arbus;


assign cdw_arbus = {8'd0,cdw_araddr,cdw_arlen};
assign ptw_arbus = {8'd0,ptw_araddr,ptw_arlen};
assign cq_arbus = {cq_ardomain,cq_arsnoop,cq_arbar,cq_araddr,cq_arlen};

//araddr
atd_arb_a
    #(
    .pPORT_N                ( 3 ),
    .pARID_W                ( 4 ),
    .pMID_W                 ( 2 ),
    .pDATA_W                ( 8+56+8)
  )
u0_arb_3to1_ar
    (
    .clk                    (iommu_clk),
    .pin_rstn               (iommu_rstn),

    .buffer_grant           ({cdw_arready,ptw_arready,cq_arready}),
    .buffer_req             ({cdw_arvalid,ptw_arvalid,cq_arvalid}),
    .buffer_araddr          ({cdw_arbus,ptw_arbus,cq_arbus}),
    .buffer_arid            ({cdw_arid,ptw_arid,cq_arid}),

    .sram_grant             (ds_ace5_arready),
    .cb_req                 (ds_ace5_arvalid),
    .cb_req_addr            (ds_ace5_arbus),
    .cb_req_arid            (ds_ace5_arid)
);

assign ds_ace5_ardomain = ds_ace5_arbus[71:70];
assign ds_ace5_arsnoop = ds_ace5_arbus[69:66];
assign ds_ace5_arbar = ds_ace5_arbus[65:64];
assign ds_ace5_araddr = ds_ace5_arbus[64-1:8];
assign ds_ace5_arlen = ds_ace5_arbus[7:0];
assign ds_ace5_arsize = 3'd5;
assign ds_ace5_arburst = 2'd1;
assign ds_ace5_arlock = 1'b0;
assign ds_ace5_arcache = 4'd0;
assign ds_ace5_arprot = 3'd0;
assign ds_ace5_arqos = 4'd0;

logic [258:0]           ds_ace5_rbus;
logic [258:0]           cdw_rbus;
logic [258:0]           ptw_rbus;
logic [258:0]           cq_rbus;


assign ds_ace5_rbus = {ds_ace5_rlast,ds_ace5_rresp,ds_ace5_rdata};

atd_rou_ad
    #(
    .pPORT_NUM              (3      ),
    .pDATA_W                (259    ),
    .pARID_W                (4      ),
    .pMODE_SEL              (1      ),
    .pRMID_W                (2      ),
    .TOP_BUF_SIZE           (8      ),
    .TOP_BUF_VALID          (8'hff  ),
    .BOT_BUF_SIZE           (8      ),
    .BOT_BUF_VALID          (8'hff  )
  )
u1_rou_1to3_r
    (
     .clk                   (iommu_clk),
     .pin_rstn              (iommu_rstn),

     .port_araddr           (ds_ace5_rbus),
     .port_arid             (ds_ace5_rid[3:0]),
     .port_arvalid          (ds_ace5_rvalid),
     .port_arready          (ds_ace5_rready),
     .port_rmid             (ds_ace5_rid[5:4]),

     .buffer_grant          ({cdw_rready,ptw_rready,cq_rready}),
     .buffer_req            ({cdw_rvalid,ptw_rvalid,cq_rvalid}),
     .buffer_araddr         ({cdw_rbus,ptw_rbus,cq_rbus}),
     .buffer_arid           ({cdw_rid,ptw_rid,cq_rid})
    );

assign cdw_rlast = cdw_rbus[258];
assign cdw_rresp = cdw_rbus[257:256];
assign cdw_rdata = cdw_rbus[255:0];
assign ptw_rlast = ptw_rbus[258];
assign ptw_rresp = ptw_rbus[257:256];
assign ptw_rdata = ptw_rbus[255:0];
assign cq_rlast = cq_rbus[258];
assign cq_rresp = cq_rbus[257:256];
assign cq_rdata = cq_rbus[255:0];


//awaddr
logic [63+13:0]         pq_awbus;
logic [63+13:0]         fq_awbus;
logic [63+13:0]         msi_awbus;
logic [63+13:0]         ptw_awbus;
logic [63+13:0]         ds_ace5_awbus;

assign pq_awbus = {4'h0,3'd5,6'd0,pq_awaddr,pq_awlen};
assign fq_awbus = {4'h0,3'd5,6'd0,fq_awaddr,fq_awlen};
assign msi_awbus = {msi_awcache,msi_awsize,msi_awatop,msi_awaddr,msi_awlen};
assign ptw_awbus = {4'hf,3'd0,6'h13,ptw_awaddr,ptw_awlen};

//aw
atd_arb_a
# (
    .pPORT_N     ( 4    ),
    .pARID_W     ( 4    ),
    .pMID_W      ( 2    ),
    .pDATA_W     (13+56+8   )
    )
u2_arb_3to1_aw
(
    .clk           (iommu_clk),
    .pin_rstn      (iommu_rstn),

    .buffer_grant  ({pq_awready,fq_awready,msi_awready,ptw_awready}),
    .buffer_req    ({pq_awvalid,fq_awvalid,msi_awvalid,ptw_awvalid}),
    .buffer_araddr ({pq_awbus,fq_awbus,msi_awbus,ptw_awbus}),
    .buffer_arid   ({pq_awid,fq_awid,msi_awid,ptw_awid}),

    .sram_grant   (ds_ace5_awready),
    .cb_req       (ds_ace5_awvalid),
    .cb_req_addr  (ds_ace5_awbus),
    .cb_req_arid  (ds_ace5_awid)
);

assign ds_ace5_awcache = ds_ace5_awbus[76:73];
assign ds_ace5_awsize = ds_ace5_awbus[72:70];
assign ds_ace5_awatop = ds_ace5_awbus[69:64];
assign ds_ace5_awaddr = ds_ace5_awbus[64-1:8];
assign ds_ace5_awlen = ds_ace5_awbus[7:0];
assign ds_ace5_awburst = 2'b1;
assign ds_ace5_awlock = 1'b0;
assign ds_ace5_awprot = 3'd0;
assign ds_ace5_awqos = 4'd0;
assign ds_ace5_awdomain = 2'b11;

logic [288:0]           pq_wbus;
logic [288:0]           fq_wbus;
logic [288:0]           msi_wbus;
logic [288:0]           ptw_wbus;
logic [288:0]           ds_ace5_wbus;

assign pq_wbus = {pq_wlast,pq_wstrb,pq_wdata};
assign fq_wbus = {fq_wlast,fq_wstrb,fq_wdata};
assign msi_wbus = {msi_wlast,msi_wstrb,msi_wdata};
assign ptw_wbus = {ptw_wlast,ptw_wstrb,ptw_wdata};

//wdata
atd_arb_d
    #(
    .pPORT_N                ( 4     ),
    .pARID_W                ( 4     ),
    .pDATA_W                ( 289   ),
    .FIFO_TMO               ( 1     )
)
u3_arb_3to1_w
    (
    .clk                    (iommu_clk           ),
    .pin_rstn               (iommu_rstn          ),

    .buffer_rready          ({pq_wready,fq_wready,msi_wready,ptw_wready}),
    .buffer_rvalid          ({pq_wvalid,fq_wvalid,msi_wvalid,ptw_wvalid}),
    .buffer_rdata           ({pq_wbus,fq_wbus,msi_wbus,ptw_wbus}),
    .buffer_rid             ({pq_awid,fq_awid,msi_awid,ptw_awid}),

    .port_rdata             (ds_ace5_wbus),
    .port_rvalid            (ds_ace5_wvalid),
    .port_rid               (),
    .port_rready            (ds_ace5_wready)

);

assign ds_ace5_wlast = ds_ace5_wbus[288];
assign ds_ace5_wstrb = ds_ace5_wbus[287:256];
assign ds_ace5_wdata = ds_ace5_wbus[255:0];


atd_rou_ad
    # (
    .pPORT_NUM              (4   ),
    .pDATA_W                (2  ),
    .pARID_W                (4   ),
    //.pMODE_SEL            (0   ),
    .pMODE_SEL              (1   ),
    .pRMID_W                (2   ),
    .TOP_BUF_SIZE           (8  ),
    .TOP_BUF_VALID          (8'hff),
    .BOT_BUF_SIZE           (8  ),
    .BOT_BUF_VALID          (8'hff)
    )
    u4_rou_1to3_b
    (
     .clk                   (iommu_clk),
     .pin_rstn              (iommu_rstn),

     .port_araddr           (ds_ace5_bresp),
     .port_arid             (ds_ace5_bid[3:0]),
     .port_arvalid          (ds_ace5_bvalid),
     .port_arready          (ds_ace5_bready),
     .port_rmid             (ds_ace5_bid[5:4]),

     .buffer_grant          ({pq_bready,fq_bready,msi_bready,ptw_bready}),
     .buffer_req            ({pq_bvalid,fq_bvalid,msi_bvalid,ptw_bvalid}),
     .buffer_araddr         ({pq_bresp,fq_bresp,msi_bresp,ptw_bresp}),
     .buffer_arid           ({pq_bid,fq_bid,msi_bid,ptw_bid})
    );



endmodule



