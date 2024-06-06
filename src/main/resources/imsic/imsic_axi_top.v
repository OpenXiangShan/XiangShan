/* Copyright bosc
 * author: zhaohong
 * Function: recognize axi-lite msi,and map the setipnum*/

module imsic_axi_top #(
parameter AXI_ID_WIDTH          = 5,     // axi id width.
parameter AXI_ADDR_WIDTH        = 32,     // 15bit is valid when 7 interrupt files,and 1:1if max spec:16384*65 is needed, the least 33bit will be must.
parameter NR_INTP_FILES         = 7,      // m,s,5vs,
parameter NR_HARTS              = 1,      //64,      // the total harts number in each group.
parameter NR_SRC                = 256,     // msi src number,1~2047.
parameter SETIP_KEEP_CYCLES     = 8,
// DO NOT INSTANCE BY PARAMETER
localparam NR_SRC_WIDTH         = $clog2(NR_SRC),
localparam NR_HARTS_WIDTH       = (NR_HARTS ==1) ? 1 : $clog2(NR_HARTS),             //6
localparam INTP_FILE_WIDTH      = $clog2(NR_INTP_FILES), // 3
localparam MSI_INFO_WIDTH       = NR_HARTS_WIDTH + INTP_FILE_WIDTH + NR_SRC_WIDTH   //17bit
)
(
//  crg
input                                       axi_clk        ,
input                                       axi_rstn       , 
input                                       fifo_rstn      , //axi_rstn & sw_rstn. 
// bus to access the m interrupt file
input                                       m_s_awvalid    ,     
input       [AXI_ADDR_WIDTH-1:0]            m_s_awaddr     ,     
output wire [AXI_ID_WIDTH-1  :0]            m_s_bid        ,      
output wire [AXI_ID_WIDTH-1  :0]            m_s_rid        ,      
input       [AXI_ID_WIDTH-1  :0]            m_s_arid       ,      
input       [AXI_ID_WIDTH-1  :0]            m_s_awid       ,      
output wire                                 m_s_awready    ,      
input  wire                                 m_s_wvalid     ,      
output wire                                 m_s_wready     ,      
input  wire [31:0]                          m_s_wdata      ,
output wire                                 m_s_bvalid     ,      
input  wire                                 m_s_bready     ,      
output wire [1:0]                           m_s_bresp      ,
input                                       m_s_arvalid    ,      
input       [AXI_ADDR_WIDTH-1:0]            m_s_araddr     ,     
output wire                                 m_s_arready    ,      
output wire                                 m_s_rvalid     ,      
input                                       m_s_rready     ,      
output wire [31:0]                          m_s_rdata      ,     
output wire [1:0]                           m_s_rresp      ,
//bus to access the s interrupt file
input                                       s_s_awvalid    ,     
input       [AXI_ADDR_WIDTH-1:0]            s_s_awaddr     ,     
output wire [AXI_ID_WIDTH-1  :0]            s_s_bid        ,      
output wire [AXI_ID_WIDTH-1  :0]            s_s_rid        ,      
input       [AXI_ID_WIDTH-1  :0]            s_s_arid       ,      
input       [AXI_ID_WIDTH-1  :0]            s_s_awid       ,      
output wire                                 s_s_awready    ,      
input  wire                                 s_s_wvalid     ,      
output wire                                 s_s_wready     ,      
input  wire [31:0]                          s_s_wdata      ,
output wire                                 s_s_bvalid     ,      
input  wire                                 s_s_bready     ,      
output wire [1:0]                           s_s_bresp      ,
input                                       s_s_arvalid    ,      
input       [AXI_ADDR_WIDTH-1:0]            s_s_araddr     ,     
output wire                                 s_s_arready    ,      
output wire                                 s_s_rvalid     ,      
input                                       s_s_rready     ,      
output wire [31:0]                          s_s_rdata      ,     
output wire [1:0]                           s_s_rresp      ,
//imsic_csr_top
output wire [MSI_INFO_WIDTH-1:0]            o_msi_info     ,
output wire                                 o_msi_info_vld    // m,s,5vs,4harts.0-3:hart0-hart3 m file. 4-9:hart0 s+vs file.
);

wire                             fifo_wr          ;
wire                             addr_is_illegal  ;
wire                             m_reg_wr         ;
wire [31:0]                      m_reg_waddr      ;
wire [31:0]                      m_reg_wdata      ;
wire                             msi_m_idle       ;

wire                             s_reg_wr         ;
wire [31:0]                      s_reg_waddr      ;
wire [31:0]                      s_reg_wdata      ;
wire                             msi_s_idle       ;

imsic_axi2reg #(
    .IS_INTP_MFILE               (1             ),
    .AXI_ID_WIDTH                (AXI_ID_WIDTH  ),
    .AXI_ADDR_WIDTH              (AXI_ADDR_WIDTH)
)u_m_imsic_axi2reg(
    //input signal
    .clk                         (axi_clk                        ),
    .rstn                        (axi_rstn                       ),
    .bid_s                       (m_s_bid[AXI_ID_WIDTH-1: 0]     ),   
    .rid_s                       (m_s_rid[AXI_ID_WIDTH-1: 0]     ),   
    .arid_s                      (m_s_arid[AXI_ID_WIDTH-1:0]     ),     
    .awid_s                      (m_s_awid[AXI_ID_WIDTH-1:0]     ),     
    .awvalid_s                   (m_s_awvalid                    ),
    .awaddr_s                    (m_s_awaddr[AXI_ADDR_WIDTH-1:0] ),
    .wvalid_s                    (m_s_wvalid                     ),
    .wdata_s                     (m_s_wdata                      ),
    .bready_s                    (m_s_bready                     ),
    .bresp_s                     (m_s_bresp[1:0]                 ),
    .arvalid_s                   (m_s_arvalid                    ),
    .araddr_s                    (m_s_araddr[AXI_ADDR_WIDTH-1:0] ),
    .rvalid_s                    (m_s_rvalid                      ),
    .addr_is_illegal             (addr_is_illegal                ),
    .fifo_wr                     (fifo_wr                        ),
    .msi_recv_vld                (msi_s_idle                     ),
    .msi_idle                    (msi_m_idle                     ),
    .awready_s                   (m_s_awready                    ),
    .wready_s                    (m_s_wready                     ),
    .bvalid_s                    (m_s_bvalid                     ),
    .arready_s                   (m_s_arready                    ),
    .rready_s                    (m_s_rready                     ),
    .rdata_s                     (m_s_rdata[31:0]                ),
    .rresp_s                     (m_s_rresp[1:0]                 ),
    .reg_wr                      (m_reg_wr                       ),
    .reg_waddr                   (m_reg_waddr[AXI_ADDR_WIDTH-1:0]),
    .reg_wdata                   (m_reg_wdata[31:0]              )                             
);

imsic_axi2reg #(
    .IS_INTP_MFILE               (0             ),
    .AXI_ID_WIDTH                (AXI_ID_WIDTH  ),
    .AXI_ADDR_WIDTH              (AXI_ADDR_WIDTH)
)u_s_imsic_axi2reg(
    //input signal
    .clk                         (axi_clk                        ),
    .rstn                        (axi_rstn                       ),
    .bid_s                       (s_s_bid[AXI_ID_WIDTH-1: 0]     ),   
    .rid_s                       (s_s_rid[AXI_ID_WIDTH-1: 0]     ),   
    .arid_s                      (s_s_arid[AXI_ID_WIDTH-1:0]     ),     
    .awid_s                      (s_s_awid[AXI_ID_WIDTH-1:0]     ),     
    .awvalid_s                   (s_s_awvalid                    ),
    .awaddr_s                    (s_s_awaddr[AXI_ADDR_WIDTH-1:0] ),
    .wvalid_s                    (s_s_wvalid                     ),
    .wdata_s                     (s_s_wdata                      ),
    .bready_s                    (s_s_bready                     ),
    .bresp_s                     (s_s_bresp[1:0]                 ),
    .arvalid_s                   (s_s_arvalid                    ),
    .araddr_s                    (s_s_araddr[AXI_ADDR_WIDTH-1:0] ),
    .rvalid_s                    (s_s_rvalid                     ),
    .addr_is_illegal             (addr_is_illegal                ),
    .fifo_wr                     (fifo_wr                        ),
    .msi_recv_vld                (msi_m_idle                     ),
    .msi_idle                    (msi_s_idle                     ),
    .awready_s                   (s_s_awready                    ),
    .wready_s                    (s_s_wready                     ),
    .bvalid_s                    (s_s_bvalid                     ),
    .arready_s                   (s_s_arready                    ),
    .rready_s                    (s_s_rready                     ),
    .rdata_s                     (s_s_rdata[31:0]                ),
    .rresp_s                     (s_s_rresp[1:0]                 ),
    .reg_wr                      (s_reg_wr                       ),
    .reg_waddr                   (s_reg_waddr[AXI_ADDR_WIDTH-1:0]),
    .reg_wdata                   (s_reg_wdata[31:0]              )                             
);

imsic_regmap #(
    .AXI_ADDR_WIDTH    (AXI_ADDR_WIDTH   ) ,
    .SETIP_KEEP_CYCLES (SETIP_KEEP_CYCLES) ,
    .NR_SRC_WIDTH      (NR_SRC_WIDTH     ) , 
    .NR_HARTS          (NR_HARTS         ) , 
    .NR_HARTS_WIDTH    (NR_HARTS_WIDTH   ) , 
    .FIFO_DATA_WIDTH   (MSI_INFO_WIDTH   ) , 
    .NR_INTP_FILES     (NR_INTP_FILES    ) ,
    .INTP_FILE_WIDTH   (INTP_FILE_WIDTH  ) 
) u_imsic_regmap(
    //input signal
    .clk                         (axi_clk                        ),
    .rstn                        (fifo_rstn                      ),//modify by zhaohong.2024.05.20
    .fifo_rstn                   (fifo_rstn                      ),
    .msi_s_busy                  ((~msi_s_idle)                  ),
    .m_reg_wr                    (m_reg_wr                       ),
    .m_reg_waddr                 (m_reg_waddr[AXI_ADDR_WIDTH-1:0]),
    .m_reg_wdata                 (m_reg_wdata[31:0]              ),                            
    .fifo_wr                     (fifo_wr                        ),
    .s_reg_wr                    (s_reg_wr                       ),
    .s_reg_waddr                 (s_reg_waddr[AXI_ADDR_WIDTH-1:0]),
    .s_reg_wdata                 (s_reg_wdata[31:0]              ),                            
    //output signal
    .addr_is_illegal             (addr_is_illegal                ),
    .o_msi_info_vld              (o_msi_info_vld                 ),
    .o_msi_info                  (o_msi_info[MSI_INFO_WIDTH-1:0])
);



endmodule
