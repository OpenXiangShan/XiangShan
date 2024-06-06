/* Copyright bosc
 * author: zhaohong
 * Function: handel axi-lite msi ,change it to reg access*/
module imsic_axi2reg #(
parameter IS_INTP_MFILE         = 0  ,
parameter AXI_ID_WIDTH          = 32 ,
parameter AXI_ADDR_WIDTH        = 32 
) 
(
//  crg
input                                   clk            ,
input                                   rstn           ,
// bus                                                  
input                                   awvalid_s      ,  
input       [AXI_ADDR_WIDTH-1:0]        awaddr_s       , 
output reg                              awready_s      ,   
input                                   wvalid_s       ,  
output reg                              wready_s       ,  
output reg  [AXI_ID_WIDTH-1:0]          bid_s          ,  
output reg  [AXI_ID_WIDTH-1:0]          rid_s          ,  
input       [AXI_ID_WIDTH-1:0]          arid_s         ,  
input       [AXI_ID_WIDTH-1:0]          awid_s         ,  
input       [31:0]                      wdata_s        ,
output reg                              bvalid_s       ,  
input                                   bready_s       ,  
output reg  [1:0]                       bresp_s        ,
input                                   arvalid_s      ,   
input       [AXI_ADDR_WIDTH-1:0]        araddr_s       , 
output reg                              arready_s      ,   
output reg                              rvalid_s       ,  
input                                   rready_s       ,  
output wire [31:0]                      rdata_s        ,    
output wire [1:0]                       rresp_s        ,
//other imsic_axi2reg
output wire                             msi_idle       ,
input                                   msi_recv_vld   ,
//imsic_regmap                                          
input                                   addr_is_illegal,
input                                   fifo_wr        ,
output wire                             reg_wr         ,
output reg  [AXI_ADDR_WIDTH-1:0]        reg_waddr      ,
output reg  [31:0]                      reg_wdata       
);
//parameter define
localparam      IDLE_ST                 =2'b00;
localparam      WR_DATA_ST              =2'b01;
localparam      WR_RESP_ST              =2'b11;
localparam      RD_ST                   =2'b10;
// signal define
reg     [1:0]                       imsic_curr_st;
reg     [1:0]                       imsic_next_st;
wire                                is_mfile     ;

assign is_mfile = IS_INTP_MFILE ? awvalid_s : 1'b0;
assign msi_idle = (imsic_curr_st == IDLE_ST) & (~is_mfile);
//start:code about state machine
always @(posedge clk or negedge rstn)
begin
    if (~rstn)
        imsic_curr_st[1:0] <= IDLE_ST;
    else 
        imsic_curr_st[1:0] <= imsic_next_st[1:0];
end
always @(*)
begin
    imsic_next_st = imsic_curr_st; 
    case (imsic_curr_st) 
    IDLE_ST:
    begin
        if (msi_recv_vld) begin
            if (awvalid_s)// write trigger state machine.
                imsic_next_st = WR_DATA_ST; 
            else if (arvalid_s)// read trigger state machine.
                imsic_next_st = RD_ST; 
        end
    end
    WR_DATA_ST:
    begin
        if (wvalid_s)begin
            imsic_next_st = WR_RESP_ST; 
        end
    end
    WR_RESP_ST:
    begin
        if (bvalid_s & bready_s)begin
            imsic_next_st = IDLE_ST; 
        end
    end
    RD_ST:
    begin
        if (rvalid_s & rready_s)begin
            imsic_next_st = IDLE_ST; 
        end
    end
    default:
            imsic_next_st = IDLE_ST; 
    endcase
end

always @(posedge clk or negedge rstn)
begin
    if (~rstn) begin
        awready_s <= 1'b0;
        bid_s     <= {AXI_ID_WIDTH{1'b0}};
    end
    else if ((imsic_curr_st == IDLE_ST) && (imsic_next_st == WR_DATA_ST))begin
        awready_s <= 1'b1;
        bid_s     <= awid_s;
    end
    else begin
        awready_s <= 1'b0;
        bid_s     <= bid_s;
    end
end


always @(posedge clk or negedge rstn)
begin
    if (~rstn) 
        wready_s <= 1'b0;
    else if ((imsic_curr_st == WR_DATA_ST) && (imsic_next_st == WR_RESP_ST))
        wready_s <= 1'b1;
    else 
        wready_s <= 1'b0;
end
always @(posedge clk or negedge rstn)
begin
    if (~rstn) begin
        bvalid_s    <= 1'b0;
        bresp_s[1:0]<= 2'b00;
    end
    else if ((imsic_next_st == WR_RESP_ST) & (fifo_wr | (wready_s & addr_is_illegal))) begin
        bvalid_s <= 1'b1;
        if (addr_is_illegal)
            bresp_s[1:0] <= 2'b11; // report DECERR if axi addr is not illegal interrupt file address.
        else
            bresp_s[1:0]<= 2'b00;
    end
    else if (bready_s) begin
        bvalid_s <= 1'b0;
        bresp_s[1:0]<= 2'b00;
    end
end
//start:code about read access

always @(posedge clk or negedge rstn)
begin
    if (~rstn) begin
        arready_s <= 1'b0;
        rid_s     <= {AXI_ID_WIDTH{1'b0}};
    end
    else if ((imsic_curr_st == IDLE_ST) && (imsic_next_st == RD_ST))begin
        arready_s <= 1'b1;
        rid_s     <= arid_s;
    end
    else begin
        arready_s <= 1'b0;
        rid_s     <= arid_s;
    end
end
always @(posedge clk or negedge rstn)
begin
    if (~rstn)
        rvalid_s <= 1'b0;
    else if (arready_s)
        rvalid_s <= 1'b1;
    else if (rready_s)
        rvalid_s <= 1'b0;
end
assign rresp_s[1:0] = 2'b00;
// code on reg interface
assign reg_wr       = wready_s;
always @(posedge clk or negedge rstn)
begin
    if (~rstn)    
        reg_waddr  <=  {AXI_ADDR_WIDTH{1'b0}};
    else if (awvalid_s & awready_s)
        reg_waddr  <=  awaddr_s;
    else;
end
always @(posedge clk or negedge rstn)
begin
    if (~rstn)    
        reg_wdata[31:0] <=  32'h0;
    else if (wvalid_s&(imsic_curr_st == WR_DATA_ST))
        reg_wdata[31:0] <=  wdata_s[31:0];
    else;
end
assign rdata_s[31:0] = 32'h0;
endmodule
