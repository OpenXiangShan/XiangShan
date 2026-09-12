module iommu_axi_mon #(
    parameter  BUS_ADDR_WIDTH              = 64,
    parameter  BUS_DATA_WIDTH              = 128,
    parameter  BUS_SIZE_WIDTH              = 3,
    parameter  BUS_STRB_WIDTH              = BUS_DATA_WIDTH/8,
    parameter  BUS_ID_WIDTH                = 8,
    parameter  BUS_USER_WIDTH              = 8,
    parameter  AWRAM_IDX_WIDTH             = 7,
    parameter  ARRAM_IDX_WIDTH             = 7,
    parameter   WRAM_IDX_WIDTH             = 7,
    parameter   BRAM_IDX_WIDTH             = 7,
    parameter   RRAM_IDX_WIDTH             = 7,
    parameter  SPARE_PARAM                 = 0
)(
    input logic                                         clk             ,
    input logic                                         rstn            ,

    input  logic [BUS_ID_WIDTH-1:0]                     slv_awid_i      ,
    input  logic [BUS_ADDR_WIDTH-1:0]                   slv_awaddr_i    ,
    input  logic [ 7:0]                                 slv_awlen_i     ,
    input  logic [BUS_SIZE_WIDTH-1:0]                   slv_awsize_i    ,
    input  logic [ 1:0]                                 slv_awburst_i   ,
    input  logic                                        slv_awlock_i    ,
    input  logic [ 3:0]                                 slv_awcache_i   ,
    input  logic [ 2:0]                                 slv_awprot_i    ,
    input  logic [ 3:0]                                 slv_awregion_i  ,
    input  logic [BUS_USER_WIDTH-1:0]                   slv_awuser_i    ,
    input  logic [ 3:0]                                 slv_awqos_i     ,
    input  logic                                        slv_awvalid_i   ,
    input  logic                                        slv_awready_i   ,
    input  logic [23:0]                                 slv_aw_device_id_i,
    input  logic [19:0]                                 slv_aw_process_id_i,
    input  logic                                        slv_aw_process_id_valid_i,
    input  logic                                        slv_aw_is_translated_i,
    input  logic [BUS_DATA_WIDTH-1:0]                   slv_wdata_i     ,
    input  logic [BUS_STRB_WIDTH-1:0]                   slv_wstrb_i     ,
    input  logic                                        slv_wlast_i     ,
    input  logic [BUS_USER_WIDTH-1:0]                   slv_wuser_i     ,
    input  logic                                        slv_wvalid_i    ,
    input  logic                                        slv_wready_i    ,
    input  logic [BUS_ID_WIDTH-1:0]                     slv_bid_i       ,
    input  logic [ 1:0]                                 slv_bresp_i     ,
    input  logic                                        slv_bvalid_i    ,
    input  logic [BUS_USER_WIDTH-1:0]                   slv_buser_i     ,
    input  logic                                        slv_bready_i    ,
    input  logic [BUS_ID_WIDTH-1:0]                     slv_arid_i      ,
    input  logic [BUS_ADDR_WIDTH-1:0]                   slv_araddr_i    ,
    input  logic [ 7:0]                                 slv_arlen_i     ,
    input  logic [BUS_SIZE_WIDTH-1:0]                   slv_arsize_i    ,
    input  logic [ 1:0]                                 slv_arburst_i   ,
    input  logic                                        slv_arlock_i    ,
    input  logic [ 3:0]                                 slv_arcache_i   ,
    input  logic [ 2:0]                                 slv_arprot_i    ,
    input  logic [ 3:0]                                 slv_arregion_i  ,
    input  logic [ BUS_USER_WIDTH-1:0]                  slv_aruser_i    ,
    input  logic [ 3:0]                                 slv_arqos_i     ,
    input  logic                                        slv_arvalid_i   ,
    input  logic                                        slv_arready_i   ,
    input  logic [23:0]                                 slv_ar_device_id_i,
    input  logic [19:0]                                 slv_ar_process_id_i,
    input  logic                                        slv_ar_process_id_valid_i,
    input  logic                                        slv_ar_is_translated_i,
    input  logic [BUS_ID_WIDTH-1:0]                     slv_rid_i       ,
    input  logic [BUS_DATA_WIDTH-1:0]                   slv_rdata_i     ,
    input  logic [ 1:0]                                 slv_rresp_i     ,
    input  logic                                        slv_rlast_i     ,
    input  logic [BUS_USER_WIDTH-1:0]                   slv_ruser_i     ,
    input  logic                                        slv_rvalid_i    ,
    input  logic                                        slv_rready_i    ,

    input  logic [BUS_ID_WIDTH-1:0]                     mst_awid_i      ,
    input  logic [BUS_ADDR_WIDTH-1:0]                   mst_awaddr_i    ,
    input  logic [ 7:0]                                 mst_awlen_i     ,
    input  logic [BUS_SIZE_WIDTH-1:0]                   mst_awsize_i    ,
    input  logic [ 1:0]                                 mst_awburst_i   ,
    input  logic                                        mst_awlock_i    ,
    input  logic [ 3:0]                                 mst_awcache_i   ,
    input  logic [ 2:0]                                 mst_awprot_i    ,
    input  logic [ 3:0]                                 mst_awregion_i  ,
    input  logic [BUS_USER_WIDTH-1:0]                   mst_awuser_i    ,
    input  logic [ 3:0]                                 mst_awqos_i     ,
    input  logic                                        mst_awvalid_i   ,
    input  logic                                        mst_awready_i   ,
    input  logic [BUS_DATA_WIDTH-1:0]                   mst_wdata_i     ,
    input  logic [BUS_STRB_WIDTH-1:0]                   mst_wstrb_i     ,
    input  logic                                        mst_wlast_i     ,
    input  logic [BUS_USER_WIDTH-1:0]                   mst_wuser_i     ,
    input  logic                                        mst_wvalid_i    ,
    input  logic                                        mst_wready_i    ,
    input  logic [BUS_ID_WIDTH-1:0]                     mst_bid_i       ,
    input  logic [ 1:0]                                 mst_bresp_i     ,
    input  logic                                        mst_bvalid_i    ,
    input  logic [BUS_USER_WIDTH-1:0]                   mst_buser_i     ,
    input  logic                                        mst_bready_i    ,
    input  logic [BUS_ID_WIDTH-1:0]                     mst_arid_i      ,
    input  logic [BUS_ADDR_WIDTH-1:0]                   mst_araddr_i    ,
    input  logic [ 7:0]                                 mst_arlen_i     ,
    input  logic [BUS_SIZE_WIDTH-1:0]                   mst_arsize_i    ,
    input  logic [ 1:0]                                 mst_arburst_i   ,
    input  logic                                        mst_arlock_i    ,
    input  logic [ 3:0]                                 mst_arcache_i   ,
    input  logic [ 2:0]                                 mst_arprot_i    ,
    input  logic [ 3:0]                                 mst_arregion_i  ,
    input  logic [ BUS_USER_WIDTH-1:0]                  mst_aruser_i    ,
    input  logic [ 3:0]                                 mst_arqos_i     ,
    input  logic                                        mst_arvalid_i   ,
    input  logic                                        mst_arready_i   ,
    input  logic [BUS_ID_WIDTH-1:0]                     mst_rid_i       ,
    input  logic [BUS_DATA_WIDTH-1:0]                   mst_rdata_i     ,
    input  logic [ 1:0]                                 mst_rresp_i     ,
    input  logic                                        mst_rlast_i     ,
    input  logic [BUS_USER_WIDTH-1:0]                   mst_ruser_i     ,
    input  logic                                        mst_rvalid_i    ,
    input  logic                                        mst_rready_i    ,

    input  logic                                        penable_i,
    input  logic                                        pwrite_i ,
    input  logic [31:0]                                 paddr_i  ,
    input  logic                                        psel_i   ,
    input  logic [31:0]                                 pwdata_i ,
    output logic [31:0]                                 prdata_o ,
    output logic                                        pready_o ,
    output logic                                        pslverr_o,

    input  logic                                        spare_i
);

typedef struct packed {
    logic [31:0]                                    cnt;
    logic [23:0]                                    device_id_i         ;
    logic [19:0]                                    process_id_i        ;
    logic                                           process_id_valid_i  ;
    logic                                           is_translated_i     ;
    logic [BUS_ID_WIDTH-1:0]                        id_i                ;
    logic [BUS_ADDR_WIDTH-1:0]                      addr_i              ;
    logic [ 7:0]                                    len_i               ;
    logic [BUS_SIZE_WIDTH-1:0]                      size_i              ;
    logic [ 1:0]                                    burst_i             ;
    logic                                           lock_i              ;
    logic [ 3:0]                                    cache_i             ;
    logic [ 2:0]                                    prot_i              ;
    logic [ 3:0]                                    region_i            ;
    logic [BUS_USER_WIDTH-1:0]                      user_i              ;
    logic [ 3:0]                                    qos_i               ;
    logic [31:0]                                    timer               ;
} awr_t;
typedef struct packed {
    logic [31:0]                                    cnt;
    logic [BUS_DATA_WIDTH-1:0]                      wdata_i     ;
    logic [BUS_STRB_WIDTH-1:0]                      wstrb_i     ;
    logic                                           wlast_i     ;
    logic [BUS_USER_WIDTH-1:0]                      wuser_i     ;
    logic [31:0]                                    timer       ;
} w_t;
typedef struct packed {
    logic [31:0]                                    cnt;
    logic [BUS_ID_WIDTH-1:0]                        bid_i       ;
    logic [ 1:0]                                    bresp_i     ;
    logic [BUS_USER_WIDTH-1:0]                      buser_i     ;
    logic [31:0]                                    timer       ;
} b_t;
typedef struct packed {
    logic [31:0]                                    cnt;
    logic [BUS_ID_WIDTH-1:0]                        rid_i       ;
    logic [BUS_DATA_WIDTH-1:0]                      rdata_i     ;
    logic [ 1:0]                                    rresp_i     ;
    logic                                           rlast_i     ;
    logic [BUS_USER_WIDTH-1:0]                      ruser_i     ;
    logic [31:0]                                    timer       ;
} r_t;

//assign iommu_test_pready        = 'd0;
assign pslverr_o='d0;

// addr: 'h0
// [0]      :   dostart, 1: enable axi-info-store, 0:disable axi-info-store
// [5:1]    :   mode
//              0: just follow dostart
//              1: slv_aw_cnt = mode_value
//              2: slv_ar_cnt = mode_value
//              3: slv_w_cnt  = mode_value
//              4: slv_b_cnt  = mode_value
//              5: slv_r_cnt  = mode_value
//              6: slv_aw_addr= mode_value
//              7: slv_ar_addr= mode_value
//              11:mst_aw_cnt = mode_value
//              12:mst_ar_cnt = mode_value
//              13:mst_w_cnt  = mode_value
//              14:mst_b_cnt  = mode_value
//              15:mst_r_cnt  = mode_value
//              16:mst_aw_addr= mode_value
//              17:mst_ar_addr= mode_value
// [31:6]   :   reserved
logic       dostart;
logic [5:1] mode, mode_ff;
logic       mode_change;
logic [5:1] srelog;
logic [15:11] mrelog;
logic [23:20] errrelog;
logic [3:0]   errflag;
always@(posedge clk or negedge rstn) begin
    if(~rstn) begin
        dostart <= 1'b0;
        mode    <= 'd0;
        srelog  <= 'd0;
        mrelog  <= 'd0;
        mode_ff <= 'd0;
    end
    else begin
        mode_ff <= mode;
        srelog  <= 'd0;
        mrelog  <= 'd0;
        errrelog<= 'd0;
        if(penable_i & pwrite_i & psel_i & paddr_i[11:0]=='d0) begin
            dostart <= pwdata_i[0];
            mode    <= pwdata_i[31:27];
            srelog  <= pwdata_i[5:1];
            mrelog  <= pwdata_i[15:11];
            errrelog<= pwdata_i[23:20];
        end
        else begin
            // NA
        end
    end
end
assign mode_change = (mode != mode_ff);

// addr: 'h4
// [31:0]      :   mode_value
logic [31:0] mode_value;
always@(posedge clk or negedge rstn) begin
    if(~rstn) begin
        mode_value <= 'd0;
    end
    else begin
        if(penable_i & pwrite_i & psel_i & paddr_i[11:0]=='h4) begin
            mode_value <= pwdata_i;
        end
        else begin
            // NA
        end
    end
end

// addr: 'hc
// [31:0]      :   mode_value1
logic [31:0] mode_value1;
always@(posedge clk or negedge rstn) begin
    if(~rstn) begin
        mode_value1 <= 'd0;
    end
    else begin
        if(penable_i & pwrite_i & psel_i & paddr_i[11:0]=='hc) begin
            mode_value1 <= pwdata_i;
        end
        else begin
            // NA
        end
    end
end


// addr: 'h8
// [3:0]        :   rd_shiftv, select the bit slice read to prdata
// [7:4]        :   rd_shifth, select the RAM addr bank to read
// [15:8]       :   rd_sel, select which ram to read
//                      1: SLV_AW_RAM
//                      2: SLV_AR_RAM
//                      3: SLV_W_RAM
//                      4: SLV_B_RAM
//                      5: SLV_R_RAM
//                      11:MST_AW_RAM
//                      12:MST_AR_RAM
//                      13:MST_W_RAM
//                      14:MST_B_RAM
//                      15:MST_R_RAM
// [31:16]      :   reserved
logic [3:0] rd_shiftv;
logic [3:0] rd_shifth;
logic [7:0] rd_sel;
always@(posedge clk or negedge rstn) begin
    if(~rstn) begin
        rd_shiftv <= 'd0;
        rd_shifth <= 'd0;
        rd_sel    <= 'd0;
    end
    else begin
        if(penable_i & pwrite_i & psel_i & paddr_i[11:0]=='h8) begin
            rd_shiftv <= pwdata_i[3:0];
            rd_shifth <= pwdata_i[7:4];
            rd_sel    <= pwdata_i[15:8];
        end
        else begin
            // NA
        end
    end
end

// addr:    'h100 + ?
// [31:0]   : read data form ram selected by rd_sel


//{{{
logic [31:0] timer;
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        timer <= 'd0;
    else if(dostart)
        timer <= timer+'d1;
end

logic [31:0] slv_aw_cnt, slv_ar_cnt, slv_w_cnt, slv_b_cnt, slv_r_cnt;
logic [31:0] mst_aw_cnt, mst_ar_cnt, mst_w_cnt, mst_b_cnt, mst_r_cnt;
always@(posedge clk or negedge rstn) begin
    if(~rstn) begin
        slv_aw_cnt  <= 'd0;
        slv_ar_cnt  <= 'd0;
        slv_w_cnt   <= 'd0;
        slv_b_cnt   <= 'd0;
        slv_r_cnt   <= 'd0;
        mst_aw_cnt  <= 'd0;
        mst_ar_cnt  <= 'd0;
        mst_w_cnt   <= 'd0;
        mst_b_cnt   <= 'd0;
        mst_r_cnt   <= 'd0;
    end
    else begin
        if(slv_awvalid_i& slv_awready_i)                slv_aw_cnt <= slv_aw_cnt + 'd1;
        if(slv_arvalid_i& slv_arready_i)                slv_ar_cnt <= slv_ar_cnt + 'd1;
        if(slv_wvalid_i & slv_wready_i  & slv_wlast_i)  slv_w_cnt  <= slv_w_cnt  + 'd1;
        if(slv_bvalid_i & slv_bready_i)                 slv_b_cnt  <= slv_b_cnt  + 'd1;
        if(slv_rvalid_i & slv_rready_i  & slv_rlast_i)  slv_r_cnt  <= slv_r_cnt  + 'd1;
        if(mst_awvalid_i& mst_awready_i)                mst_aw_cnt <= mst_aw_cnt + 'd1;
        if(mst_arvalid_i& mst_arready_i)                mst_ar_cnt <= mst_ar_cnt + 'd1;
        if(mst_wvalid_i & mst_wready_i  & mst_wlast_i)  mst_w_cnt  <= mst_w_cnt  + 'd1;
        if(mst_bvalid_i & mst_bready_i)                 mst_b_cnt  <= mst_b_cnt  + 'd1;
        if(mst_rvalid_i & mst_rready_i  & mst_rlast_i)  mst_r_cnt  <= mst_r_cnt  + 'd1;
    end
end

logic enable;
//always@(posedge clk or negedge rstn) begin
//    if(~rstn)
//        enable <= 1'b0;
//    else begin
//        if(dostart) begin
//            case(mode)
//            'd0:    begin
//                    enable <= 1'b1;
//            end
//            'd1:    begin
//                    enable <= 1'b1;
//            end
//            'd6:    begin
//                    enable <= 1'b1;
//            end
//            'd16:    begin
//                    enable <= 1'b1;
//            end
//            default:enable <= 1'b1;
//            endcase
//        end
//        else begin
//            enable <= 1'b0;
//        end
//    end
//end
assign enable = dostart & (mode=='d0 | mode=='d1 | mode=='d6 | mode=='d16);
//}}}

// AWRAM {{{
logic slv_aw_ram_wr;
logic slv_aw_ram_rd;
awr_t slv_aw_ram_din;
awr_t slv_aw_ram_dout;
logic [AWRAM_IDX_WIDTH  :0] slv_aw_ram_addra;
logic [AWRAM_IDX_WIDTH-1:0] slv_aw_ram_addrb;

logic slv_aw_ram_wr_enable;
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        slv_aw_ram_wr_enable <= 'd0;
    else if(mode_change)
        slv_aw_ram_wr_enable <= 'd0;
    else if(mode=='d0)
        slv_aw_ram_wr_enable <= 'd1;
    else if((mode=='d1) & (slv_awvalid_i & slv_awready_i & slv_aw_cnt==mode_value))
        slv_aw_ram_wr_enable <= 'd1;
    else if((mode=='d6) & (slv_awvalid_i & slv_awready_i & slv_awaddr_i[31:0]==mode_value))
        slv_aw_ram_wr_enable <= 'd1;
    else if((mode=='d16)& (mst_awvalid_i & mst_awready_i & mst_awaddr_i[31:0]==mode_value))
        slv_aw_ram_wr_enable <= 'd1;
end

assign slv_aw_ram_wr = enable & slv_awvalid_i & slv_awready_i & (slv_aw_ram_addra < 2**AWRAM_IDX_WIDTH) &
                        (slv_aw_ram_wr_enable | (
                                                    ((mode=='d6) & (slv_awvalid_i & slv_awready_i & slv_awaddr_i[31:0]==mode_value)) |
                                                    ((mode=='d1) & (slv_awvalid_i & slv_awready_i & slv_aw_cnt  ==mode_value)) |
                                                    ((mode=='d16)& (mst_awvalid_i & mst_awready_i & mst_awaddr_i[31:0]==mode_value))
                                                    ));
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        slv_aw_ram_addra <= 'd0;
    else if(srelog[1])
        slv_aw_ram_addra <= 'd0;
    else if(slv_aw_ram_wr)
        slv_aw_ram_addra <= slv_aw_ram_addra + 'd1;
end
assign slv_aw_ram_din.cnt                   = slv_aw_cnt;
assign slv_aw_ram_din.device_id_i           = slv_aw_device_id_i        ;
assign slv_aw_ram_din.process_id_i          = slv_aw_process_id_i       ;
assign slv_aw_ram_din.process_id_valid_i    = slv_aw_process_id_valid_i ;
assign slv_aw_ram_din.is_translated_i       = slv_aw_is_translated_i    ;
assign slv_aw_ram_din.id_i                  = slv_awid_i                ;
assign slv_aw_ram_din.addr_i                = slv_awaddr_i              ;
assign slv_aw_ram_din.len_i                 = slv_awlen_i               ;
assign slv_aw_ram_din.size_i                = slv_awsize_i              ;
assign slv_aw_ram_din.burst_i               = slv_awburst_i             ;
assign slv_aw_ram_din.lock_i                = slv_awlock_i              ;
assign slv_aw_ram_din.cache_i               = slv_awcache_i             ;
assign slv_aw_ram_din.prot_i                = slv_awprot_i              ;
assign slv_aw_ram_din.region_i              = slv_awregion_i            ;
assign slv_aw_ram_din.user_i                = slv_awuser_i              ;
assign slv_aw_ram_din.qos_i                 = slv_awqos_i               ;
assign slv_aw_ram_din.timer                 = timer                     ;
assign slv_aw_ram_rd    = psel_i & ~pwrite_i & rd_sel=='d1 & paddr_i[11:0]>='h100;
logic  [15:0] slv_aw_ram_addrb_raw;
assign slv_aw_ram_addrb_raw = {rd_shifth, paddr_i[11:0]-'h100} >> 2;
assign slv_aw_ram_addrb = slv_aw_ram_addrb_raw[AWRAM_IDX_WIDTH-1:0];
    iommu_acd_sdpram_model #(
    /*parameter int  unsigned */ .AWIDTH    (AWRAM_IDX_WIDTH        ), // = 4,
    /*parameter int  unsigned */ .DWIDTH    ($bits(slv_aw_ram_din)  )  // = 128
    ) U_slv_aw_ram (
    /*input  logic                      */  .clk    (clk                ),
    /*input  logic                      */  .wea    (~slv_aw_ram_wr     ),
    /*input  logic                      */  .mea    (~slv_aw_ram_wr     ),
    /*input  logic [AWIDTH-1:0]         */  .addra  ( slv_aw_ram_addra[AWRAM_IDX_WIDTH-1:0]  ),
    /*input  logic [DWIDTH-1:0]         */  .dina   ( slv_aw_ram_din    ),
    /*input  logic                      */  .meb    (~slv_aw_ram_rd     ),
    /*input  logic [AWIDTH-1:0]         */  .addrb  ( slv_aw_ram_addrb  ),
    /*output logic [DWIDTH-1:0]         */  .doutb  ( slv_aw_ram_dout   )
    );



logic mst_aw_ram_wr;
logic mst_aw_ram_rd;
awr_t mst_aw_ram_din;
awr_t mst_aw_ram_dout;
logic [AWRAM_IDX_WIDTH  :0] mst_aw_ram_addra;
logic [AWRAM_IDX_WIDTH-1:0] mst_aw_ram_addrb;

logic mst_aw_ram_wr_enable;
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        mst_aw_ram_wr_enable <= 'd0;
    else if(mode_change)
        mst_aw_ram_wr_enable <= 'd0;
    else if(mode=='d0)
        mst_aw_ram_wr_enable <= 'd1;
    else if((mode=='d1) & (mst_awvalid_i & mst_awready_i & mst_aw_cnt==mode_value))
        mst_aw_ram_wr_enable <= 'd1;
    else if((mode=='d6) & (slv_awvalid_i & slv_awready_i & slv_awaddr_i[31:0]==mode_value))
        mst_aw_ram_wr_enable <= 'd1;
    else if((mode=='d16)& (mst_awvalid_i & mst_awready_i & mst_awaddr_i[31:0]==mode_value))
        mst_aw_ram_wr_enable <= 'd1;
end

assign mst_aw_ram_wr = enable & mst_awvalid_i & mst_awready_i & (mst_aw_ram_addra < 2**AWRAM_IDX_WIDTH) &
                        (mst_aw_ram_wr_enable | (
                                                    ((mode=='d6) & (slv_awvalid_i & slv_awready_i & slv_awaddr_i[31:0]==mode_value)) |
                                                    ((mode=='d1) & (mst_awvalid_i & mst_awready_i & mst_aw_cnt  ==mode_value)) |
                                                    ((mode=='d16)& (mst_awvalid_i & mst_awready_i & mst_awaddr_i[31:0]==mode_value))
                                                    ));
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        mst_aw_ram_addra <= 'd0;
    else if(mrelog[11])
        mst_aw_ram_addra <= 'd0;
    else if(mst_aw_ram_wr)
        mst_aw_ram_addra <= mst_aw_ram_addra + 'd1;
end
assign mst_aw_ram_din.cnt                   = mst_aw_cnt;
assign mst_aw_ram_din.device_id_i           = 'd0; //mst_aw_device_id_i           ;
assign mst_aw_ram_din.process_id_i          = 'd0; //mst_aw_process_id_i          ;
assign mst_aw_ram_din.process_id_valid_i    = 'd0; //mst_aw_process_id_valid_i    ;
assign mst_aw_ram_din.is_translated_i       = 'd0; //mst_aw_is_translated_i       ;
assign mst_aw_ram_din.id_i                  = mst_awid_i                        ;
assign mst_aw_ram_din.addr_i                = mst_awaddr_i                      ;
assign mst_aw_ram_din.len_i                 = mst_awlen_i                       ;
assign mst_aw_ram_din.size_i                = mst_awsize_i                      ;
assign mst_aw_ram_din.burst_i               = mst_awburst_i                     ;
assign mst_aw_ram_din.lock_i                = mst_awlock_i                      ;
assign mst_aw_ram_din.cache_i               = mst_awcache_i                     ;
assign mst_aw_ram_din.prot_i                = mst_awprot_i                      ;
assign mst_aw_ram_din.region_i              = mst_awregion_i                    ;
assign mst_aw_ram_din.user_i                = mst_awuser_i                      ;
assign mst_aw_ram_din.qos_i                 = mst_awqos_i                       ;
assign mst_aw_ram_din.timer                 = timer                             ;
assign mst_aw_ram_rd    = psel_i & ~pwrite_i & rd_sel=='d11 & paddr_i[11:0]>='h100;
logic [15:0] mst_aw_ram_addrb_raw;
assign mst_aw_ram_addrb_raw = {rd_shifth, paddr_i[11:0]-'h100} >> 2;
assign mst_aw_ram_addrb = mst_aw_ram_addrb_raw[AWRAM_IDX_WIDTH-1:0];
    iommu_acd_sdpram_model #(
    /*parameter int  unsigned */ .AWIDTH    (AWRAM_IDX_WIDTH        ), // = 4,
    /*parameter int  unsigned */ .DWIDTH    ($bits(mst_aw_ram_din)  )  // = 128
    ) U_mst_aw_ram (
    /*input  logic                      */  .clk    (clk                ),
    /*input  logic                      */  .wea    (~mst_aw_ram_wr     ),
    /*input  logic                      */  .mea    (~mst_aw_ram_wr     ),
    /*input  logic [AWIDTH-1:0]         */  .addra  ( mst_aw_ram_addra[AWRAM_IDX_WIDTH-1:0]  ),
    /*input  logic [DWIDTH-1:0]         */  .dina   ( mst_aw_ram_din    ),
    /*input  logic                      */  .meb    (~mst_aw_ram_rd     ),
    /*input  logic [AWIDTH-1:0]         */  .addrb  ( mst_aw_ram_addrb  ),
    /*output logic [DWIDTH-1:0]         */  .doutb  ( mst_aw_ram_dout   )
    );

//}}}

// ARRAM {{{
logic slv_ar_ram_wr;
logic slv_ar_ram_rd;
awr_t slv_ar_ram_din;
awr_t slv_ar_ram_dout;
logic [AWRAM_IDX_WIDTH  :0] slv_ar_ram_addra;
logic [AWRAM_IDX_WIDTH-1:0] slv_ar_ram_addrb;

logic slv_ar_ram_wr_enable;
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        slv_ar_ram_wr_enable <= 'd0;
    else if(mode_change)
        slv_ar_ram_wr_enable <= 'd0;
    else if(mode=='d0)
        slv_ar_ram_wr_enable <= 'd1;
    else if((mode=='d1) & (slv_arvalid_i & slv_arready_i & slv_ar_cnt==mode_value1))
        slv_ar_ram_wr_enable <= 'd1;
    else if((mode=='d6) & (slv_arvalid_i & slv_arready_i & slv_araddr_i[31:0]==mode_value1))
        slv_ar_ram_wr_enable <= 'd1;
    else if((mode=='d16)& (mst_arvalid_i & mst_arready_i & mst_araddr_i[31:0]==mode_value1))
        slv_ar_ram_wr_enable <= 'd1;
end

assign slv_ar_ram_wr = enable & slv_arvalid_i & slv_arready_i & (slv_ar_ram_addra < 2**AWRAM_IDX_WIDTH) &
                        (slv_ar_ram_wr_enable | (
                                                    ((mode=='d6) & (slv_arvalid_i & slv_arready_i & slv_araddr_i[31:0]==mode_value1)) |
                                                    ((mode=='d1) & (slv_arvalid_i & slv_arready_i & slv_ar_cnt  ==mode_value1)) |
                                                    ((mode=='d16)& (mst_arvalid_i & mst_arready_i & mst_araddr_i[31:0]==mode_value1))
                                                    ));
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        slv_ar_ram_addra <= 'd0;
    else if(srelog[2])
        slv_ar_ram_addra <= 'd0;
    else if(slv_ar_ram_wr)
        slv_ar_ram_addra <= slv_ar_ram_addra + 'd1;
end
assign slv_ar_ram_din.cnt                   = slv_ar_cnt;
assign slv_ar_ram_din.device_id_i           = slv_ar_device_id_i            ;
assign slv_ar_ram_din.process_id_i          = slv_ar_process_id_i           ;
assign slv_ar_ram_din.process_id_valid_i    = slv_ar_process_id_valid_i     ;
assign slv_ar_ram_din.is_translated_i       = slv_ar_is_translated_i        ;
assign slv_ar_ram_din.id_i                  = slv_arid_i                    ;
assign slv_ar_ram_din.addr_i                = slv_araddr_i                  ;
assign slv_ar_ram_din.len_i                 = slv_arlen_i                   ;
assign slv_ar_ram_din.size_i                = slv_arsize_i                  ;
assign slv_ar_ram_din.burst_i               = slv_arburst_i                 ;
assign slv_ar_ram_din.lock_i                = slv_arlock_i                  ;
assign slv_ar_ram_din.cache_i               = slv_arcache_i                 ;
assign slv_ar_ram_din.prot_i                = slv_arprot_i                  ;
assign slv_ar_ram_din.region_i              = slv_arregion_i                ;
assign slv_ar_ram_din.user_i                = slv_aruser_i                  ;
assign slv_ar_ram_din.qos_i                 = slv_arqos_i                   ;
assign slv_ar_ram_din.timer                 = timer                         ;
assign slv_ar_ram_rd    = psel_i & ~pwrite_i & rd_sel=='d2 & paddr_i[11:0]>='h100;
logic [15:0] slv_ar_ram_addrb_raw;
assign slv_ar_ram_addrb_raw = {rd_shifth, paddr_i[11:0]-'h100} >> 2;
assign slv_ar_ram_addrb = slv_ar_ram_addrb_raw[ARRAM_IDX_WIDTH-1:0];
    iommu_acd_sdpram_model #(
    /*parameter int  unsigned */ .AWIDTH    (ARRAM_IDX_WIDTH        ), // = 4,
    /*parameter int  unsigned */ .DWIDTH    ($bits(slv_aw_ram_din)  )  // = 128
    ) U_slv_ar_ram (
    /*input  logic                      */  .clk    (clk                ),
    /*input  logic                      */  .wea    (~slv_ar_ram_wr     ),
    /*input  logic                      */  .mea    (~slv_ar_ram_wr     ),
    /*input  logic [AWIDTH-1:0]         */  .addra  ( slv_ar_ram_addra[ARRAM_IDX_WIDTH-1:0]  ),
    /*input  logic [DWIDTH-1:0]         */  .dina   ( slv_ar_ram_din    ),
    /*input  logic                      */  .meb    (~slv_ar_ram_rd     ),
    /*input  logic [AWIDTH-1:0]         */  .addrb  ( slv_ar_ram_addrb  ),
    /*output logic [DWIDTH-1:0]         */  .doutb  ( slv_ar_ram_dout   )
    );

logic mst_ar_ram_wr;
logic mst_ar_ram_rd;
awr_t mst_ar_ram_din;
awr_t mst_ar_ram_dout;
logic [AWRAM_IDX_WIDTH  :0] mst_ar_ram_addra;
logic [AWRAM_IDX_WIDTH-1:0] mst_ar_ram_addrb;

logic mst_ar_ram_wr_enable;
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        mst_ar_ram_wr_enable <= 'd0;
    else if(mode_change)
        mst_ar_ram_wr_enable <= 'd0;
    else if(mode=='d0)
        mst_ar_ram_wr_enable <= 'd1;
    else if((mode=='d1) & (mst_arvalid_i & mst_arready_i & mst_ar_cnt==mode_value1))
        mst_ar_ram_wr_enable <= 'd1;
    else if((mode=='d6) & (slv_arvalid_i & slv_arready_i & slv_araddr_i[31:0]==mode_value1))
        mst_ar_ram_wr_enable <= 'd1;
    else if((mode=='d16)& (mst_arvalid_i & mst_arready_i & mst_araddr_i[31:0]==mode_value1))
        mst_ar_ram_wr_enable <= 'd1;
end

assign mst_ar_ram_wr = enable & mst_arvalid_i & mst_arready_i & (mst_ar_ram_addra < 2**AWRAM_IDX_WIDTH) &
                        (mst_ar_ram_wr_enable | (
                                                    ((mode=='d6) & (slv_arvalid_i & slv_arready_i & slv_araddr_i[31:0]==mode_value1)) |
                                                    ((mode=='d1) & (mst_arvalid_i & mst_arready_i & mst_ar_cnt  ==mode_value1)) |
                                                    ((mode=='d16)& (mst_arvalid_i & mst_arready_i & mst_araddr_i[31:0]==mode_value1))
                                                    ));
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        mst_ar_ram_addra <= 'd0;
    else if(mrelog[12])
        mst_ar_ram_addra <= 'd0;
    else if(mst_ar_ram_wr)
        mst_ar_ram_addra <= mst_ar_ram_addra + 'd1;
end
assign mst_ar_ram_din.cnt                   = mst_ar_cnt;
assign mst_ar_ram_din.device_id_i           = 'd0; //mst_ar_device_id_i           ;
assign mst_ar_ram_din.process_id_i          = 'd0; //mst_ar_process_id_i          ;
assign mst_ar_ram_din.process_id_valid_i    = 'd0; //mst_ar_process_id_valid_i    ;
assign mst_ar_ram_din.is_translated_i       = 'd0; //mst_ar_is_translated_i       ;
assign mst_ar_ram_din.id_i                  = mst_arid_i                        ;
assign mst_ar_ram_din.addr_i                = mst_araddr_i                      ;
assign mst_ar_ram_din.len_i                 = mst_arlen_i                       ;
assign mst_ar_ram_din.size_i                = mst_arsize_i                      ;
assign mst_ar_ram_din.burst_i               = mst_arburst_i                     ;
assign mst_ar_ram_din.lock_i                = mst_arlock_i                      ;
assign mst_ar_ram_din.cache_i               = mst_arcache_i                     ;
assign mst_ar_ram_din.prot_i                = mst_arprot_i                      ;
assign mst_ar_ram_din.region_i              = mst_arregion_i                    ;
assign mst_ar_ram_din.user_i                = mst_aruser_i                      ;
assign mst_ar_ram_din.qos_i                 = mst_arqos_i                       ;
assign mst_ar_ram_din.timer                 = timer                             ;
assign mst_ar_ram_rd    = psel_i & ~pwrite_i & rd_sel=='d12 & paddr_i[11:0]>='h100;
logic [15:0] mst_ar_ram_addrb_raw;
assign mst_ar_ram_addrb_raw = {rd_shifth, paddr_i[11:0]-'h100} >> 2;
assign mst_ar_ram_addrb = mst_ar_ram_addrb_raw[ARRAM_IDX_WIDTH-1:0];
    iommu_acd_sdpram_model #(
    /*parameter int  unsigned */ .AWIDTH    (ARRAM_IDX_WIDTH         ), // = 4,
    /*parameter int  unsigned */ .DWIDTH    ($bits(mst_aw_ram_din)  )  // = 128
    ) U_mst_ar_ram (
    /*input  logic                      */  .clk    (clk                ),
    /*input  logic                      */  .wea    (~mst_ar_ram_wr     ),
    /*input  logic                      */  .mea    (~mst_ar_ram_wr     ),
    /*input  logic [AWIDTH-1:0]         */  .addra  ( mst_ar_ram_addra[ARRAM_IDX_WIDTH-1:0]  ),
    /*input  logic [DWIDTH-1:0]         */  .dina   ( mst_ar_ram_din    ),
    /*input  logic                      */  .meb    (~mst_ar_ram_rd     ),
    /*input  logic [AWIDTH-1:0]         */  .addrb  ( mst_ar_ram_addrb  ),
    /*output logic [DWIDTH-1:0]         */  .doutb  ( mst_ar_ram_dout   )
    );

//}}}

// WRAM {{{
logic slv_w_ram_wr;
logic slv_w_ram_rd;
w_t slv_w_ram_din;
w_t slv_w_ram_dout;
logic [WRAM_IDX_WIDTH  :0] slv_w_ram_addra;
logic [WRAM_IDX_WIDTH-1:0] slv_w_ram_addrb;

logic slv_w_ram_wr_enable;
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        slv_w_ram_wr_enable <= 'd0;
    else if(mode_change)
        slv_w_ram_wr_enable <= 'd0;
    else if(mode=='d0)
        slv_w_ram_wr_enable <= 'd1;
    else if((mode=='d1) & (slv_wvalid_i & slv_wready_i & slv_w_cnt==mode_value))
        slv_w_ram_wr_enable <= 'd1;
    else if((mode=='d6) & (slv_awvalid_i & slv_awready_i & slv_awaddr_i[31:0]==mode_value))
        slv_w_ram_wr_enable <= 'd1;
    else if((mode=='d16)& (mst_awvalid_i & mst_awready_i & mst_awaddr_i[31:0]==mode_value))
        slv_w_ram_wr_enable <= 'd1;
end

assign slv_w_ram_wr = enable & slv_wvalid_i & slv_wready_i & (slv_w_ram_addra < 2**WRAM_IDX_WIDTH) &
                        (slv_w_ram_wr_enable | (
                                                    ((mode=='d6) & (slv_awvalid_i & slv_awready_i & slv_awaddr_i[31:0]==mode_value)) |
                                                    ((mode=='d1) & (slv_wvalid_i  & slv_wready_i  & slv_w_cnt   ==mode_value)) |
                                                    ((mode=='d16)& (mst_awvalid_i & mst_awready_i & mst_awaddr_i[31:0]==mode_value))
                                                    ));

always@(posedge clk or negedge rstn) begin
    if(~rstn)
        slv_w_ram_addra <= 'd0;
    else if(srelog[3])
        slv_w_ram_addra <= 'd0;
    else if(slv_w_ram_wr)
        slv_w_ram_addra <= slv_w_ram_addra + 'd1;
end
assign slv_w_ram_din.cnt        = slv_w_cnt;
assign slv_w_ram_din.wdata_i    = slv_wdata_i;
assign slv_w_ram_din.wstrb_i    = slv_wstrb_i;
assign slv_w_ram_din.wlast_i    = slv_wlast_i;
assign slv_w_ram_din.wuser_i    = slv_wuser_i;
assign slv_w_ram_din.timer      = timer     ;
assign slv_w_ram_rd    = psel_i & ~pwrite_i & rd_sel=='d3 & paddr_i[11:0]>='h100;
logic [15:0] slv_w_ram_addrb_raw;
assign slv_w_ram_addrb_raw = {rd_shifth, paddr_i[11:0]-'h100} >> 2;
assign slv_w_ram_addrb = slv_w_ram_addrb_raw[WRAM_IDX_WIDTH-1:0];
    iommu_acd_sdpram_model #(
    /*parameter int  unsigned */ .AWIDTH    (WRAM_IDX_WIDTH         ), // = 4,
    /*parameter int  unsigned */ .DWIDTH    ($bits(slv_w_ram_din)   )  // = 128
    ) U_slv_w_ram (
    /*input  logic                      */  .clk    (clk                ),
    /*input  logic                      */  .wea    (~slv_w_ram_wr      ),
    /*input  logic                      */  .mea    (~slv_w_ram_wr      ),
    /*input  logic [AWIDTH-1:0]         */  .addra  ( slv_w_ram_addra[WRAM_IDX_WIDTH-1:0]   ),
    /*input  logic [DWIDTH-1:0]         */  .dina   ( slv_w_ram_din     ),
    /*input  logic                      */  .meb    (~slv_w_ram_rd      ),
    /*input  logic [AWIDTH-1:0]         */  .addrb  ( slv_w_ram_addrb   ),
    /*output logic [DWIDTH-1:0]         */  .doutb  ( slv_w_ram_dout    )
    );

logic mst_w_ram_wr;
logic mst_w_ram_rd;
w_t mst_w_ram_din;
w_t mst_w_ram_dout;
logic [WRAM_IDX_WIDTH  :0] mst_w_ram_addra;
logic [WRAM_IDX_WIDTH-1:0] mst_w_ram_addrb;

logic mst_w_ram_wr_enable;
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        mst_w_ram_wr_enable <= 'd0;
    else if(mode_change)
        mst_w_ram_wr_enable <= 'd0;
    else if(mode=='d0)
        mst_w_ram_wr_enable <= 'd1;
    else if((mode=='d1) & (mst_wvalid_i & mst_wready_i & mst_w_cnt==mode_value))
        mst_w_ram_wr_enable <= 'd1;
    else if((mode=='d6) & (slv_awvalid_i & slv_awready_i & slv_awaddr_i[31:0]==mode_value))
        mst_w_ram_wr_enable <= 'd1;
    else if((mode=='d16)& (mst_awvalid_i & mst_awready_i & mst_awaddr_i[31:0]==mode_value))
        mst_w_ram_wr_enable <= 'd1;
end

assign mst_w_ram_wr = enable & mst_wvalid_i & mst_wready_i & (mst_w_ram_addra < 2**WRAM_IDX_WIDTH) &
                        (mst_w_ram_wr_enable | (
                                                    ((mode=='d6) & (slv_awvalid_i & slv_awready_i & slv_awaddr_i[31:0]==mode_value)) |
                                                    ((mode=='d1) & (mst_wvalid_i  & mst_wready_i  & mst_w_cnt   ==mode_value)) |
                                                    ((mode=='d16)& (mst_awvalid_i & mst_awready_i & mst_awaddr_i[31:0]==mode_value))
                                                    ));

always@(posedge clk or negedge rstn) begin
    if(~rstn)
        mst_w_ram_addra <= 'd0;
    else if(mrelog[13])
        mst_w_ram_addra <= 'd0;
    else if(mst_w_ram_wr)
        mst_w_ram_addra <= mst_w_ram_addra + 'd1;
end
assign mst_w_ram_din.cnt        = mst_w_cnt;
assign mst_w_ram_din.wdata_i    = mst_wdata_i;
assign mst_w_ram_din.wstrb_i    = mst_wstrb_i;
assign mst_w_ram_din.wlast_i    = mst_wlast_i;
assign mst_w_ram_din.wuser_i    = mst_wuser_i;
assign mst_w_ram_din.timer      = timer;
assign mst_w_ram_rd    = psel_i & ~pwrite_i & rd_sel=='d13 & paddr_i[11:0]>='h100;
logic [15:0] mst_w_ram_addrb_raw;
assign mst_w_ram_addrb_raw = {rd_shifth, paddr_i[11:0]-'h100} >> 2;
assign mst_w_ram_addrb = mst_w_ram_addrb_raw[WRAM_IDX_WIDTH-1:0];
    iommu_acd_sdpram_model #(
    /*parameter int  unsigned */ .AWIDTH    (WRAM_IDX_WIDTH         ), // = 4,
    /*parameter int  unsigned */ .DWIDTH    ($bits(mst_w_ram_din)   )  // = 128
    ) U_mst_w_ram (
    /*input  logic                      */  .clk    (clk                ),
    /*input  logic                      */  .wea    (~mst_w_ram_wr      ),
    /*input  logic                      */  .mea    (~mst_w_ram_wr      ),
    /*input  logic [AWIDTH-1:0]         */  .addra  ( mst_w_ram_addra[WRAM_IDX_WIDTH-1:0]   ),
    /*input  logic [DWIDTH-1:0]         */  .dina   ( mst_w_ram_din     ),
    /*input  logic                      */  .meb    (~mst_w_ram_rd      ),
    /*input  logic [AWIDTH-1:0]         */  .addrb  ( mst_w_ram_addrb   ),
    /*output logic [DWIDTH-1:0]         */  .doutb  ( mst_w_ram_dout    )
    );

//}}}

// BRAM {{{
logic slv_b_ram_wr;
logic slv_b_ram_rd;
b_t slv_b_ram_din;
b_t slv_b_ram_dout;
logic [BRAM_IDX_WIDTH  :0] slv_b_ram_addra;
logic [BRAM_IDX_WIDTH-1:0] slv_b_ram_addrb;

logic slv_b_ram_wr_enable;
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        slv_b_ram_wr_enable <= 'd0;
    else if(mode_change)
        slv_b_ram_wr_enable <= 'd0;
    else if(mode=='d0)
        slv_b_ram_wr_enable <= 'd1;
    else if((mode=='d1) & (slv_bvalid_i & slv_bready_i & slv_b_cnt==mode_value))
        slv_b_ram_wr_enable <= 'd1;
    else if((mode=='d6) & (slv_awvalid_i & slv_awready_i & slv_awaddr_i[31:0]==mode_value))
        slv_b_ram_wr_enable <= 'd1;
    else if((mode=='d16)& (mst_awvalid_i & mst_awready_i & mst_awaddr_i[31:0]==mode_value))
        slv_b_ram_wr_enable <= 'd1;
end

assign slv_b_ram_wr = enable & slv_bvalid_i & slv_bready_i & (slv_b_ram_addra < 2**BRAM_IDX_WIDTH) &
                        (slv_b_ram_wr_enable | (
                                                    ((mode=='d6) & (slv_awvalid_i & slv_awready_i & slv_awaddr_i[31:0]==mode_value)) |
                                                    ((mode=='d1) & (slv_bvalid_i  & slv_bready_i  & slv_b_cnt   ==mode_value)) |
                                                    ((mode=='d16)& (mst_awvalid_i & mst_awready_i & mst_awaddr_i[31:0]==mode_value))
                                                    ));

always@(posedge clk or negedge rstn) begin
    if(~rstn)
        slv_b_ram_addra <= 'd0;
    else if(srelog[4])
        slv_b_ram_addra <= 'd0;
    else if(slv_b_ram_wr)
        slv_b_ram_addra <= slv_b_ram_addra + 'd1;
end
assign slv_b_ram_din.cnt        = slv_b_cnt;
assign slv_b_ram_din.bid_i      = slv_bid_i      ;
assign slv_b_ram_din.bresp_i    = slv_bresp_i    ;
assign slv_b_ram_din.buser_i    = slv_buser_i    ;
assign slv_b_ram_din.timer      = timer;
assign slv_b_ram_rd    = psel_i & ~pwrite_i & rd_sel=='d4 & paddr_i[11:0]>='h100;
logic [15:0] slv_b_ram_addrb_raw;
assign slv_b_ram_addrb_raw = {rd_shifth, paddr_i[11:0]-'h100} >> 2;
assign slv_b_ram_addrb = slv_b_ram_addrb_raw[BRAM_IDX_WIDTH-1:0];
    iommu_acd_sdpram_model #(
    /*parameter int  unsigned */ .AWIDTH    (BRAM_IDX_WIDTH         ), // = 4,
    /*parameter int  unsigned */ .DWIDTH    ($bits(slv_b_ram_din)   )  // = 128
    ) U_slv_b_ram (
    /*input  logic                      */  .clk    (clk                ),
    /*input  logic                      */  .wea    (~slv_b_ram_wr      ),
    /*input  logic                      */  .mea    (~slv_b_ram_wr      ),
    /*input  logic [AWIDTH-1:0]         */  .addra  ( slv_b_ram_addra[BRAM_IDX_WIDTH-1:0]   ),
    /*input  logic [DWIDTH-1:0]         */  .dina   ( slv_b_ram_din     ),
    /*input  logic                      */  .meb    (~slv_b_ram_rd      ),
    /*input  logic [AWIDTH-1:0]         */  .addrb  ( slv_b_ram_addrb   ),
    /*output logic [DWIDTH-1:0]         */  .doutb  ( slv_b_ram_dout    )
    );

logic mst_b_ram_wr;
logic mst_b_ram_rd;
b_t mst_b_ram_din;
b_t mst_b_ram_dout;
logic [BRAM_IDX_WIDTH  :0] mst_b_ram_addra;
logic [BRAM_IDX_WIDTH-1:0] mst_b_ram_addrb;

logic mst_b_ram_wr_enable;
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        mst_b_ram_wr_enable <= 'd0;
    else if(mode_change)
        mst_b_ram_wr_enable <= 'd0;
    else if(mode=='d0)
        mst_b_ram_wr_enable <= 'd1;
    else if((mode=='d1) & (mst_bvalid_i & mst_bready_i & mst_b_cnt==mode_value))
        mst_b_ram_wr_enable <= 'd1;
    else if((mode=='d6) & (slv_awvalid_i & slv_awready_i & slv_awaddr_i[31:0]==mode_value))
        mst_b_ram_wr_enable <= 'd1;
    else if((mode=='d16)& (mst_awvalid_i & mst_awready_i & mst_awaddr_i[31:0]==mode_value))
        mst_b_ram_wr_enable <= 'd1;
end

assign mst_b_ram_wr = enable & mst_bvalid_i & mst_bready_i & (mst_b_ram_addra < 2**BRAM_IDX_WIDTH) &
                        (mst_b_ram_wr_enable | (
                                                    ((mode=='d6) & (slv_awvalid_i & slv_awready_i & slv_awaddr_i[31:0]==mode_value)) |
                                                    ((mode=='d1) & (mst_bvalid_i  & mst_bready_i  & mst_b_cnt   ==mode_value)) |
                                                    ((mode=='d16)& (mst_awvalid_i & mst_awready_i & mst_awaddr_i[31:0]==mode_value))
                                                    ));

always@(posedge clk or negedge rstn) begin
    if(~rstn)
        mst_b_ram_addra <= 'd0;
    else if(mrelog[14])
        mst_b_ram_addra <= 'd0;
    else if(mst_b_ram_wr)
        mst_b_ram_addra <= mst_b_ram_addra + 'd1;
end
assign mst_b_ram_din.cnt        = mst_b_cnt;
assign mst_b_ram_din.bid_i      = mst_bid_i      ;
assign mst_b_ram_din.bresp_i    = mst_bresp_i    ;
assign mst_b_ram_din.buser_i    = mst_buser_i    ;
assign mst_b_ram_din.timer      = timer;
assign mst_b_ram_rd    = psel_i & ~pwrite_i & rd_sel=='d14 & paddr_i[11:0]>='h100;
logic [15:0] mst_b_ram_addrb_raw;
assign mst_b_ram_addrb_raw = {rd_shifth, paddr_i[11:0]-'h100} >> 2;
assign mst_b_ram_addrb = mst_b_ram_addrb_raw[BRAM_IDX_WIDTH-1:0];
    iommu_acd_sdpram_model #(
    /*parameter int  unsigned */ .AWIDTH    (BRAM_IDX_WIDTH         ), // = 4,
    /*parameter int  unsigned */ .DWIDTH    ($bits(mst_b_ram_din)   )  // = 128
    ) U_mst_b_ram (
    /*input  logic                      */  .clk    (clk                ),
    /*input  logic                      */  .wea    (~mst_b_ram_wr      ),
    /*input  logic                      */  .mea    (~mst_b_ram_wr      ),
    /*input  logic [AWIDTH-1:0]         */  .addra  ( mst_b_ram_addra[BRAM_IDX_WIDTH-1:0]   ),
    /*input  logic [DWIDTH-1:0]         */  .dina   ( mst_b_ram_din     ),
    /*input  logic                      */  .meb    (~mst_b_ram_rd      ),
    /*input  logic [AWIDTH-1:0]         */  .addrb  ( mst_b_ram_addrb   ),
    /*output logic [DWIDTH-1:0]         */  .doutb  ( mst_b_ram_dout    )
    );

//}}}

// RRAM {{{
logic slv_r_ram_wr;
logic slv_r_ram_rd;
r_t slv_r_ram_din;
r_t slv_r_ram_dout;
logic [RRAM_IDX_WIDTH  :0] slv_r_ram_addra;
logic [RRAM_IDX_WIDTH-1:0] slv_r_ram_addrb;

logic slv_r_ram_wr_enable;
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        slv_r_ram_wr_enable <= 'd0;
    else if(mode_change)
        slv_r_ram_wr_enable <= 'd0;
    else if(mode=='d0)
        slv_r_ram_wr_enable <= 'd1;
    else if((mode=='d1) & (slv_rvalid_i & slv_rready_i & slv_r_cnt==mode_value1))
        slv_r_ram_wr_enable <= 'd1;
    else if((mode=='d6) & (slv_arvalid_i & slv_arready_i & slv_araddr_i[31:0]==mode_value1))
        slv_r_ram_wr_enable <= 'd1;
    else if((mode=='d16)& (mst_arvalid_i & mst_arready_i & mst_araddr_i[31:0]==mode_value1))
        slv_r_ram_wr_enable <= 'd1;
end

assign slv_r_ram_wr = enable & slv_rvalid_i & slv_rready_i & (slv_r_ram_addra < 2**RRAM_IDX_WIDTH) &
                        (slv_r_ram_wr_enable | (
                                                    ((mode=='d6) & (slv_arvalid_i & slv_arready_i & slv_araddr_i[31:0]==mode_value1)) |
                                                    ((mode=='d1) & (slv_rvalid_i  & slv_rready_i  & slv_r_cnt   ==mode_value1)) |
                                                    ((mode=='d16)& (mst_arvalid_i & mst_arready_i & mst_araddr_i[31:0]==mode_value1))
                                                    ));

always@(posedge clk or negedge rstn) begin
    if(~rstn)
        slv_r_ram_addra <= 'd0;
    else if(srelog[5])
        slv_r_ram_addra <= 'd0;
    else if(slv_r_ram_wr)
        slv_r_ram_addra <= slv_r_ram_addra + 'd1;
end
assign slv_r_ram_din.cnt        = slv_r_cnt;
assign slv_r_ram_din.rid_i      = slv_rid_i      ;
assign slv_r_ram_din.rdata_i    = slv_rdata_i    ;
assign slv_r_ram_din.rresp_i    = slv_rresp_i    ;
assign slv_r_ram_din.rlast_i    = slv_rlast_i    ;
assign slv_r_ram_din.ruser_i    = slv_ruser_i    ;
assign slv_r_ram_din.timer      = timer;
assign slv_r_ram_rd    = psel_i & ~pwrite_i & rd_sel=='d5 & paddr_i[11:0]>='h100;
logic [15:0] slv_r_ram_addrb_raw;
assign slv_r_ram_addrb_raw = {rd_shifth, paddr_i[11:0]-'h100} >> 2;
assign slv_r_ram_addrb = slv_r_ram_addrb_raw[RRAM_IDX_WIDTH-1:0];
    iommu_acd_sdpram_model #(
    /*parameter int  unsigned */ .AWIDTH    (RRAM_IDX_WIDTH         ), // = 4,
    /*parameter int  unsigned */ .DWIDTH    ($bits(slv_r_ram_din)   )  // = 128
    ) U_slv_r_ram (
    /*input  logic                      */  .clk    (clk                ),
    /*input  logic                      */  .wea    (~slv_r_ram_wr      ),
    /*input  logic                      */  .mea    (~slv_r_ram_wr      ),
    /*input  logic [AWIDTH-1:0]         */  .addra  ( slv_r_ram_addra[RRAM_IDX_WIDTH-1:0]   ),
    /*input  logic [DWIDTH-1:0]         */  .dina   ( slv_r_ram_din     ),
    /*input  logic                      */  .meb    (~slv_r_ram_rd      ),
    /*input  logic [AWIDTH-1:0]         */  .addrb  ( slv_r_ram_addrb   ),
    /*output logic [DWIDTH-1:0]         */  .doutb  ( slv_r_ram_dout    )
    );

logic mst_r_ram_wr;
logic mst_r_ram_rd;
r_t mst_r_ram_din;
r_t mst_r_ram_dout;
logic [RRAM_IDX_WIDTH  :0] mst_r_ram_addra;
logic [RRAM_IDX_WIDTH-1:0] mst_r_ram_addrb;

logic mst_r_ram_wr_enable;
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        mst_r_ram_wr_enable <= 'd0;
    else if(mode_change)
        mst_r_ram_wr_enable <= 'd0;
    else if(mode=='d0)
        mst_r_ram_wr_enable <= 'd1;
    else if((mode=='d1) & (mst_rvalid_i & mst_rready_i & mst_r_cnt==mode_value1))
        mst_r_ram_wr_enable <= 'd1;
    else if((mode=='d6) & (slv_arvalid_i & slv_arready_i & slv_araddr_i[31:0]==mode_value1))
        mst_r_ram_wr_enable <= 'd1;
    else if((mode=='d16)& (mst_arvalid_i & mst_arready_i & mst_araddr_i[31:0]==mode_value1))
        mst_r_ram_wr_enable <= 'd1;
end

assign mst_r_ram_wr = enable & mst_rvalid_i & mst_rready_i & (mst_r_ram_addra < 2**RRAM_IDX_WIDTH) &
                        (mst_r_ram_wr_enable | (
                                                    ((mode=='d6) & (slv_arvalid_i & slv_arready_i & slv_araddr_i[31:0]==mode_value1)) |
                                                    ((mode=='d1) & (mst_rvalid_i  & mst_rready_i  & mst_r_cnt   ==mode_value1)) |
                                                    ((mode=='d16)& (mst_arvalid_i & mst_arready_i & mst_araddr_i[31:0]==mode_value1))
                                                    ));

always@(posedge clk or negedge rstn) begin
    if(~rstn)
        mst_r_ram_addra <= 'd0;
    else if(mrelog[15])
        mst_r_ram_addra <= 'd0;
    else if(mst_r_ram_wr)
        mst_r_ram_addra <= mst_r_ram_addra + 'd1;
end
assign mst_r_ram_din.cnt        = mst_r_cnt;
assign mst_r_ram_din.rid_i      = mst_rid_i      ;
assign mst_r_ram_din.rdata_i    = mst_rdata_i    ;
assign mst_r_ram_din.rresp_i    = mst_rresp_i    ;
assign mst_r_ram_din.rlast_i    = mst_rlast_i    ;
assign mst_r_ram_din.ruser_i    = mst_ruser_i    ;
assign mst_r_ram_din.timer      = timer;
assign mst_r_ram_rd    = psel_i & ~pwrite_i & rd_sel=='d15 & paddr_i[11:0]>='h100;
logic [15:0] mst_r_ram_addrb_raw;
assign mst_r_ram_addrb_raw = {rd_shifth, paddr_i[11:0]-'h100} >> 2;
assign mst_r_ram_addrb = mst_r_ram_addrb_raw[RRAM_IDX_WIDTH-1:0];
    iommu_acd_sdpram_model #(
    /*parameter int  unsigned */ .AWIDTH    (RRAM_IDX_WIDTH         ), // = 4,
    /*parameter int  unsigned */ .DWIDTH    ($bits(mst_r_ram_din)   )  // = 128
    ) U_mst_r_ram (
    /*input  logic                      */  .clk    (clk                ),
    /*input  logic                      */  .wea    (~mst_r_ram_wr      ),
    /*input  logic                      */  .mea    (~mst_r_ram_wr      ),
    /*input  logic [AWIDTH-1:0]         */  .addra  ( mst_r_ram_addra[RRAM_IDX_WIDTH-1:0]   ),
    /*input  logic [DWIDTH-1:0]         */  .dina   ( mst_r_ram_din     ),
    /*input  logic                      */  .meb    (~mst_r_ram_rd      ),
    /*input  logic [AWIDTH-1:0]         */  .addrb  ( mst_r_ram_addrb   ),
    /*output logic [DWIDTH-1:0]         */  .doutb  ( mst_r_ram_dout    )
    );

//}}}

logic [31:0] slv_bresp_err_cnt, mst_bresp_err_cnt;
logic [31:0] slv_rresp_err_cnt, mst_rresp_err_cnt;
always@(posedge clk or negedge rstn) begin
    if(~rstn) begin
        slv_bresp_err_cnt <= 'd0;
        errflag[0]           <= 'd0;
    end
    else begin
        if(errrelog[20]) begin
            slv_bresp_err_cnt   <= 'd0;
            errflag[0]          <= 'd0;
        end
        else if(slv_bvalid_i & slv_bready_i & (slv_bresp_i!='d0) & (errflag[0]=='d0)) begin
            slv_bresp_err_cnt   <= slv_b_cnt;
            errflag[0]          <= 1'b1;
        end
    end
end

always@(posedge clk or negedge rstn) begin
    if(~rstn) begin
        mst_bresp_err_cnt <= 'd0;
        errflag[1]           <= 'd0;
    end
    else begin
        if(errrelog[21]) begin
            mst_bresp_err_cnt   <= 'd0;
            errflag[1]          <= 1'b0;
        end
        else if(mst_bvalid_i & mst_bready_i & (mst_bresp_i!='d0) & (errflag[1]=='d0)) begin
            mst_bresp_err_cnt   <= mst_b_cnt;
            errflag[1]          <= 1'b1;
        end
    end
end
always@(posedge clk or negedge rstn) begin
    if(~rstn) begin
        slv_rresp_err_cnt <= 'd0;
        errflag[2]           <= 'd0;
    end
    else begin
        if(errrelog[22]) begin
            slv_rresp_err_cnt   <= 'd0;
            errflag[2]          <= 1'b0;
        end
        else if(slv_rvalid_i & slv_rready_i & (slv_rresp_i!='d0) & (errflag[2]=='d0)) begin
            slv_rresp_err_cnt   <= slv_r_cnt;
            errflag[2]          <= 1'b1;
        end
    end
end
always@(posedge clk or negedge rstn) begin
    if(~rstn) begin
        mst_rresp_err_cnt <= 'd0;
        errflag[3]           <= 'd0;
    end
    else begin
        if(errrelog[23]) begin
            mst_rresp_err_cnt   <= 'd0;
            errflag[3]          <= 1'b0;
        end
        else if(mst_rvalid_i & mst_rready_i & (mst_rresp_i!='d0) & (errflag[3]=='d0)) begin
            mst_rresp_err_cnt   <= mst_r_cnt;
            errflag[3]          <= 1'b1;
        end
    end
end


logic [31:0] cfg_prdata;
logic [31:0] ram_prdata;
always@(posedge clk or negedge rstn) begin
    if(~rstn)
        cfg_prdata <= 'd0;
    else begin
        if(psel_i & ~pwrite_i) begin
            case(paddr_i[7:0])
            'h0: cfg_prdata <= {26'd0, mode, dostart};
            'h4: cfg_prdata <= {mode_value};
            'h8: cfg_prdata <= {16'd0, rd_sel, rd_shifth, rd_shiftv};
            'hc: cfg_prdata <= {mode_value1};
            'h10:cfg_prdata <= slv_aw_cnt;
            'h14:cfg_prdata <= slv_ar_cnt;
            'h18:cfg_prdata <= slv_w_cnt;
            'h1c:cfg_prdata <= slv_b_cnt;
            'h20:cfg_prdata <= slv_r_cnt;
            'h30:cfg_prdata <= mst_aw_cnt;
            'h34:cfg_prdata <= mst_ar_cnt;
            'h38:cfg_prdata <= mst_w_cnt;
            'h3c:cfg_prdata <= mst_b_cnt;
            'h40:cfg_prdata <= mst_r_cnt;
            'h50:cfg_prdata <= slv_bresp_err_cnt;
            'h54:cfg_prdata <= mst_bresp_err_cnt;
            'h58:cfg_prdata <= slv_rresp_err_cnt;
            'h5c:cfg_prdata <= mst_rresp_err_cnt;
            'h60:cfg_prdata <= {28'd0, errflag};
            default: cfg_prdata <= 'hefefefef;
            endcase
        end
    end
end

awr_t   slv_aw_ram_dout_shifted;
awr_t   slv_ar_ram_dout_shifted;
w_t     slv_w_ram_dout_shifted;
b_t     slv_b_ram_dout_shifted;
r_t     slv_r_ram_dout_shifted;
awr_t   mst_aw_ram_dout_shifted;
awr_t   mst_ar_ram_dout_shifted;
w_t     mst_w_ram_dout_shifted;
b_t     mst_b_ram_dout_shifted;
r_t     mst_r_ram_dout_shifted;
assign slv_aw_ram_dout_shifted = slv_aw_ram_dout >> (rd_shiftv*32);
assign slv_ar_ram_dout_shifted = slv_ar_ram_dout >> (rd_shiftv*32);
assign slv_w_ram_dout_shifted  = slv_w_ram_dout  >> (rd_shiftv*32);
assign slv_b_ram_dout_shifted  = slv_b_ram_dout  >> (rd_shiftv*32);
assign slv_r_ram_dout_shifted  = slv_r_ram_dout  >> (rd_shiftv*32);
assign mst_aw_ram_dout_shifted = mst_aw_ram_dout >> (rd_shiftv*32);
assign mst_ar_ram_dout_shifted = mst_ar_ram_dout >> (rd_shiftv*32);
assign mst_w_ram_dout_shifted  = mst_w_ram_dout  >> (rd_shiftv*32);
assign mst_b_ram_dout_shifted  = mst_b_ram_dout  >> (rd_shiftv*32);
assign mst_r_ram_dout_shifted  = mst_r_ram_dout  >> (rd_shiftv*32);

assign ram_prdata =
                    rd_sel=='d1  ? slv_aw_ram_dout_shifted[31:0] :
                    rd_sel=='d2  ? slv_ar_ram_dout_shifted[31:0] :
                    rd_sel=='d3  ? slv_w_ram_dout_shifted[31:0] :
                    rd_sel=='d4  ? slv_b_ram_dout_shifted[31:0] :
                    rd_sel=='d5  ? slv_r_ram_dout_shifted[31:0] :
                    rd_sel=='d11 ? mst_aw_ram_dout_shifted[31:0] :
                    rd_sel=='d12 ? mst_ar_ram_dout_shifted[31:0] :
                    rd_sel=='d13 ? mst_w_ram_dout_shifted[31:0] :
                    rd_sel=='d14 ? mst_b_ram_dout_shifted[31:0] :
                    rd_sel=='d15 ? mst_r_ram_dout_shifted[31:0] :
                                   32'hff_ee_dd_cc;

assign pready_o = 1'b1;
assign prdata_o = paddr_i[11:8]>0 ? ram_prdata : cfg_prdata;


endmodule


module iommu_acd_sdpram_model #(
    parameter int  unsigned  AWIDTH     = 4,
    parameter int  unsigned  DWIDTH     = 128
) (
    input  logic                        clk,
    input  logic                        wea,
    input  logic                        mea,
    input  logic [AWIDTH-1:0]           addra,
    input  logic [DWIDTH-1:0]           dina,
    input  logic                        meb,
    input  logic [AWIDTH-1:0]           addrb,
    output logic [DWIDTH-1:0]           doutb
);

`ifdef IOMMU_IMPLEMENTATION
    genvar i,j;
    generate
        //--- use ff if depth is too small
        if(AWIDTH<5) begin
            logic [DWIDTH-1:0]  mem[(1<<AWIDTH)-1:0];
            logic [DWIDTH-1:0]  memreg;

            assign doutb = memreg;

            always@(posedge clk) begin
                if (~wea & ~mea) begin
                    mem[addra] <= dina;
                end
            end


            always@(posedge clk) begin
                if (~meb) begin
                    memreg <= mem[addrb];
                end
                else begin
                    memreg <= 'b0;
                end
            end
        end
        //--- ATTENTION: modify following IMPLEMENTATION branch code with real ram lib, wbuf_ram_wrap
        else if(DWIDTH==206 && AWIDTH==6) begin
            sram_acd_2p64x206 U_ram_DONTTOUCH(
                .RW0_clk    (clk        ),
                .RW0_addra  (addra      ),
                .RW0_addrb  (addrb      ),
                .RW0_wen    (~mea & ~wea),
                .RW0_ren    (~meb       ),
                .RW0_wdata  (dina       ),
                .RW0_rdata  (doutb      )
            );
        end
        else if(DWIDTH==85 && AWIDTH==6) begin
            sram_acd_2p64x85 U_ram_DONTTOUCH(
                .RW0_clk    (clk        ),
                .RW0_addra  (addra      ),
                .RW0_addrb  (addrb      ),
                .RW0_wen    (~mea & ~wea),
                .RW0_ren    (~meb       ),
                .RW0_wdata  (dina       ),
                .RW0_rdata  (doutb      )
            );
        end
        else if(DWIDTH==342 && AWIDTH==8) begin
            sram_acd_2p256x342 U_ram_DONTTOUCH(
                .RW0_clk    (clk        ),
                .RW0_addra  (addra      ),
                .RW0_addrb  (addrb      ),
                .RW0_wen    (~mea & ~wea),
                .RW0_ren    (~meb       ),
                .RW0_wdata  (dina       ),
                .RW0_rdata  (doutb      )
            );
        end
        else if(DWIDTH==361 && AWIDTH==8) begin
            sram_acd_2p256x361 U_ram_DONTTOUCH(
                .RW0_clk    (clk        ),
                .RW0_addra  (addra      ),
                .RW0_addrb  (addrb      ),
                .RW0_wen    (~mea & ~wea),
                .RW0_ren    (~meb       ),
                .RW0_wdata  (dina       ),
                .RW0_rdata  (doutb      )
            );
        end
        //-- ATTENTION END
        else begin
            logic [DWIDTH-1:0]  mem[(1<<AWIDTH)-1:0];
            logic [DWIDTH-1:0]  memreg;

            assign doutb = memreg;

            always@(posedge clk) begin
                if (~wea & ~mea) begin
                    mem[addra] <= dina;
                end
            end


            always@(posedge clk) begin
                if (~meb) begin
                    memreg <= mem[addrb];
                end
                else begin
                    memreg <= 'b0;
                end
            end
        end
    endgenerate
`else
logic [DWIDTH-1:0]  mem[(1<<AWIDTH)-1:0];
logic [DWIDTH-1:0]  memreg;

assign doutb = memreg;

always@(posedge clk) begin
    if (~wea & ~mea) begin
        mem[addra] <= dina;
    end
end


always@(posedge clk) begin
    if (~meb) begin
        memreg <= mem[addrb];
    end
    else begin
        memreg <= 'b0;
    end
end
`endif

endmodule
