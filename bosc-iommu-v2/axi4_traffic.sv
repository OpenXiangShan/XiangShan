module axi4_traffic(
    input                   clk,
    input                   rstn,

    input   [31:0]          axi4_ar_cfg0    ,
    input   [31:0]          axi4_ar_cfg1    ,
    input   [31:0]          axi4_ar_cfg2    ,
    input   [31:0]          axi4_ar_cfg3    ,
    input   [31:0]          axi4_ar_cfg4    ,
    input   [31:0]          axi4_ar_cfg5    ,
    input                   axi4_ar_start   ,

    input   [31:0]          axi4_aw_cfg0    ,
    input   [31:0]          axi4_aw_cfg1    ,
    input   [31:0]          axi4_aw_cfg2    ,
    input   [31:0]          axi4_aw_cfg3    ,
    input   [31:0]          axi4_aw_cfg4    ,
    input   [31:0]          axi4_aw_cfg5    ,
    input                   axi4_aw_start   ,

    output  [31:0]          axi4_traffic_rlt,

    output  [3:0]           axi4_awid       ,
    output  [39:0]          axi4_awaddr     ,
    output  [7:0]           axi4_awlen      ,
    output  [2:0]           axi4_awsize     ,
    output  [1:0]           axi4_awburst    ,
    output                  axi4_awlock     ,
    output  [3:0]           axi4_awcache    ,
    output  [2:0]           axi4_awprot     ,
    output  [3:0]           axi4_awregion   ,
    output  [53:0]          axi4_awuser     ,
    output  [3:0]           axi4_awqos      ,
    output                  axi4_awvalid    ,
    output  [3:0]           axi4_arid       ,
    output  [39:0]          axi4_araddr     ,
    output  [7:0]           axi4_arlen      ,
    output  [2:0]           axi4_arsize     ,
    output  [1:0]           axi4_arburst    ,
    output                  axi4_arlock     ,
    output  [3:0]           axi4_arcache    ,
    output  [2:0]           axi4_arprot     ,
    output  [3:0]           axi4_arregion   ,
    output  [53:0]          axi4_aruser     ,
    output  [3:0]           axi4_arqos      ,
    output                  axi4_arvalid    ,
    output  [255:0]         axi4_wdata      ,
    output  [31:0]          axi4_wstrb      ,
    output                  axi4_wlast      ,
    output                  axi4_wvalid     ,
    output  [7:0]           axi4_wuser      ,
    output                  axi4_rready     ,
    output                  axi4_bready     ,
    input                   axi4_awready    ,
    input                   axi4_arready    ,
    input                   axi4_wready     ,
    input   [3:0]           axi4_rid        ,
    input   [255:0]         axi4_rdata      ,
    input   [1:0]           axi4_rresp      ,
    input                   axi4_rlast      ,
    input                   axi4_rvalid     ,
    input   [7:0]           axi4_ruser      ,
    input   [3:0]           axi4_bid        ,
    input                   axi4_bvalid     ,
    input   [1:0]           axi4_bresp      ,
    input   [7:0]           axi4_buser
);



////////**********************************************************************************************************///////
//ar channel
////////**********************************************************************************************************///////
    // FSM states
    enum logic [1:0] {
    AR_IDLE         ,
    AR_REQ      ,
    AR_ACK
    } ar_cs,ar_ns;



logic     [11:0]          ar_pkt_intv  ;
logic     [19:0]          ar_pkt_num ;
logic     [3:0]           init_arid  ;
logic     [39:0]          init_araddr  ;
logic     [7:0]           init_arlen   ;
logic     [2:0]           init_arsize   ;
logic     [3:0]           init_arcache ;
logic     [2:0]           init_arprot  ;
logic     [53:0]          init_aruser  ;
logic     [3:0]           init_arqos   ;
logic                     init_ar_tst  ;
logic                     arvalid       ;
logic     [39:0]          araddr        ;
logic     [11:0]          ar_cnt1       ;
logic     [19:0]          ar_cnt2       ;


assign  axi4_arid     = init_arid;
assign  axi4_araddr   = araddr;
assign  axi4_arlen    = init_arlen;
assign  axi4_arsize   = init_arsize; //data_width==2**5 byte
assign  axi4_arburst  = 2'd1; //incr
assign  axi4_arlock   = 1'b0;
assign  axi4_arcache  = init_arcache;
assign  axi4_arprot   = init_arprot;
assign  axi4_arregion = 4'd0;
assign  axi4_aruser   = init_aruser;
assign  axi4_arqos    = init_arqos;
assign  axi4_arvalid  = arvalid;
assign  axi4_rready = 1'b1;


assign init_arsize   = 3'd5; //data_width==2**5 byte
assign ar_pkt_intv = axi4_ar_cfg0[11:0];
assign ar_pkt_num  = axi4_ar_cfg0[31:12];
assign init_araddr = {axi4_ar_cfg2[7:0],axi4_ar_cfg1};
assign init_arid = axi4_ar_cfg2[11:8];
assign init_arlen = axi4_ar_cfg2[19:12];
assign init_arcache = axi4_ar_cfg2[23:20];
assign init_arprot = axi4_ar_cfg2[26:24];
assign init_arqos = axi4_ar_cfg2[31:28];
assign init_aruser = {axi4_ar_cfg4[21:0],axi4_ar_cfg3};//1'b0,pv,pid,did,8'd0;
assign init_ar_tst = axi4_ar_cfg5[0];


always@(posedge clk or negedge rstn) begin
    if (!rstn)
        ar_cs <= AR_IDLE;
    else
        ar_cs <= ar_ns;
end


always@(*) begin
    case (ar_cs)
        AR_IDLE:
            if (axi4_ar_start)
                ar_ns = AR_REQ;
            else
                ar_ns = AR_IDLE;
        AR_REQ:
            if ((ar_cnt1 == ar_pkt_intv) && (ar_cnt2 == ar_pkt_num - 1))
                ar_ns = AR_IDLE;
            else if (ar_cnt1 == ar_pkt_intv)
                ar_ns = AR_ACK;
            else
                ar_ns = AR_REQ;
        AR_ACK:
            if (axi4_arready)
                ar_ns = AR_REQ;
            else
                ar_ns = AR_ACK;
        default:
             ar_ns = AR_IDLE;
    endcase
end


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        ar_cnt1 <= 12'd0;
    else if (axi4_ar_start)
        ar_cnt1 <= 12'd0;
    else if (ar_cs == AR_REQ)
        ar_cnt1 <= ar_cnt1 + 1;
    else
        ar_cnt1 <= 12'd0;
end


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        ar_cnt2 <= 20'd0;
    else if (axi4_ar_start)
        ar_cnt2 <= 20'd0;
    else if ((ar_cs == AR_ACK) && axi4_arready)
        ar_cnt2 <= ar_cnt2 + 1;
    else
        ar_cnt2 <= ar_cnt2;
end


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        arvalid <= 1'b0;
    else if ((ar_cs == AR_REQ) && (ar_cnt1 == 12'd1))
        arvalid <= 1'b1;
    else
        arvalid <= 1'b0;
end


//aligh to 32byte
always@(posedge clk or negedge rstn) begin
    if (~rstn)
        araddr <= 40'd0;
    else if (axi4_ar_start)
        araddr <= init_araddr;
    else if ((ar_cs == AR_REQ) && (ar_cnt1 == 12'd2))
        araddr <= araddr + 2**init_arsize * (init_arlen+1);
    else
        araddr <= araddr;
end


////////**********************************************************************************************************///////
//ar compare
////////**********************************************************************************************************///////
logic [19:0]            ar_cmp_cnt;
logic                   ar_cmp_end;
logic [255:0]           ar_cmp_data;
logic                   ar_cmp_err_en;
logic [7:0]             ar_cmp_err_cnt;

assign axi4_traffic_rlt[28] = ar_cmp_end;
assign axi4_traffic_rlt[27:8] = ar_cmp_cnt;
assign axi4_traffic_rlt[7:0] = ar_cmp_err_cnt[7:0];


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        ar_cmp_cnt <= 20'd0;
    else if (axi4_ar_start || axi4_ar_cfg5[0])
        ar_cmp_cnt <= 20'd0;
    else if ((axi4_rresp == 2'd0) && axi4_rvalid && axi4_rlast)
        ar_cmp_cnt <= ar_cmp_cnt + 1;
    else
        ar_cmp_cnt <= ar_cmp_cnt;
end


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        ar_cmp_end <= 1'b0;
    else if (axi4_ar_start || axi4_ar_cfg5[0])

        ar_cmp_end <= 1'b0;
    else if (ar_cmp_cnt == ar_pkt_num)
        ar_cmp_end <= 1'b1;
    else
        ar_cmp_end <= ar_cmp_end;
end


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        ar_cmp_data <= 256'd0;
    else if (axi4_ar_start)
        ar_cmp_data <= 256'h000f_000e_000d_000c_000b_000a_0009_0008_0007_0006_0005_0004_0003_0002_0001_0000;
    else if (axi4_rvalid)
        ar_cmp_data <= {ar_cmp_data[254:0],ar_cmp_data[255]};
    else
        ar_cmp_data <= ar_cmp_data;
end


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        ar_cmp_err_en <= 1'b0;
    else if (axi4_ar_start || axi4_ar_cfg5[0])
        ar_cmp_err_en <= 1'b0;
    else if (axi4_rvalid && (axi4_rdata != ar_cmp_data))
        ar_cmp_err_en <= 1'b1;
    else
        ar_cmp_err_en <= 1'b0;
end

always@(posedge clk or negedge rstn) begin
    if (~rstn)
        ar_cmp_err_cnt <= 8'd0;
    else if (axi4_ar_start  || axi4_ar_cfg5[0])
        ar_cmp_err_cnt <= 8'd0;
    else if (ar_cmp_err_en)
        ar_cmp_err_cnt <= ar_cmp_err_cnt + 1;
    else
        ar_cmp_err_cnt <= ar_cmp_err_cnt;
end

////////**********************************************************************************************************///////
//aw channel
////////**********************************************************************************************************///////
    // FSM states
    enum logic [1:0] {
    AW_IDLE         ,
    AW_REQ      ,
    AW_ACK
    } aw_cs,aw_ns;

logic     [11:0]          aw_pkt_intv  ;
logic     [19:0]          aw_pkt_num ;
logic     [3:0]           init_awid  ;
logic     [39:0]          init_awaddr  ;
logic     [7:0]           init_awlen   ;
logic     [2:0]           init_awsize   ;
logic     [3:0]           init_awcache ;
logic     [2:0]           init_awprot  ;
logic     [53:0]          init_awuser  ;
logic     [3:0]           init_awqos   ;
logic                     init_aw_tst   ;
logic                     awvalid       ;
logic     [39:0]          awaddr        ;
logic     [11:0]          aw_cnt1       ;
logic     [19:0]          aw_cnt2       ;
logic     [255:0]         wdata   ;
logic                     wvalid        ;
logic                     wlast     ;

assign  axi4_awid     = init_awid;
assign  axi4_awaddr   = awaddr;
assign  axi4_awlen    = init_awlen;
assign  axi4_awsize   = init_awsize; //data_width==2**5byte
assign  axi4_awburst  = 2'd1; //incr
assign  axi4_awlock   = 1'b0;
assign  axi4_awcache  = init_awcache;
assign  axi4_awprot   = init_awprot;
assign  axi4_awregion = 4'd0;
assign  axi4_awuser   = init_awuser;
assign  axi4_awqos    = init_awqos;
assign  axi4_awvalid  = awvalid;
assign  axi4_bready = 1'b1;
assign  axi4_wdata  = wdata;
assign  axi4_wstrb  = '1;
assign  axi4_wlast  = wlast;
assign  axi4_wvalid = wvalid;
assign  axi4_wuser  = 'd0;

assign init_awsize   = 3'd5; //data_width==2**5 byte
assign aw_pkt_intv = axi4_aw_cfg0[11:0];
assign aw_pkt_num  = axi4_aw_cfg0[31:12];
assign init_awaddr = {axi4_aw_cfg2[7:0],axi4_aw_cfg1};
assign init_awid = axi4_aw_cfg2[11:8];
assign init_awlen = axi4_aw_cfg2[19:12];
assign init_awcache = axi4_aw_cfg2[23:20];
assign init_awprot = axi4_aw_cfg2[26:24];
assign init_awqos = axi4_aw_cfg2[31:28];
assign init_awuser = {axi4_aw_cfg4[21:0],axi4_aw_cfg3};//1'b0,pv,pid,did,8'd0;
assign init_aw_tst = axi4_aw_cfg5[0];


always@(posedge clk or negedge rstn) begin
    if (!rstn)
        aw_cs <= AW_IDLE;
    else
        aw_cs <= aw_ns;
end


always@(*) begin
    case (aw_cs)
    AW_IDLE:
            if (axi4_aw_start)
                aw_ns = AW_REQ;
            else
                aw_ns = AW_IDLE;
        AW_REQ:
            if ((aw_cnt1 == aw_pkt_intv) && (aw_cnt2 == aw_pkt_num - 1))
                aw_ns = AW_IDLE;
            else if (aw_cnt1 == aw_pkt_intv)
                aw_ns = AW_ACK;
            else
                aw_ns = AW_REQ;
        AW_ACK:
            if (axi4_awready)
                aw_ns = AW_REQ;
            else
                aw_ns = AW_ACK;
        default:
             aw_ns = AW_IDLE;
    endcase
end


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        aw_cnt1 <= 12'd0;
    else if (axi4_aw_start)
        aw_cnt1 <= 12'd0;
    else if (aw_cs == AW_REQ)
        aw_cnt1 <= aw_cnt1 + 1;
    else
        aw_cnt1 <= 12'd0;
end


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        aw_cnt2 <= 20'd0;
    else if (axi4_aw_start)
        aw_cnt2 <= 20'd0;
    else if ((aw_cs == AW_ACK) && axi4_awready)
        aw_cnt2 <= aw_cnt2 + 1;
    else
        aw_cnt2 <= aw_cnt2;
end


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        awvalid <= 1'b0;
    else if ((aw_cs == AW_REQ) && (aw_cnt1 == 12'd1))
        awvalid <= 1'b1;
    else
        awvalid <= 1'b0;
end


//aligh to 32byte
always@(posedge clk or negedge rstn) begin
    if (~rstn)
        awaddr <= 40'd0;
    else if (axi4_aw_start)
        awaddr <= init_awaddr;
    else if ((aw_cs == AW_REQ) && (aw_cnt1 == 12'd2))
        awaddr <= awaddr + 2**init_awsize * (init_awlen + 1);
    else
        awaddr <= awaddr;
end


////////**********************************************************************************************************///////
//w channel
////////**********************************************************************************************************///////
logic     [11:0]          w_cnt1;
logic     [19:0]          w_cnt2;
logic     [7:0]           w_cnt3;


// FSM states
    enum logic [1:0] {
    W_IDLE      ,
    W_REQ       ,
    W_ACK
    } w_cs,w_ns;


always@(posedge clk or negedge rstn) begin
    if (!rstn)
        w_cs <= W_IDLE;
    else
        w_cs <= w_ns;
end


always@(*) begin
    case (w_cs)
        W_IDLE:
            if (axi4_aw_start)
                w_ns = W_REQ;
            else
                w_ns = W_IDLE;
        W_REQ:
//            if ((w_cnt1 == aw_pkt_intv - init_awlen) && (w_cnt2 == aw_pkt_num - 1))
//              w_ns = W_IDLE;
            if (w_cnt1 == aw_pkt_intv)
                w_ns = W_ACK;
            else
                w_ns = W_REQ;
        W_ACK:
            if (wvalid && axi4_wready && (w_cnt3 == init_awlen) && (w_cnt2 == aw_pkt_num - 1))
                w_ns = W_IDLE;
            else if (wvalid && axi4_wready && (w_cnt3 == init_awlen))
                w_ns = W_REQ;
            else
                w_ns = W_ACK;
        default:
             w_ns = W_IDLE;
    endcase
end


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        w_cnt1 <= 12'd0;
    else if (axi4_aw_start)
        w_cnt1 <= 12'd0;
    else if (w_cs == W_REQ)
        w_cnt1 <= w_cnt1 + 1;
    else
        w_cnt1 <= 12'd0;
end


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        w_cnt2 <= 20'd0;
    else if (axi4_aw_start)
        w_cnt2 <= 20'd0;
    else if ((w_cs == W_ACK) && axi4_wready && wvalid && (w_cnt3 == init_awlen))
        w_cnt2 <= w_cnt2 + 1;
    else
        w_cnt2 <= w_cnt2;
end

always@(posedge clk or negedge rstn) begin
    if (~rstn)
        w_cnt3 <= 8'd0;
    else if (axi4_aw_start || (w_cs != W_ACK))
        w_cnt3 <= 8'd0;
    else if ((w_cs == W_ACK) && axi4_wready && wvalid && (w_cnt3 == init_awlen))
        w_cnt3 <= 8'd0;
    else if ((w_cs == W_ACK) && axi4_wready && wvalid)
        w_cnt3 <= w_cnt3 + 1;
    else
        w_cnt3 <= w_cnt3;
end


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        wvalid <= 1'b0;
    else if ((w_cs == W_ACK) && axi4_wready && wvalid && (w_cnt3 == init_awlen))
         wvalid <= 1'b0;
    else if (w_cs == W_ACK)
        wvalid <= 1'b1;
    else
        wvalid <= 1'b0;
end

assign wlast = ((w_cs == W_ACK) && axi4_wready && wvalid && (w_cnt3 == init_awlen));


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        wdata <= 256'd0;
    else if (axi4_aw_start)
        wdata <= 256'h000f_000e_000d_000c_000b_000a_0009_0008_0007_0006_0005_0004_0003_0002_0001_0000;
    else if ((w_cs == W_ACK) && axi4_wready && wvalid)
        wdata <= {wdata[254:0],wdata[255]};
//      wdata <= {wdata[255:140]+16};
    else
        wdata <= wdata;
end

////////**********************************************************************************************************///////
//aw pkt counter
////////**********************************************************************************************************///////
logic [19:0]            aw_cmp_cnt;
logic                   aw_cmp_end;

assign axi4_traffic_rlt[31:30] = 2'd0;
assign axi4_traffic_rlt[29] = aw_cmp_end;
always@(posedge clk or negedge rstn) begin
    if (~rstn)
        aw_cmp_cnt <= 20'd0;
    else if (axi4_aw_start || axi4_aw_cfg5[0])
        aw_cmp_cnt <= 20'd0;
    else if ((axi4_bresp == 2'd0) && axi4_bvalid)
        aw_cmp_cnt <= aw_cmp_cnt + 1;
    else
        aw_cmp_cnt <= aw_cmp_cnt;
end


always@(posedge clk or negedge rstn) begin
    if (~rstn)
        aw_cmp_end <= 1'b0;
    else if (axi4_aw_start || axi4_aw_cfg5[0])
        aw_cmp_end <= 1'b0;
    else if (aw_cmp_cnt == aw_pkt_num)
        aw_cmp_end <= 1'b1;
    else
        aw_cmp_end <= aw_cmp_end;
end


endmodule


