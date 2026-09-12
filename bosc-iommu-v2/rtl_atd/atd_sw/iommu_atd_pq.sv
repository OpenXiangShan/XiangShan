///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd_pq.sv
//Version       :   2.0
//Author        :   yangyy
//Description   :
//      xxx     :
//      xxxx    :
//      xx      :
//Increase: 2.1
//
//          2.2
///*********************************************//

module iommu_atd_pq #(
    parameter  RISCV_PLEN   = 56
) (
    input                           iommu_clk,
    input                           iommu_rstn,

    input   [43:0]                  iommu_pqb_ppn_i,
    input   [4:0]                   iommu_pqb_size_i,
    input   [31:0]                  iommu_pqh_i,

    input   [3:0]                   iommu_pqcsr_i,
    output  [3:0]                   iommu_pqcsr_o,
    input                           iommu_pip_clr_i,
    output                          iommu_pip_en_o,
    output  [31:0]                  iommu_pqt_o,

    //from cdw
    input                           pq_pri_wen_i,
    input   [127:0]                 pq_pri_wdata_i,
    output                          pq_pri_full_o,

    //pq
    output                          pq_awvalid_o,
    input                           pq_awready_i,
    output  [3:0]                   pq_awid_o,
    output  [RISCV_PLEN-1:0]        pq_awaddr_o,
    output  [7:0]                   pq_awlen_o,
    output                          pq_wvalid_o,
    input                           pq_wready_i,
    output                          pq_wlast_o,
    output  [31:0]                  pq_wstrb_o,
    output  [255:0]                 pq_wdata_o,

    input                           pq_bvalid_i,
    output                          pq_bready_o,
    input [3:0]                     pq_bid_i,
    input [1:0]                     pq_bresp_i
);

    localparam  logic   [1:0]   BURST_INCR                  = 2'b01;
    localparam  logic   [1:0]   RESP_OKAY                   = 2'b00;

    // FSM states
    enum logic [2:0] {
    IDLE    ,
    AW_REQ  ,
    W_DATA  ,
    B_RESP  ,
    ERROR
    } pq_cs,pq_ns;

    localparam logic [31:0]     RO_REG_FQT  = 32'd0;
    //----------------------------
    //#  IOMMU Fault Queue Structs
    //----------------------------

    typedef struct packed {
        logic [63:0]    iotval2;
        logic [63:0]    iotval;
        logic [31:0]    reserved;
        logic [31:0]    custom;
        logic [63:0]    req;
    } pq_entry_t;


    logic [3:0]             pq_csr;
    logic                   pq_en;
    logic                   pq_en_d;
    logic                   pq_on;
    logic                   pq_ie;
    logic                   pq_mf_clr;
    logic                   pq_of_clr;

    logic                   pq_busy;
    logic                   pq_mf;
    logic                   pq_of;
    logic                   pq_msi_int;
    logic                   iommu_pip_en;

    // Physical pointer to access memory
    logic [255:0]           pq_wdata;
    logic [RISCV_PLEN-1:0]  pq_pptr_q;
    logic [31:0]            pq_tail;
    // To mask the input tail index according to the size of the CQ
    logic [31:0]            masked_tail;
    logic [31:0]            masked_head;
//    assign          masked_tail = pq_tail_i & ~({32{1'b1}} << (iommu_pqb_size_i+1));

    pq_entry_t              pq_entry_q;

    logic                   pq_rfifo_empty;
    logic                   pq_rfifo_ren;
    logic [127:0]           pq_rfifo_dout;


atd_sync_fifo
    #(
    .FIFO_WID       (128),
    .FIFO_DEPTH_WID (4),
    .FIFO_DEPTH     (16),
    .REG_OUT        (0)
    )
u0_pq_rfifo(
           .clk       (iommu_clk),
           .rstn      (iommu_rstn),
           .wen       (pq_pri_wen_i && pq_en),
           .ren       (pq_rfifo_ren ),
           .din       (pq_pri_wdata_i),
           .dout      (pq_rfifo_dout),
           .full      (pq_pri_full_o),
           .empty     (pq_rfifo_empty)
   );


// Default values
// AXI parameters
// AW
assign pq_awid_o = 4'b0001;
assign pq_awaddr_o = {pq_pptr_q[RISCV_PLEN-1:5],5'd0};
assign pq_awlen_o  = 8'd0;         // FQ records are 32-bytes wide

assign pq_awvalid_o = pq_cs == AW_REQ;
assign pq_wdata_o = pq_wdata;
// W
// Must be set on each transfer


assign pq_rfifo_ren = (pq_cs == W_DATA) && pq_wready_i;

always@(*) begin
    if (pq_rfifo_ren)
        pq_wdata = {128'd0,pq_rfifo_dout};
    else
        pq_wdata = 256'd0;
end

assign pq_wstrb_o = 32'h0000ffff;
// Must be set in the last transfer
assign pq_wlast_o = pq_rfifo_ren;

// Send data through W channel
assign pq_wvalid_o = pq_cs == W_DATA;

// B
assign pq_bready_o = ((pq_cs == B_RESP) && pq_bvalid_i);

//pq input signals register
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_csr <= 4'd0;
    else
        pq_csr <= iommu_pqcsr_i;
end

//input signals
assign pq_en = pq_csr[0];// CQ active bit. Indicates to SW whether the CQ is active or not
assign pq_ie = pq_csr[1];// CQ interrupt enable bit from cqcsr, handled by SW
assign pq_mf_clr = pq_csr[2];
assign pq_of_clr = pq_csr[3];


assign pq_busy = (pq_en || pq_on) && !pq_en_d;
assign iommu_pqcsr_o = {pq_busy,pq_on,pq_of,pq_mf};

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_en_d <= 1'b0;
    else
        pq_en_d <= pq_en;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_on <= 1'b0;
    else if (!pq_en && (pq_cs == IDLE))
        pq_on <= 1'b0;
    else if (pq_en && !pq_en_d)
        pq_on <= 1'b1;
    else
        pq_on <= pq_on;
end

//assign  error_vector = (pq_mf_i | pq_of_i);
assign masked_tail = pq_tail & ~({32{1'b1}} << (iommu_pqb_size_i+1));
assign masked_head = (iommu_pqh_i - 1) & ~({32{1'b1}} << (iommu_pqb_size_i+1));

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_cs <= IDLE;
    else
        pq_cs <= pq_ns;
end


always@(*) begin
    case (pq_cs)
        IDLE:   // CQ fetch is automatically triggered when head != tail and CQ is enabled
            if (pq_en_d && !pq_rfifo_empty && (masked_tail == masked_head))
                pq_ns = ERROR;
            else if (pq_en_d && (!pq_rfifo_empty))
                pq_ns = AW_REQ;
            else
                pq_ns = IDLE;
        AW_REQ:
            if (pq_awready_i)
                pq_ns = W_DATA;
            else
                pq_ns = AW_REQ;
        W_DATA:
            if (pq_wready_i)
                pq_ns = B_RESP;
            else
                pq_ns = W_DATA;
        B_RESP:
            if (pq_bvalid_i && (pq_bresp_i != RESP_OKAY))
                pq_ns = ERROR;
            else if (pq_bvalid_i && (pq_bresp_i == RESP_OKAY))
                pq_ns = IDLE;
            else
                pq_ns = B_RESP;
        ERROR:
            if (pq_mf_clr || pq_of_clr)
                pq_ns = IDLE;
            else
                pq_ns = ERROR;
        default:
             pq_ns = IDLE;
    endcase
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_pptr_q <= 'd0;
    else if ((pq_cs == IDLE) && pq_en_d && (!pq_rfifo_empty) && (masked_tail != masked_head))
        pq_pptr_q <= ({iommu_pqb_ppn_i, 12'b0}) | ({19'd0,masked_tail, 5'b0});
    else
        pq_pptr_q <= pq_pptr_q;
end

assign iommu_pqt_o = pq_tail;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_tail <= RO_REG_FQT;
    else if (!pq_en_d && pq_en)//rising_edge
        pq_tail <= RO_REG_FQT;
    else if ((pq_cs == B_RESP) && pq_bvalid_i && (pq_bresp_i != RESP_OKAY))
        pq_tail <= pq_tail;
    else if ((pq_cs == B_RESP) && pq_bvalid_i)
        pq_tail <= (pq_tail + 1'b1) & ~({32{1'b1}} << (iommu_pqb_size_i+1'b1));    // Increment pqt
    else
        pq_tail <= pq_tail;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_mf <= 1'b0;
    else if (pq_mf_clr || (pq_en && !pq_en_d))//rising_edge
        pq_mf <= 1'b0;
    else if ((pq_cs == B_RESP) && pq_bvalid_i && (pq_bresp_i != RESP_OKAY))
        pq_mf <= 1'b1;
    else
        pq_mf <= pq_mf;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_of <= 1'b0;
    else if (pq_of_clr || (pq_en && !pq_en_d))//rising_edge
        pq_of <= 1'b0;
    // If a fault that must be reported occurs and the FQ is full, set pq_of and signal error
    else if ((pq_cs == IDLE) && pq_en_d && !pq_rfifo_empty && (masked_tail == masked_head))
        pq_of <= 1'b1;
    else
        pq_of <= pq_of;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        pq_msi_int <= 1'b0;
    else if (iommu_pip_clr_i)
        pq_msi_int <= 1'b0;
    else if ((pq_cs == B_RESP) && pq_bvalid_i && (pq_bresp_i == RESP_OKAY))
        pq_msi_int <= 1'b1;
    else
        pq_msi_int <= pq_msi_int;
end

 // To set pip bit in ipsr register if a fault occurs and pq_ie is set
assign iommu_pip_en_o = iommu_pip_en;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_pip_en <= 1'b0;
    else if (iommu_pip_clr_i)
        iommu_pip_en <= 1'b0;
    else if (pq_ie && (pq_mf || pq_of || pq_msi_int))
        iommu_pip_en <= 1'b1;
    else
        iommu_pip_en <= iommu_pip_en;
end


endmodule
