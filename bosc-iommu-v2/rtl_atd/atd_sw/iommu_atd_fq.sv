///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd_fq.sv
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

module iommu_atd_fq #(
//    parameter    RISCV_VLEN   = 64,
    parameter  RISCV_GLEN   = 41,
    parameter  RISCV_PLEN   = 56
) (
    input                           iommu_clk,
    input                           iommu_rstn,

    input   [43:0]                  iommu_fqb_ppn_i,
    input   [4:0]                   iommu_fqb_size_i,
    input   [31:0]                  iommu_fqh_i,

    input   [3:0]                   iommu_fqcsr_i,
    output  [3:0]                   iommu_fqcsr_o,
    input                           iommu_fip_clr_i,
    output                          iommu_fip_en_o,
    output  [31:0]                  iommu_fqt_o,

    //from ACD
    input                           fq_acd_valid_i,
    input   [255:0]                 fq_acd_record_i,
    output                          fq_acd_ready_o,

    //from CDW
    input                           fq_cdw_valid_i,
    input   [255:0]                 fq_cdw_record_i,
    output                          fq_cdw_ready_o,

    //from PTW
    input                           fq_ptw_valid_i,
    input   [255:0]                 fq_ptw_record_i,
    output                          fq_ptw_ready_o,

//    // indicate if transaction has supervisor privilege (only if pid valid)//ax.prot[0]
//    input                           is_supervisor_i,
//    input                           is_guest_pf_i,
//    input                           is_implicit_i,

    //fq
    output                          fq_awvalid_o,
    input                           fq_awready_i,
    output  [3:0]                   fq_awid_o,
    output  [RISCV_PLEN-1:0]        fq_awaddr_o,
    output  [7:0]                   fq_awlen_o,
    output                          fq_wvalid_o,
    input                           fq_wready_i,
    output                          fq_wlast_o,
    output  [31:0]                  fq_wstrb_o,
    output  [255:0]                 fq_wdata_o,

    input                           fq_bvalid_i,
    output                          fq_bready_o,
    input [3:0]                     fq_bid_i,
    input [1:0]                     fq_bresp_i
);


    localparam  logic   [1:0]   BURST_INCR                  = 2'b01;
    localparam  logic   [1:0]   RESP_OKAY                   = 2'b00;

    // FSM states
    enum logic [3:0] {
    IDLE    ,
    ACD_AW_REQ  ,
    ACD_W_DATA  ,
    PTW_AW_REQ  ,
    PTW_W_DATA  ,
    CDW_AW_REQ  ,
    CDW_W_DATA  ,
    B_RESP  ,
    ERROR
    } fq_cs,fq_ns;

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
    } fq_entry_t;


    logic [3:0]             fq_csr;
    logic                   fq_en;
    logic                   fq_en_d;
    logic                   fq_on;
    logic                   fq_ie;
    logic                   fq_mf_clr;
    logic                   fq_of_clr;

    logic                   fq_busy;
    logic                   fq_mf;
    logic                   fq_of;
    logic                   fq_msi_int;
    logic                   iommu_fip_en;

    // Physical pointer to access memory
    logic [255:0]           fq_wdata;
    logic [RISCV_PLEN-1:0]  fq_pptr_q;
    logic [31:0]            fq_tail;
    // To mask the input tail index according to the size of the CQ
    logic [31:0]            masked_tail;
    logic [31:0]            masked_head;
//    assign          masked_tail = fq_tail_i & ~({32{1'b1}} << (iommu_fqb_size_i+1));

    fq_entry_t              fq_entry_q;

    logic                   fq_rfifo_empty;
    logic                   fq_acd_rfifo_empty;
    logic                   fq_acd_rfifo_full;
    logic                   fq_acd_rfifo_ren;
    logic [255:0]           fq_acd_rfifo_dout;
    logic                   fq_cdw_rfifo_empty;
    logic                   fq_cdw_rfifo_full;
    logic                   fq_cdw_rfifo_ren;
    logic [255:0]           fq_cdw_rfifo_dout;
    logic                   fq_ptw_rfifo_empty;
    logic                   fq_ptw_rfifo_full;
    logic                   fq_ptw_rfifo_ren;
    logic [255:0]           fq_ptw_rfifo_dout;


assign fq_acd_ready_o = !fq_acd_rfifo_full;
assign fq_cdw_ready_o = !fq_cdw_rfifo_full;
assign fq_ptw_ready_o = !fq_ptw_rfifo_full;

atd_sync_fifo
    #(
    .FIFO_WID       (256),
    .FIFO_DEPTH_WID (4),
    .FIFO_DEPTH     (16),
    .REG_OUT        (0)
    )
u0_fq_acd_rfifo(
           .clk       (iommu_clk),
           .rstn      (iommu_rstn),
           .wen       (fq_acd_valid_i && fq_en),
           .ren       (fq_acd_rfifo_ren ),
           .din       (fq_acd_record_i),
           .dout      (fq_acd_rfifo_dout),
           .full      (fq_acd_rfifo_full),
           .empty     (fq_acd_rfifo_empty)
   );


atd_sync_fifo
    #(
    .FIFO_WID       (256),
    .FIFO_DEPTH_WID (4),
    .FIFO_DEPTH     (16),
    .REG_OUT        (0)
    )
u1_fq_cdw_rfifo(
           .clk       (iommu_clk),
           .rstn      (iommu_rstn),
           .wen       (fq_cdw_valid_i && fq_en),
           .ren       (fq_cdw_rfifo_ren ),
           .din       (fq_cdw_record_i),
           .dout      (fq_cdw_rfifo_dout),
           .full      (fq_cdw_rfifo_full),
           .empty     (fq_cdw_rfifo_empty)
   );


atd_sync_fifo
    #(
    .FIFO_WID       (256),
    .FIFO_DEPTH_WID (4),
    .FIFO_DEPTH     (16),
    .REG_OUT        (0)
    )
u2_fq_ptw_rfifo(
           .clk       (iommu_clk),
           .rstn      (iommu_rstn),
           .wen       (fq_ptw_valid_i && fq_en),
           .ren       (fq_ptw_rfifo_ren ),
           .din       (fq_ptw_record_i),
           .dout      (fq_ptw_rfifo_dout),
           .full      (fq_ptw_rfifo_full),
           .empty     (fq_ptw_rfifo_empty)
   );


// Default values
// AXI parameters
// AW
assign fq_awid_o = 4'b0000;
assign fq_awaddr_o = {fq_pptr_q[RISCV_PLEN-1:5],5'd0};
assign fq_awlen_o  = 8'd0;         // FQ records are 32-bytes wide

assign fq_awvalid_o = (fq_cs == ACD_AW_REQ) || (fq_cs == PTW_AW_REQ) || (fq_cs == CDW_AW_REQ);
assign fq_wdata_o = fq_wdata;
// W
// Must be set on each transfer


assign fq_acd_rfifo_ren = (fq_cs == ACD_W_DATA) && fq_wready_i;
assign fq_cdw_rfifo_ren = (fq_cs == CDW_W_DATA) && fq_wready_i;
assign fq_ptw_rfifo_ren = (fq_cs == PTW_W_DATA) && fq_wready_i;


always@(*) begin
    if (fq_acd_rfifo_ren)
        fq_wdata = fq_acd_rfifo_dout;
    else if (fq_ptw_rfifo_ren)
        fq_wdata = fq_ptw_rfifo_dout;
    else if (fq_cdw_rfifo_ren)
        fq_wdata = fq_cdw_rfifo_dout;
    else
        fq_wdata = 256'd0;
end

assign fq_wstrb_o = 32'hffff_ffff;
// Must be set in the last transfer
assign fq_wlast_o = fq_acd_rfifo_ren || fq_cdw_rfifo_ren || fq_ptw_rfifo_ren;

// Send data through W channel
assign fq_wvalid_o = (fq_cs == ACD_W_DATA) || (fq_cs == CDW_W_DATA) || (fq_cs == PTW_W_DATA);

// B
assign fq_bready_o = ((fq_cs == B_RESP) && fq_bvalid_i);

//fq input signals register
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fq_csr <= 4'd0;
    else
        fq_csr <= iommu_fqcsr_i;
end

//input signals
assign fq_en = fq_csr[0];// CQ active bit. Indicates to SW whether the CQ is active or not
assign fq_ie = fq_csr[1];// CQ interrupt enable bit from cqcsr, handled by SW
assign fq_mf_clr = fq_csr[2];
assign fq_of_clr = fq_csr[3];


assign fq_busy = (fq_en || fq_on) && !fq_en_d;
assign iommu_fqcsr_o = {fq_busy,fq_on,fq_of,fq_mf};

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fq_en_d <= 1'b0;
    else
        fq_en_d <= fq_en;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fq_on <= 1'b0;
    else if (!fq_en && (fq_cs == IDLE))
        fq_on <= 1'b0;
    else if (fq_en && !fq_en_d)
        fq_on <= 1'b1;
    else
        fq_on <= fq_on;
end

assign fq_rfifo_empty = fq_acd_rfifo_empty && fq_ptw_rfifo_empty && fq_cdw_rfifo_empty;

//assign  error_vector = (fq_mf_i | fq_of_i);
assign masked_tail = fq_tail & ~({32{1'b1}} << (iommu_fqb_size_i+1));
assign masked_head = (iommu_fqh_i - 1) & ~({32{1'b1}} << (iommu_fqb_size_i+1));

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fq_cs <= IDLE;
    else
        fq_cs <= fq_ns;
end


always@(*) begin
    case (fq_cs)
        IDLE:   // CQ fetch is automatically triggered when head != tail and CQ is enabled
            if (fq_en_d && !fq_rfifo_empty && (masked_tail == masked_head))
                fq_ns = ERROR;
            else if (fq_en_d && (!fq_acd_rfifo_empty))
                fq_ns = ACD_AW_REQ;
            else if (fq_en_d && (!fq_ptw_rfifo_empty))
                fq_ns = PTW_AW_REQ;
            else if (fq_en_d && (!fq_cdw_rfifo_empty))
                fq_ns = CDW_AW_REQ;
            else
                fq_ns = IDLE;
        ACD_AW_REQ:
            if (fq_awready_i)
                fq_ns = ACD_W_DATA;
            else
                fq_ns = ACD_AW_REQ;
        ACD_W_DATA:
            if (fq_wready_i)
                fq_ns = B_RESP;
            else
                fq_ns = ACD_W_DATA;
        PTW_AW_REQ:
            if (fq_awready_i)
                fq_ns = PTW_W_DATA;
            else
                fq_ns = PTW_AW_REQ;
        PTW_W_DATA:
            if (fq_wready_i)
                fq_ns = B_RESP;
            else
                fq_ns = PTW_W_DATA;
        CDW_AW_REQ:
            if (fq_awready_i)
                fq_ns = CDW_W_DATA;
            else
                fq_ns = CDW_AW_REQ;
        CDW_W_DATA:
            if (fq_wready_i)
                fq_ns = B_RESP;
            else
                fq_ns = CDW_W_DATA;
        B_RESP:
            if (fq_bvalid_i && (fq_bresp_i != RESP_OKAY))
                fq_ns = ERROR;
            else if (fq_bvalid_i && (fq_bresp_i == RESP_OKAY))
                fq_ns = IDLE;
            else
                fq_ns = B_RESP;
        ERROR:
            if (fq_mf_clr || fq_of_clr)
                fq_ns = IDLE;
            else
                fq_ns = ERROR;
        default:
             fq_ns = IDLE;
    endcase
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fq_pptr_q <= 'd0;
    else if ((fq_cs == IDLE) && fq_en_d && (!fq_rfifo_empty) && (masked_tail != masked_head))
        fq_pptr_q <= ({iommu_fqb_ppn_i, 12'b0}) | ({19'd0,masked_tail, 5'b0});
    else
        fq_pptr_q <= fq_pptr_q;
end

assign iommu_fqt_o = fq_tail;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fq_tail <= RO_REG_FQT;
    else if (!fq_en_d && fq_en)//rising_edge
        fq_tail <= RO_REG_FQT;
    else if ((fq_cs == B_RESP) && fq_bvalid_i && (fq_bresp_i != RESP_OKAY))
        fq_tail <= fq_tail;
    else if ((fq_cs == B_RESP) && fq_bvalid_i)
        fq_tail <= (fq_tail + 1'b1) & ~({32{1'b1}} << (iommu_fqb_size_i+1'b1));    // Increment fqt
    else
        fq_tail <= fq_tail;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fq_mf <= 1'b0;
    else if (fq_mf_clr || (fq_en && !fq_en_d))//rising_edge
        fq_mf <= 1'b0;
    else if ((fq_cs == B_RESP) && fq_bvalid_i && (fq_bresp_i != RESP_OKAY))
        fq_mf <= 1'b1;
    else
        fq_mf <= fq_mf;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fq_of <= 1'b0;
    else if (fq_of_clr || (fq_en && !fq_en_d))//rising_edge
        fq_of <= 1'b0;
    // If a fault that must be reported occurs and the FQ is full, set fq_of and signal error
    else if ((fq_cs == IDLE) && fq_en_d && (!fq_rfifo_empty) && (masked_tail == masked_head))
        fq_of <= 1'b1;
    else
        fq_of <= fq_of;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fq_msi_int <= 1'b0;
    else if (iommu_fip_clr_i || (fq_en && !fq_en_d))
        fq_msi_int <= 1'b0;
    else if ((fq_cs == B_RESP) && fq_bvalid_i && (fq_bresp_i == RESP_OKAY))
        fq_msi_int <= 1'b1;
    else
        fq_msi_int <= fq_msi_int;
end

 // To set fip bit in ipsr register if a fault occurs and fq_ie is set
assign iommu_fip_en_o = iommu_fip_en;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_fip_en <= 1'b0;
    else if (iommu_fip_clr_i)
        iommu_fip_en <= 1'b0;
    else if (fq_ie && (fq_mf || fq_of || fq_msi_int))
        iommu_fip_en <= 1'b1;
    else
        iommu_fip_en <= iommu_fip_en;
end

endmodule
