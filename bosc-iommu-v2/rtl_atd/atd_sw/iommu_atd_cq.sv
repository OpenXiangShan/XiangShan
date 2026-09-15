///*********************************************//
//compony:bosc
//
//Project       :   rv_iommu_bosc
//FileName      :   iommu_atd_cq.sv
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

module iommu_atd_cq #(
    parameter  RISCV_PLEN   = 56
) (
    input                           iommu_clk,
    input                           iommu_rstn,

// Base address of the CQ in memory (Should be aligned. See Spec)
// Size of the CQ as log2-1 (2 entries: 0 | 4 entries: 1 | 8 entries: 2 | ...)
    input   [43:0]                  iommu_cqb_ppn_i,
    input   [4:0]                   iommu_cqb_size_i,
    input   [31:0]                  iommu_cqt_i,

    input   [5:0]                   iommu_cqcsr_i,
    output  [5:0]                   iommu_cqcsr_o,
    input                           iommu_cip_clr_i,
    output                          iommu_cip_en_o,
// CQ head index (the IOMMU reads the next entry from cq_base + cq_head * 16 bytes)
    output  [31:0]                  iommu_cqh_o,

    //to atd-cache
    output                          cq_atd_fifo_wen_o,
    output  [127:0]                 cq_atd_fifo_din_o,
    input                           cq_atd_fifo_full_i,

    output                          cq_atd_fence_valid_o,
    input                           cq_atd_fence_ready_i,

    //to atd_tc to acd
    output                          cq_acd_fifo_wen_o,
    output  [127:0]                 cq_acd_fifo_din_o,
    input                           cq_acd_fifo_full_i,

    input                           cq_acd_fence_ready_i,//from acd
    input                           cq_acd_dvm_ready_i,//from acd
    input                           cq_ats_fifo_full_i,
    input                           cq_atc_fence_ready_i,//from pcie-rp

    //to msi
    output                          cq_iofence_wvalid_o,
    output [63:0]                   cq_iofence_waddr_o,
    output [31:0]                   cq_iofence_wdata_o,
    input                           cq_iofence_wready_i,

    //DVM sync//acsnoop=0b1111
    input                           cq_acvalid_i,
    output                          cq_acready_o,
    input  [52-1:0]                 cq_acaddr_i,
    input  [3:0]                    cq_acvmidext_i,
    input  [3:0]                    cq_acsnoop_i,
    input  [2:0]                    cq_acprot_i,
    //support dvm,crresp=0b0000
    //not support dvm,crresp=0b0010
    output                          cq_crvalid_o,
    input                           cq_crready_i,
    output  [4:0]                   cq_crresp_o,

    //DVM complete//arsnoop=0b1110
    output  [1:0]                   cq_ardomain_o,
    output  [3:0]                   cq_arsnoop_o,
    output  [1:0]                   cq_arbar_o,

    output                          cq_arvalid_o,
    input                           cq_arready_i,
    output  [3:0]                   cq_arid_o,
    output  [RISCV_PLEN-1:0]        cq_araddr_o,
    output  [7:0]                   cq_arlen_o,
    input                           cq_rvalid_i,
    output                          cq_rready_o,
    input                           cq_rlast_i,
    input   [3:0]                   cq_rid_i,
    input   [1:0]                   cq_rresp_i,
    input   [255:0]                 cq_rdata_i
);


    localparam logic            CQ_WSI_EN   = 1'b0;

    localparam logic [1:0]      RESP_OKAY   = 2'b00;
    localparam logic [1:0]      BURST_FIXED = 2'b00;
    localparam logic [1:0]      BURST_INCR  = 2'b01;

    // Func3
    localparam logic [2:0]      VMA         = 3'b000;
    localparam logic [2:0]      GVMA        = 3'b001;

    localparam logic [2:0]      DDT         = 3'b000;
    localparam logic [2:0]      PDT         = 3'b001;


    // FSM states
    enum logic [2:0] {
    IDLE        ,
    DVM_SYNC    ,
    FETCH       ,
    DECODE      ,
    DVM_COMP    ,
    IOFENCE_END ,
    ERROR
    } cq_cs,cq_ns;

    localparam logic [31:0]     RO_REG_CQH  = 32'd0;

    // Generic CQ entry (used to check type of command)
    typedef struct packed {
        logic [117:0]   operands;
        logic [2:0]     func3;
        logic [6:0]     opcode;
    } cq_entry_t;

    // IOTLB Invalidation Command
    typedef struct packed {
        logic [1:0]     rsv_4;
        logic [51:0]    addr;           // Actually VPN... Named 'ADDR' to match with Spec document
        logic [13:0]    rsv_3;
        logic [15:0]    gscid;
        logic [9:0]     rsv_2;
        logic           gv;
        logic           pscv;
        logic [19:0]    pscid;
        logic           rsv_1;
        logic           av;
        logic [2:0]     func3;
        logic [6:0]     opcode;

    } cq_iotinval_t;

    // CQ IO Fence command
    typedef struct packed {
        logic [1:0]     rsv_2;
        logic [61:0]    addr;
        logic [31:0]    data;
        logic [17:0]    rsv_1;
        logic           pw;
        logic           pr;
        logic           wsi;
        logic           av;
        logic [2:0]     func3;
        logic [6:0]     opcode;
    } cq_iofence_t;

    // Context Directory Cache Invalidation Commands
    typedef struct packed {
        logic [63:0]    rsv_4;
        logic [23:0]    did;
        logic [5:0]     rsv_3;
        logic           dv;
        logic           rsv_2;
        logic [19:0]    pid;
        logic [1:0]     rsv_1;
        logic [2:0]     func3;
        logic [6:0]     opcode;
    } cq_iodirinval_t;

    // Pcie ATS.inval Commands
    typedef struct packed {
        logic [51:0]    addr;//untranslated_addr
        logic           s;
        logic [9:0]     zero;
        logic           g;
        logic [23:0]    did;
        logic [5:0]     rsv_2;
        logic           dsv;
        logic           pv;
        logic [19:0]    pid;
        logic [1:0]     rsv_1;
        logic [2:0]     func3;
        logic [6:0]     opcode;//0x0
    } cq_ats_inval_t;

    // Pcie ATS.prgr Commands
    typedef struct packed {
        logic [15:0]    zero_3;
        logic [3:0]     resp_code;
        logic [2:0]     zero_2;
        logic [8:0]     grp_idx;
        logic [31:0]    zero_1;
        logic [23:0]    did;
        logic [5:0]     rsv_2;
        logic           dsv;
        logic           pv;
        logic [19:0]    pid;
        logic [1:0]     rsv_1;
        logic [2:0]     func3;
        logic [6:0]     opcode;//0x1
    } cq_ats_prgr_t;


//input signals
    logic                       cq_atd_fifo_full;
    logic   [5:0]               cq_csr;
    logic                       cq_en;
    logic                       cq_en_d;
    logic                       cq_on;
    logic                       cq_ie;
    logic                       cq_mf_clr;
    logic                       cmd_to_clr;
    logic                       cmd_ill_clr;
    logic                       fence_w_ip_clr;

    logic                       cq_busy;             // CQ busy bit. Indicates SW that the CQ is in the middle of a state transition,
    logic                       cq_mf;            // Set when a memory fault occurred during CQ access
    logic                       cq_cmd_to;
    logic                       cq_cmd_ill;
    logic                       fence_w_ip;
    logic                       iommu_cip_en;

    // To mask the input head index according to the size of the CQ
    logic   [31:0]              masked_head;
    logic   [31:0]              cq_head;

    logic                       cq_error_flag;
    // Physical pointer to access memory
    logic [RISCV_PLEN-1:0]      cq_pptr_q;

    logic                       cq_fifo_wen;
    logic   [127:0]             cq_fifo_wdata;
    logic   [127:0]             cq_entry_rdata;
    logic   [127:0]             cq_entry_rdata_q;

    logic                       cq_iofence_atd_valid;
    logic                       cq_iofence_atd_end;
    logic                       cq_iofence_acd_end;
    logic                       cq_iofence_atc_end;
    logic                       cq_sync_acd_end;
    logic                       cq_sync_end;

    logic                       cq_iofence_end;
    logic                       cq_iofence_wvalid;
    logic [63:0]                cq_iofence_waddr;
    logic [31:0]                cq_iofence_wdata;

    // Cast read bus to receive CQ entries from memory
    cq_entry_t                  cq_entry;
    cq_iotinval_t               cmd_iotinval;
    cq_iofence_t                cmd_iofence;
    cq_iodirinval_t             cmd_iodirinval;
    cq_ats_inval_t              cmd_ats_inval;
    cq_ats_prgr_t               cmd_ats_prgr;

    //dvm signal defination
    logic                       dvm_arvalid;
    logic                       cq_crvalid;
    logic   [51:0]              dvm_inv_raddr1;
    logic   [3:0]               dvm_inv_vmidext1;
    logic                       dvm_inv_raddr2_en;
    logic                       dvm_inv_en;
    logic                       dvm_sync_en;
    logic   [51:0]              dvm_inv_raddr2;
    logic   [3:0]               dvm_inv_vmidext2;
    logic   [44:0]              va;
    logic   [44:0]              va_q;
    logic   [15:0]              vmid;
    logic   [15:0]              vmid_q;
    logic   [19:0]              asid;
    logic   [1:0]               hypervisor;
    logic                       vmid_valid;
    logic                       asid_valid;
    logic                       leaf;
    logic   [1:0]               stage;
    logic                       av;
    logic   [2:0]               inv_func;
    logic   [2:0]               inv_func_q;

    logic                       dvm_rfifo_wen;
    logic   [127:0]             dvm_rfifo_wdata;
    logic                       dvm_rfifo_ren;
    logic   [127:0]             dvm_rfifo_rdata;
    logic                       dvm_rfifo_full;
    logic                       dvm_rfifo_empty;

////////**********************************************************************************************************///////
//axi4 interface siganls
////////**********************************************************************************************************///////
// AR
assign cq_arid_o    = 4'b0010;
assign cq_araddr_o  = {cq_pptr_q[RISCV_PLEN-1:5],5'd0};            // Physical address to access
assign cq_arlen_o   = 8'd0;                 // CQ entries are 32-bytes wide (1 beat)
assign cq_ardomain_o = 2'd0;//2'd3;
assign cq_arsnoop_o = 4'd0;//4'b1110;
assign cq_arbar_o = 2'b00;

//cq_atd_fifo_full_i = !atd_invalid_req_ready
assign cq_atd_fifo_full = !cq_atd_fifo_full_i;
assign cq_arvalid = (cq_cs == FETCH) && cq_atd_fifo_full_i && !cq_acd_fifo_full_i && !cq_ats_fifo_full_i;
assign dvm_arvalid = (cq_cs == DVM_COMP);
assign cq_arvalid_o = cq_arvalid || dvm_arvalid;
// R
assign cq_rready_o = (((cq_cs == DECODE) && cq_rvalid_i) || (cq_cs == ERROR));

assign cq_entry         = cq_entry_t'(cq_entry_rdata);
assign cmd_iotinval     = cq_iotinval_t'(cq_entry_rdata);
assign cmd_iofence      = cq_iofence_t'(cq_entry_rdata);
assign cmd_iodirinval   = cq_iodirinval_t'(cq_entry_rdata);
assign cmd_ats_inval    = cq_ats_inval_t'(cq_entry_rdata);
assign cmd_ats_prgr     = cq_ats_prgr_t'(cq_entry_rdata);

//CQ busy bit. Indicates SW that the CQ is in the middle of a state transition,
//so it has to wait to write to cqcsr
assign cq_busy = (cq_en || cq_on) && !cq_en_d;
assign iommu_cqcsr_o = {cq_busy,cq_on,fence_w_ip,cq_cmd_ill,cq_cmd_to,cq_mf};

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_csr <= 6'd0;
    else
        cq_csr <= iommu_cqcsr_i;
end


////////**********************************************************************************************************///////
//input cqcsr register signals
////////**********************************************************************************************************///////
assign cq_en = cq_csr[0];// CQ active bit. Indicates to SW whether the CQ is active or not
assign cq_ie = cq_csr[1];// CQ interrupt enable bit from cqcsr, handled by SW
assign cq_mf_clr = cq_csr[2];
assign cmd_to_clr = cq_csr[3];
assign cmd_ill_clr = cq_csr[4];
assign fence_w_ip_clr = cq_csr[5];

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_en_d <= 1'b0;
    else
        cq_en_d <= cq_en;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_on <= 1'b0;
    else if (!cq_en && (cq_cs == IDLE))
        cq_on <= 1'b0;
    else if (cq_en && !cq_en_d)
        cq_on <= 1'b1;
    else
        cq_on <= cq_on;
end

assign masked_head = cq_head & ~({32{1'b1}} << (iommu_cqb_size_i+1));

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_cs <= IDLE;
    else
        cq_cs <= cq_ns;
end


always@(*) begin
    case (cq_cs)
        IDLE:
            if (cq_acvalid_i && (cq_acsnoop_i == 4'b1111) && (cq_acaddr_i[14:12] == 3'd4))//sync
                cq_ns = DVM_SYNC;
            // CQ fetch is automatically triggered when head != tail and CQ is enabled
            else if (cq_en_d && (iommu_cqt_i != masked_head))
                cq_ns = FETCH;
            else
                cq_ns = IDLE;
        DVM_SYNC:
            if (cq_sync_end)
                cq_ns = DVM_COMP;
            else
                cq_ns = DVM_SYNC;
        FETCH:
            if (cq_arready_i && cq_atd_fifo_full_i && !cq_acd_fifo_full_i && !cq_ats_fifo_full_i)
                cq_ns = DECODE;
            else
                cq_ns = FETCH;
        DECODE:
            if (cq_error_flag)
                cq_ns = ERROR;
            else if (cq_rvalid_i && cq_rlast_i)
                case (cq_entry.opcode)
                    7'd1: //IOTINVAL
                        cq_ns = IDLE;
                    7'd2: //IOFENCE
                        cq_ns = IOFENCE_END;
                    7'd3: //IODIR
                        cq_ns = IDLE;
                    7'd4: //ATS
                        cq_ns = IDLE;
                    default:
                        cq_ns = ERROR;
                endcase
             else
         cq_ns = DECODE;
        IOFENCE_END:
            if ((cq_iofence_end && !cmd_iofence.av) || (cq_iofence_wvalid && cq_iofence_wready_i && cmd_iofence.av))
                cq_ns = IDLE;
            else
                cq_ns = IOFENCE_END;
        DVM_COMP:
            if (dvm_arvalid && cq_arready_i)
                cq_ns = IDLE;
            else
                cq_ns = DVM_COMP;
        ERROR:
            if (cq_mf_clr || cmd_to_clr || cmd_ill_clr || fence_w_ip_clr)
                cq_ns = IDLE;
            else
                cq_ns = ERROR;
        default:
             cq_ns = IDLE;
    endcase
end


always@(*) begin
    if ((cq_cs == DECODE) && cq_rvalid_i && cq_rlast_i && (cq_rresp_i != RESP_OKAY))
        cq_error_flag = 1'b1;
    else if ((cq_cs == DECODE) && cq_rvalid_i && cq_rlast_i && (cq_entry.opcode == 7'd1) &&
                            ((|cmd_iotinval.rsv_1) || (|cmd_iotinval.rsv_2) ||
                             (|cmd_iotinval.rsv_3) || (|cmd_iotinval.rsv_4) ||
                             ((cmd_iotinval.func3 == GVMA) && cmd_iotinval.pscv)))
        cq_error_flag = 1'b1;
    else if ((cq_cs == DECODE) && cq_rvalid_i && cq_rlast_i && (cq_entry.opcode == 7'd2) && ((|cmd_iofence.rsv_1) || (|cmd_iofence.rsv_2)))
        cq_error_flag = 1'b1;
    else if ((cq_cs == DECODE) && cq_rvalid_i && cq_rlast_i && (cq_entry.opcode == 7'd3) &&
                            ((|cmd_iodirinval.rsv_1) || (|cmd_iodirinval.rsv_2)  ||
                            (|cmd_iodirinval.rsv_3) || (|cmd_iodirinval.rsv_4)   ||
                            ((cmd_iodirinval.func3 == DDT) && (|cmd_iodirinval.pid)) ||
                            ((cmd_iodirinval.func3 == PDT) && !cmd_iodirinval.dv)))
        cq_error_flag = 1'b1;
    else if ((cq_cs == DECODE) && cq_rvalid_i && cq_rlast_i && (cq_entry.opcode == 7'd4) &&
                            ((|cmd_ats_inval.rsv_1) || (|cmd_ats_inval.rsv_2) || (|cmd_ats_inval.zero) ||
                            (|cmd_ats_prgr.zero_1) || (|cmd_ats_prgr.zero_2) || (|cmd_ats_prgr.zero_3) ||
                            (|cmd_ats_prgr.rsv_1) || (|cmd_ats_prgr.rsv_2)))//ATS
        cq_error_flag = 1'b1;
    else if ((cq_cs == DECODE) && cq_rvalid_i && cq_rlast_i && (cq_entry.opcode == 7'd0))//RSV
        cq_error_flag = 1'b1;
    else
        cq_error_flag = 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_pptr_q <= 'd0;
    else if (cq_ns == ERROR)
        cq_pptr_q <= 'd0;
    else if ((cq_cs == IDLE) && (cq_en_d) && (iommu_cqt_i != masked_head))
        cq_pptr_q <= {iommu_cqb_ppn_i, 12'b0} | {20'd0,masked_head, 4'b0};
    else
        cq_pptr_q <= cq_pptr_q;
end


assign iommu_cqh_o = cq_head;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_head <= RO_REG_CQH;
    else if (!cq_en_d && cq_en)//rising_edge
        cq_head <= RO_REG_CQH;
    else if ((cq_cs == DECODE) && (cq_entry.opcode != 7'd2) && cq_rvalid_i && cq_rlast_i)
        cq_head <= (cq_head + 1) & ~({32{1'b1}} << (iommu_cqb_size_i + 1));  // head is incremented after fetching a command
//    else if ((cq_cs == IOFENCE_END) && cq_iofence_wvalid && cq_iofence_wready_i)
    else if ((cq_cs == IOFENCE_END) && ((cq_iofence_end && !cmd_iofence.av) || (cq_iofence_wvalid && cq_iofence_wready_i && cmd_iofence.av)))
        cq_head <= (cq_head + 1) & ~({32{1'b1}} << (iommu_cqb_size_i + 1));  // head is incremented after fence wrote msi
    else
        cq_head <= cq_head;
end


// Set when a memory fault occurred during CQ access
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_mf <= 1'b0;
    else if (cq_mf_clr || (cq_en && !cq_en_d)) //rising_edge
        cq_mf <= 1'b0;
    else if ((cq_cs == DECODE) && cq_rvalid_i && cq_rlast_i && (cq_rresp_i != RESP_OKAY))
        cq_mf <= 1'b1;
    else
        cq_mf <= cq_mf;
end

 // Illegal or unsupported command was fetched from CQ
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_cmd_ill <= 1'b0;
    else if (cmd_ill_clr || (cq_en && !cq_en_d)) //rising_edge
        cq_cmd_ill <= 1'b0;
    else if ((cq_cs == DECODE) && cq_rvalid_i && cq_rlast_i && (cq_rresp_i != RESP_OKAY))
        cq_cmd_ill <= 1'b1;
    else if (cq_ns == ERROR)
        cq_cmd_ill <= 1'b1;
    else
        cq_cmd_ill <= cq_cmd_ill;
end


// The execution of a command lead to a timeout //! Future work for PCIe ATS
// it maybe need to add iofence timeout
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_cmd_to <= 1'b0;
    else if (cmd_to_clr || (cq_en && !cq_en_d)) //rising_edge
        cq_cmd_to <= 1'b0;
    else if (cq_en_d && (cq_ats_fifo_full_i || cq_acd_fifo_full_i || cq_atd_fifo_full))
        cq_cmd_to <= 1'b1;
    else
        cq_cmd_to <= cq_cmd_to;
end

// Set to indicate completion of an IOFENCE command
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        fence_w_ip <= 1'b0;
    else if (fence_w_ip_clr || (cq_en && !cq_en_d))//rising_edge
        fence_w_ip <= 1'b0;
    else if ((cq_cs == DECODE) && (cq_entry.opcode == 7'd2) && ((|cmd_iofence.rsv_1) || (|cmd_iofence.rsv_2)))
        fence_w_ip <= 1'b0;
    else if ((cq_cs == DECODE) && (cq_entry.opcode == 7'd2) && cmd_iofence.wsi && CQ_WSI_EN)
        fence_w_ip <= 1'b1;
    else
        fence_w_ip <= fence_w_ip;
end

 // To set cip bit in ipsr register if a fault occurs and cq_ie is set
assign iommu_cip_en_o = iommu_cip_en;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        iommu_cip_en <= 1'b0;
    else if (iommu_cip_clr_i)
        iommu_cip_en <= 1'b0;
    else if (cq_ie && (cq_mf || cq_cmd_to || cq_cmd_ill || fence_w_ip))
        iommu_cip_en <= 1'b1;
    else
        iommu_cip_en <= iommu_cip_en;
end

////////**********************************************************************************************************///////
//gen cq output signal to atd-cache or atd-dti
////////**********************************************************************************************************///////
assign cq_atd_fifo_wen_o = cq_fifo_wen;
assign cq_atd_fifo_din_o = cq_fifo_wdata;
assign cq_atd_fence_valid_o = cq_iofence_atd_valid;
assign cq_acd_fifo_wen_o = cq_fifo_wen;
assign cq_acd_fifo_din_o = cq_fifo_wdata;


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_fifo_wen <= 1'b0;
    else if ((cq_cs == DECODE) && (!cq_error_flag) && cq_rvalid_i && cq_rlast_i)
        cq_fifo_wen <= 1'b1;
    else if (!dvm_rfifo_empty && cq_atd_fifo_full_i && !cq_acd_fifo_full_i && !cq_ats_fifo_full_i)
        cq_fifo_wen <= 1'b1;
    else
        cq_fifo_wen <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        dvm_rfifo_ren <= 1'b0;
    else if ((cq_cs == DECODE) && (!cq_error_flag) && cq_rvalid_i && cq_rlast_i)
        dvm_rfifo_ren <= 1'b0;
    else if (!dvm_rfifo_empty && cq_atd_fifo_full_i && !cq_acd_fifo_full_i && !cq_ats_fifo_full_i)
        dvm_rfifo_ren <= 1'b1;
    else
        dvm_rfifo_ren <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_fifo_wdata <= 128'd0;
    else if ((cq_cs == DECODE) && cq_rvalid_i && cq_rlast_i && cq_pptr_q[4])
        cq_fifo_wdata <= cq_rdata_i[255:128];
    else if ((cq_cs == DECODE) && cq_rvalid_i && cq_rlast_i)
        cq_fifo_wdata <= cq_rdata_i[127:0];
    else if (!dvm_rfifo_empty && cq_atd_fifo_full_i && !cq_acd_fifo_full_i && !cq_ats_fifo_full_i)
        cq_fifo_wdata <= dvm_rfifo_rdata;
    else
        cq_fifo_wdata <= cq_fifo_wdata;
end


always@(*) begin
    if (!iommu_rstn)
        cq_entry_rdata = 128'd0;
    else if ((cq_cs == DECODE) && cq_rvalid_i && cq_rlast_i && cq_pptr_q[4])
        cq_entry_rdata = cq_rdata_i[255:128];
    else if ((cq_cs == DECODE) && cq_rvalid_i && cq_rlast_i)
        cq_entry_rdata = cq_rdata_i[127:0];
    else
        cq_entry_rdata = cq_entry_rdata_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_entry_rdata_q <= 128'd0;
    else
        cq_entry_rdata_q <= cq_entry_rdata;
end


////////**********************************************************************************************************///////
//gen iofence valid and end
//glue logic is different between atd_cache and acd/atc
////////**********************************************************************************************************///////
//atd iofence end
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_iofence_atd_valid <= 1'b0;
    else if ((cq_cs == DECODE) && (cq_entry.opcode == 7'd2) && cq_rvalid_i && cq_rlast_i)
        cq_iofence_atd_valid <= 1'b1;
    else if (cq_cs == DVM_SYNC)
        cq_iofence_atd_valid <= 1'b1;
    else if (cq_atd_fence_ready_i)
        cq_iofence_atd_valid <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_iofence_atd_end <= 1'b0;
    else if (cq_iofence_end)
        cq_iofence_atd_end <= 1'b0;
    else if (cq_iofence_atd_valid && cq_atd_fence_ready_i)
        cq_iofence_atd_end <= 1'b1;
end

//acd end
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_iofence_acd_end <= 1'b0;
    else if (cq_iofence_end)
        cq_iofence_acd_end <= 1'b0;
    else if (cq_acd_fence_ready_i)//pulse
        cq_iofence_acd_end <= 1'b1;
end

//atc end
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_iofence_atc_end <= 1'b0;
    else if (cq_iofence_end)
        cq_iofence_atc_end <= 1'b0;
    else if (cq_atc_fence_ready_i)//pulse
        cq_iofence_atc_end <= 1'b1;
end

//all end
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_iofence_end <= 1'b0;
    else if (cq_iofence_end)
        cq_iofence_end <= 1'b0;
//    else if (cq_iofence_atd_end && cq_iofence_acd_end && cq_iofence_atc_end)
    else if (cq_iofence_atd_end && cq_iofence_acd_end)
        cq_iofence_end <= 1'b1;
end

////////**********************************************************************************************************///////
//gen dvm tlbi sync to acd ready
////////**********************************************************************************************************///////
//acd dvm end
always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_sync_acd_end <= 1'b0;
    else if (cq_sync_end)
        cq_sync_acd_end <= 1'b0;
    else if (cq_acd_dvm_ready_i)//pulse
        cq_sync_acd_end <= 1'b1;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_sync_end <= 1'b0;
    else if (cq_sync_end)
        cq_sync_end <= 1'b0;
    else if (cq_iofence_atd_end && cq_sync_acd_end)
        cq_sync_end <= 1'b1;
end

////////**********************************************************************************************************///////
//gen cq output signal to atd-cache or atd-dti
////////**********************************************************************************************************///////
assign cq_iofence_wvalid_o = cq_iofence_wvalid;
assign cq_iofence_waddr_o = cq_iofence_waddr;
assign cq_iofence_wdata_o = cq_iofence_wdata;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_iofence_wvalid <= 1'b0;
    else if (cq_iofence_end && cmd_iofence.av)
        cq_iofence_wvalid <= 1'b1;
    else if (cq_iofence_wready_i)
        cq_iofence_wvalid <= 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_iofence_waddr <= 64'd0;
    // "If AV=1, the IOMMU writes DATA to memory at a 4-byte aligned address ADDR[63:2] * 4"
    else if (cq_iofence_end)
        cq_iofence_waddr <= {cmd_iofence.addr, 2'b0};
    else if (cq_iofence_wready_i)
        cq_iofence_waddr <= 64'd0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_iofence_wdata <= 32'd0;
    else if (cq_iofence_end)
        cq_iofence_wdata <= cmd_iofence.data;
    else if (cq_iofence_wready_i)
        cq_iofence_wdata <= 32'd0;
end


////////**********************************************************************************************************///////
//ace5-liteDVM:tlb invalid
////////**********************************************************************************************************///////
assign cq_acready_o = cq_cs == IDLE;
assign cq_crvalid_o = cq_crvalid;
assign cq_crresp_o = 5'b00000;

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        cq_crvalid <= 1'b0;
    else if (!cq_crvalid && cq_crready_i)
        cq_crvalid <= 1'b0;
    else if (cq_acvalid_i && (cq_acsnoop_i == 4'b1111))
        cq_crvalid <= 1'b1;
    else
        cq_crvalid <= cq_crvalid;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        dvm_inv_raddr1 <= 52'd0;
    else if (cq_acvalid_i && (cq_acsnoop_i == 4'b1111) && (cq_acaddr_i[14:12] == 3'd0) && !dvm_inv_raddr2_en)
        dvm_inv_raddr1 <= cq_acaddr_i;
    else
        dvm_inv_raddr1 <= dvm_inv_raddr1;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        dvm_inv_vmidext1 <= 4'd0;
    else if (cq_acvalid_i && (cq_acsnoop_i == 4'b1111) && (cq_acaddr_i[14:12] == 3'd0) && !dvm_inv_raddr2_en)
        dvm_inv_vmidext1 <= cq_acvmidext_i;
    else
        dvm_inv_vmidext1 <= dvm_inv_vmidext1;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        dvm_inv_raddr2_en <= 1'b0;
    else if (cq_acvalid_i && dvm_inv_raddr2_en && (cq_acsnoop_i == 4'b1111))
        dvm_inv_raddr2_en <= 1'b0;
    else if (cq_acvalid_i && (cq_acsnoop_i == 4'b1111) && (cq_acaddr_i[14:12] == 3'd0) && cq_acaddr_i[0])
        dvm_inv_raddr2_en <= 1'b1;
    else
        dvm_inv_raddr2_en <= dvm_inv_raddr2_en;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        dvm_inv_en <= 1'b0;
    else if (cq_acvalid_i && (cq_acsnoop_i == 4'b1111) && (cq_acaddr_i[14:12] == 3'd0) && !cq_acaddr_i[0])
        dvm_inv_en <= 1'b1;
    else if (cq_acvalid_i && (cq_acsnoop_i == 4'b1111) && (cq_acaddr_i[14:12] == 3'd0) && dvm_inv_raddr2_en)
        dvm_inv_en <= 1'b1;
    else
        dvm_inv_en <= 1'b0;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        dvm_sync_en <= 1'b0;
    else if (cq_acvalid_i && (cq_acsnoop_i == 4'b1111) && (cq_acaddr_i[14:12] == 3'd4))//sync
        dvm_sync_en <= 1'b1;
    else
        dvm_sync_en <= 1'b0;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        dvm_rfifo_wen <= 1'b0;
    else
        dvm_rfifo_wen <= dvm_inv_en || dvm_sync_en;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        dvm_inv_raddr2 <= 52'd0;
    else if (cq_acvalid_i && dvm_inv_raddr2_en && (cq_acsnoop_i == 4'b1111))
        dvm_inv_raddr2 <= cq_acaddr_i;
    else
        dvm_inv_raddr2 <= dvm_inv_raddr2;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        dvm_inv_vmidext2 <= 4'd0;
    else if (cq_acvalid_i && dvm_inv_raddr2_en && (cq_acsnoop_i == 4'b1111))
        dvm_inv_vmidext2 <= cq_acvmidext_i;
    else
        dvm_inv_vmidext2 <= dvm_inv_vmidext2;
end


//4+4+4+4+1+28=45bit
//assign va = {dvm_inv_raddr1[47:44],dvm_inv_raddr2[47:44],dvm_inv_raddr1[43:40],dvm_inv_raddr2[43:40],dvm_inv_raddr2[3],dvm_inv_raddr2[39:12]};
//va
always@(*) begin
    if (!iommu_rstn)
        va = 45'd0;
    else if (dvm_inv_en && !cq_acaddr_i[0] && !dvm_inv_raddr2_en)
        va = 45'd0;
    else if (dvm_inv_en && dvm_inv_raddr2_en)
        va = {dvm_inv_raddr1[47:44],dvm_inv_raddr2[47:44],dvm_inv_raddr1[43:40],dvm_inv_raddr2[43:40],dvm_inv_raddr2[3],dvm_inv_raddr2[39:12]};
    else
        va = va_q;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        va_q <= 45'd0;
    else
        va_q <= va;
end


//gscid
always@(*) begin
    if (!iommu_rstn)
        vmid = 16'd0;
    else if (dvm_inv_en && !cq_acaddr_i[0] && !dvm_inv_raddr2_en)
        vmid = {dvm_inv_raddr1[43:40],dvm_inv_vmidext1,dvm_inv_raddr1[31:24]};
    else if (dvm_inv_en && dvm_inv_raddr2_en)
        vmid = {dvm_inv_vmidext2,dvm_inv_vmidext1,dvm_inv_raddr1[31:24]};
    else
        vmid = vmid_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        vmid_q <= 16'd0;
    else
        vmid_q <= vmid;
end


assign asid = {4'd0,dvm_inv_raddr1[39:32],dvm_inv_raddr1[23:16]};//pscid
assign hypervisor = dvm_inv_raddr1[11:10];
assign vmid_valid = dvm_inv_raddr1[6];//gv
assign asid_valid = dvm_inv_raddr1[5];//pscv
assign leaf = dvm_inv_raddr1[4];
assign stage = dvm_inv_raddr1[3:2];
assign av = leaf && dvm_inv_raddr1[0];


always@(*) begin
    if (!iommu_rstn)
        inv_func = 3'd0;
    else if (dvm_inv_en && (hypervisor == 2'b10) && (stage == 2'b10))
        inv_func = 3'd0;
    else if (dvm_inv_en)
        inv_func = 3'd1;
    else
        inv_func = inv_func_q;
end

always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        inv_func_q <= 16'd0;
    else
        inv_func_q <= inv_func;
end


always@(posedge iommu_clk or negedge iommu_rstn) begin
    if (!iommu_rstn)
        dvm_rfifo_wdata <= 128'd0;
    else if (dvm_inv_en)
        dvm_rfifo_wdata <= {9'd0,va,14'd0,vmid,10'd0,vmid_valid,asid_valid,asid,1'b0,av,inv_func,7'd1};
    else if (dvm_sync_en)
        dvm_rfifo_wdata <= {118'd0,3'd2,7'd2};//func3=3'd2 is costume
    else
        dvm_rfifo_wdata <= dvm_rfifo_wdata;
end


atd_sync_fifo
    #(
    .FIFO_WID       (128),
    .FIFO_DEPTH_WID (4),
    .FIFO_DEPTH     (16),
//  .FIFO_DEPTH_WID (2),
//    .FIFO_DEPTH   (4),
    .REG_OUT        (0)
    )
u0_dvm_sync_fifo(
    .clk                (iommu_clk),
    .rstn               (iommu_rstn),
    .wen                (dvm_rfifo_wen),
    .ren                (dvm_rfifo_ren),
    .din                (dvm_rfifo_wdata),
    .dout               (dvm_rfifo_rdata),
    .full               (dvm_rfifo_full),
    .empty              (dvm_rfifo_empty)
   );



endmodule

