module ddr_wrapper (
    input  logic         aresetn_0,
    input  logic         aclk_0,

    input  logic [17:0]  awid_0,
    input  logic [35:0]  awaddr_0,
    input  logic [7:0]   awlen_0,
    input  logic [2:0]   awsize_0,
    input  logic [1:0]   awburst_0,
    input  logic         awlock_0,
    input  logic [3:0]   awcache_0,
    input  logic [2:0]   awprot_0,
    input  logic [7:0]   awuser_0,
    input  logic         awvalid_0,
    output logic         awready_0,
    input  logic [3:0]   awqos_0,
    input  logic [3:0]   awregion_0,
    input  logic         awurgent_0,
    input  logic         awpoison_0,
    output logic         awpoison_intr_0,
    input  logic         awautopre_0,

    input  logic [255:0] wdata_0,
    input  logic [31:0]  wstrb_0,
    input  logic         wlast_0,
    input  logic         wvalid_0,
    output logic         wready_0,
    input  logic [7:0]   wuser_0,

    output logic [17:0]  bid_0,
    output logic [1:0]   bresp_0,
    output logic [7:0]   buser_0,
    output logic         bvalid_0,
    input  logic         bready_0,

    input  logic [17:0]  arid_0,
    input  logic [35:0]  araddr_0,
    input  logic [7:0]   arlen_0,
    input  logic [2:0]   arsize_0,
    input  logic [1:0]   arburst_0,
    input  logic         arlock_0,
    input  logic [3:0]   arcache_0,
    input  logic [2:0]   arprot_0,
    input  logic [7:0]   aruser_0,
    input  logic         arvalid_0,
    output logic         arready_0,
    input  logic [3:0]   arqos_0,
    input  logic [3:0]   arregion_0,
    input  logic         arpoison_0,
    output logic         arpoison_intr_0,
    input  logic         arurgent_0,
    input  logic         arautopre_0,

    output logic [17:0]  rid_0,
    output logic [255:0] rdata_0,
    output logic [1:0]   rresp_0,
    output logic [7:0]   ruser_0,
    output logic         rlast_0,
    output logic         rvalid_0,
    input  logic         rready_0,

    output logic         training_over
);
    localparam int AXI_ADDRW = 36;
    localparam int AXI_IDW = 18;
    localparam int AXI_USERW = 8;
    localparam int AXI_CHANNEL_ADDR_BIT = 6;
    localparam int AXI_ASYNC_FIFO_DEPTH = 256;
    localparam int AXI_ROUTE_FIFO_DEPTH = 256;
    localparam int AXI_ROUTE_FIFO_PTRW = $clog2(AXI_ROUTE_FIFO_DEPTH);
    localparam int AXI_ROUTE_FIFO_COUNTW = $clog2(AXI_ROUTE_FIFO_DEPTH + 1);
    localparam logic [AXI_ROUTE_FIFO_COUNTW-1:0] AXI_ROUTE_FIFO_DEPTH_COUNT =
        AXI_ROUTE_FIFO_DEPTH[AXI_ROUTE_FIFO_COUNTW-1:0];
    localparam int CORE_CLK_ACLK_WINDOW_CYCLES = 30;
    localparam int CORE_CLK_POSEDGES_PER_WINDOW = 8;
    localparam int CORE_CLK_ACCUM_WIDTH = $clog2(
        CORE_CLK_ACLK_WINDOW_CYCLES + CORE_CLK_POSEDGES_PER_WINDOW
    );

    logic core_ddrc_core_clk;
    logic [CORE_CLK_ACCUM_WIDTH-1:0] core_clk_phase_accum;
    logic core_ddrc_resetn_meta;
    logic core_ddrc_resetn;
    logic apb_init_done_aclk_meta;
    logic apb_init_done_aclk;

    logic awready_ch0;
    logic awready_ch1;
    logic awvalid_ch0;
    logic awvalid_ch1;
    logic wready_ch0;
    logic wready_ch1;
    logic wvalid_ch0;
    logic wvalid_ch1;
    logic bready_ch0;
    logic bready_ch1;
    logic [AXI_IDW-1:0] bid_ch0;
    logic [AXI_IDW-1:0] bid_ch1;
    logic [1:0] bresp_ch0;
    logic [1:0] bresp_ch1;
    logic [AXI_USERW-1:0] buser_ch0;
    logic [AXI_USERW-1:0] buser_ch1;
    logic bvalid_ch0;
    logic bvalid_ch1;
    logic arready_ch0;
    logic arready_ch1;
    logic arvalid_ch0;
    logic arvalid_ch1;
    logic rready_ch0;
    logic rready_ch1;
    logic [AXI_IDW-1:0] rid_ch0;
    logic [AXI_IDW-1:0] rid_ch1;
    logic [255:0] rdata_ch0;
    logic [255:0] rdata_ch1;
    logic [1:0] rresp_ch0;
    logic [1:0] rresp_ch1;
    logic [AXI_USERW-1:0] ruser_ch0;
    logic [AXI_USERW-1:0] ruser_ch1;
    logic rlast_ch0;
    logic rlast_ch1;
    logic rvalid_ch0;
    logic rvalid_ch1;

    logic awready_mc_ch0;
    logic awready_mc_ch1;
    logic awvalid_mc_ch0;
    logic awvalid_mc_ch1;
    logic [AXI_IDW-1:0] awid_mc_ch0;
    logic [AXI_IDW-1:0] awid_mc_ch1;
    logic [AXI_ADDRW-1:0] awaddr_mc_ch0;
    logic [AXI_ADDRW-1:0] awaddr_mc_ch1;
    logic [7:0] awlen_mc_ch0;
    logic [7:0] awlen_mc_ch1;
    logic [2:0] awsize_mc_ch0;
    logic [2:0] awsize_mc_ch1;
    logic [1:0] awburst_mc_ch0;
    logic [1:0] awburst_mc_ch1;
    logic [AXI_USERW-1:0] awuser_mc_ch0;
    logic [AXI_USERW-1:0] awuser_mc_ch1;
    logic [3:0] awqos_mc_ch0;
    logic [3:0] awqos_mc_ch1;
    logic wready_mc_ch0;
    logic wready_mc_ch1;
    logic wvalid_mc_ch0;
    logic wvalid_mc_ch1;
    logic [AXI_USERW-1:0] wuser_mc_ch0;
    logic [AXI_USERW-1:0] wuser_mc_ch1;
    logic [255:0] wdata_mc_ch0;
    logic [255:0] wdata_mc_ch1;
    logic [31:0] wstrb_mc_ch0;
    logic [31:0] wstrb_mc_ch1;
    logic wlast_mc_ch0;
    logic wlast_mc_ch1;
    logic bready_mc_ch0;
    logic bready_mc_ch1;
    logic [AXI_IDW-1:0] bid_mc_ch0;
    logic [AXI_IDW-1:0] bid_mc_ch1;
    logic [1:0] bresp_mc_ch0;
    logic [1:0] bresp_mc_ch1;
    logic [AXI_USERW-1:0] buser_mc_ch0;
    logic [AXI_USERW-1:0] buser_mc_ch1;
    logic bvalid_mc_ch0;
    logic bvalid_mc_ch1;
    logic arready_mc_ch0;
    logic arready_mc_ch1;
    logic arvalid_mc_ch0;
    logic arvalid_mc_ch1;
    logic [AXI_IDW-1:0] arid_mc_ch0;
    logic [AXI_IDW-1:0] arid_mc_ch1;
    logic [AXI_ADDRW-1:0] araddr_mc_ch0;
    logic [AXI_ADDRW-1:0] araddr_mc_ch1;
    logic [7:0] arlen_mc_ch0;
    logic [7:0] arlen_mc_ch1;
    logic [2:0] arsize_mc_ch0;
    logic [2:0] arsize_mc_ch1;
    logic [1:0] arburst_mc_ch0;
    logic [1:0] arburst_mc_ch1;
    logic [AXI_USERW-1:0] aruser_mc_ch0;
    logic [AXI_USERW-1:0] aruser_mc_ch1;
    logic [3:0] arqos_mc_ch0;
    logic [3:0] arqos_mc_ch1;
    logic rready_mc_ch0;
    logic rready_mc_ch1;
    logic [AXI_IDW-1:0] rid_mc_ch0;
    logic [AXI_IDW-1:0] rid_mc_ch1;
    logic [AXI_USERW-1:0] ruser_mc_ch0;
    logic [AXI_USERW-1:0] ruser_mc_ch1;
    logic [255:0] rdata_mc_ch0;
    logic [255:0] rdata_mc_ch1;
    logic [1:0] rresp_mc_ch0;
    logic [1:0] rresp_mc_ch1;
    logic rlast_mc_ch0;
    logic rlast_mc_ch1;
    logic rvalid_mc_ch0;
    logic rvalid_mc_ch1;

    logic [AXI_ROUTE_FIFO_DEPTH-1:0] w_route_fifo;
    logic [AXI_ROUTE_FIFO_DEPTH-1:0] b_route_fifo;
    logic [AXI_ROUTE_FIFO_DEPTH-1:0] r_route_fifo;
    logic [AXI_ROUTE_FIFO_PTRW-1:0] w_route_rptr;
    logic [AXI_ROUTE_FIFO_PTRW-1:0] w_route_wptr;
    logic [AXI_ROUTE_FIFO_PTRW-1:0] b_route_rptr;
    logic [AXI_ROUTE_FIFO_PTRW-1:0] b_route_wptr;
    logic [AXI_ROUTE_FIFO_PTRW-1:0] r_route_rptr;
    logic [AXI_ROUTE_FIFO_PTRW-1:0] r_route_wptr;
    logic [AXI_ROUTE_FIFO_COUNTW-1:0] w_route_count;
    logic [AXI_ROUTE_FIFO_COUNTW-1:0] b_route_count;
    logic [AXI_ROUTE_FIFO_COUNTW-1:0] r_route_count;

    logic aw_hold_valid;
    logic aw_hold_channel_sel;
    logic [AXI_IDW-1:0] aw_hold_id;
    logic [AXI_ADDRW-1:0] aw_hold_addr;
    logic [7:0] aw_hold_len;
    logic [2:0] aw_hold_size;
    logic [1:0] aw_hold_burst;
    logic [3:0] aw_hold_cache;
    logic [2:0] aw_hold_prot;
    logic [AXI_USERW-1:0] aw_hold_user;
    logic [3:0] aw_hold_qos;

    logic ar_hold_valid;
    logic ar_hold_channel_sel;
    logic [AXI_IDW-1:0] ar_hold_id;
    logic [AXI_ADDRW-1:0] ar_hold_addr;
    logic [7:0] ar_hold_len;
    logic [2:0] ar_hold_size;
    logic [1:0] ar_hold_burst;
    logic [AXI_USERW-1:0] ar_hold_user;
    logic [3:0] ar_hold_qos;

    logic aw_accept_ext;
    logic ar_accept_ext;
    logic aw_hold_fire;
    logic ar_hold_fire;
    logic aw_route_full;
    logic w_route_full;
    logic b_route_full;
    logic r_route_full;
    logic w_route_empty;
    logic b_route_empty;
    logic r_route_empty;
    logic w_route_front;
    logic b_route_front;
    logic r_route_front;
    logic aw_route_push;
    logic ar_route_push;
    logic w_route_pop;
    logic b_route_pop;
    logic r_route_pop;
    logic b_route_sel;
    logic b_route_valid;
    logic b_route_hs;
    logic r_route_sel;
    logic r_route_valid;
    logic r_route_hs;

    typedef struct packed {
        logic        is_poll;
        logic        is_write;
        logic [11:0] addr;
        logic [31:0] data;
        logic [31:0] mask;
    } apb_init_cmd_t;

    typedef enum logic [1:0] {
        APB_INIT_IDLE,
        APB_INIT_SETUP,
        APB_INIT_ACCESS,
        APB_INIT_DONE
    } apb_init_state_t;

    localparam int APB_INIT_SEQ_LEN = 23;
    localparam int APB_INIT_INDEXW = $clog2(APB_INIT_SEQ_LEN);

    apb_init_state_t apb_init_state;
    logic [APB_INIT_INDEXW-1:0] apb_init_index;
    apb_init_cmd_t apb_init_current;
    logic apb_init_done;
    logic [11:0] apb_paddr;
    logic [31:0] apb_pwdata;
    logic apb_pwrite;
    logic apb_psel;
    logic apb_penable;
    logic apb_pready_ch0;
    logic apb_pready_ch1;
    logic [31:0] apb_prdata_ch0;
    logic [31:0] apb_prdata_ch1;
    logic apb_pslverr_ch0;
    logic apb_pslverr_ch1;
    logic apb_pready;
    logic [31:0] apb_prdata;
    logic apb_pslverr;

    function automatic logic [AXI_ADDRW-1:0] axi_channel_local_addr(
        input logic [AXI_ADDRW-1:0] addr
    );
        axi_channel_local_addr = addr;
        axi_channel_local_addr[AXI_ADDRW-1:AXI_CHANNEL_ADDR_BIT] =
            {1'b0, addr[AXI_ADDRW-1:AXI_CHANNEL_ADDR_BIT+1]};
    endfunction

    function automatic apb_init_cmd_t apb_init_write(
        input logic [11:0] addr,
        input logic [31:0] data
    );
        apb_init_write.is_poll = 1'b0;
        apb_init_write.is_write = 1'b1;
        apb_init_write.addr = addr;
        apb_init_write.data = data;
        apb_init_write.mask = 32'hffff_ffff;
    endfunction

    function automatic apb_init_cmd_t apb_init_poll(
        input logic [11:0] addr,
        input logic [31:0] data,
        input logic [31:0] mask
    );
        apb_init_poll.is_poll = 1'b1;
        apb_init_poll.is_write = 1'b0;
        apb_init_poll.addr = addr;
        apb_init_poll.data = data;
        apb_init_poll.mask = mask;
    endfunction

    function automatic apb_init_cmd_t apb_init_cmd(input logic [APB_INIT_INDEXW-1:0] idx);
        apb_init_cmd = apb_init_write(12'h000, 32'h0000_0000);
        unique case (idx)
            5'd0:  apb_init_cmd = apb_init_write(12'h000, 32'h1082_2516);
            5'd1:  apb_init_cmd = apb_init_write(12'h004, 32'h0408_040c);
            5'd2:  apb_init_cmd = apb_init_write(12'h008, 32'h0a18_0c34);
            5'd3:  apb_init_cmd = apb_init_write(12'h014, 32'h0400_0105);
            5'd4:  apb_init_cmd = apb_init_write(12'h028, 32'h0014_1008);
            5'd5:  apb_init_cmd = apb_init_write(12'h01c, 32'h0000_0001);
            5'd6:  apb_init_cmd = apb_init_write(12'h010, 32'h0001_5e80);
            5'd7:  apb_init_cmd = apb_init_write(12'h100, 32'h0000_0000);
            5'd8:  apb_init_cmd = apb_init_write(12'h060, 32'h0a0a_0c0a);
            5'd9:  apb_init_cmd = apb_init_write(12'h040, 32'h000d_0145);
            5'd10: apb_init_cmd = apb_init_write(12'h044, 32'h0000_5018);
            5'd11: apb_init_cmd = apb_init_write(12'h048, 32'h0000_8400);
            5'd12: apb_init_cmd = apb_init_write(12'h04c, 32'h0001_0044);
            5'd13: apb_init_cmd = apb_init_write(12'h050, 32'h0000_0028);
            5'd14: apb_init_cmd = apb_init_write(12'h054, 32'h0040_0000);
            5'd15: apb_init_cmd = apb_init_write(12'h058, 32'h0000_1000);
            5'd16: apb_init_cmd = apb_init_write(12'h300, 32'h0000_0003);
            5'd17: apb_init_cmd = apb_init_write(12'h02c, 32'h0000_1e78);
            5'd18: apb_init_cmd = apb_init_write(12'h064, 32'h0000_0001);
            5'd19: apb_init_cmd = apb_init_write(12'h020, 32'h0000_008c);
            5'd20: apb_init_cmd = apb_init_write(12'hff4, 32'h0000_0001);
            5'd21: apb_init_cmd = apb_init_poll(12'h03c, 32'h0000_0001, 32'h0000_0001);
            5'd22: apb_init_cmd = apb_init_write(12'h034, 32'h0000_0001);
            default: begin
            end
        endcase
    endfunction

    assign training_over = apb_init_done_aclk;
    assign awpoison_intr_0 = 1'b0;
    assign arpoison_intr_0 = 1'b0;

    assign w_route_full = (w_route_count == AXI_ROUTE_FIFO_DEPTH_COUNT);
    assign b_route_full = (b_route_count == AXI_ROUTE_FIFO_DEPTH_COUNT);
    assign r_route_full = (r_route_count == AXI_ROUTE_FIFO_DEPTH_COUNT);
    assign aw_route_full = w_route_full || b_route_full;
    assign w_route_empty = (w_route_count == '0);
    assign b_route_empty = (b_route_count == '0);
    assign r_route_empty = (r_route_count == '0);
    assign w_route_front = w_route_fifo[w_route_rptr];
    assign b_route_front = b_route_fifo[b_route_rptr];
    assign r_route_front = r_route_fifo[r_route_rptr];

    assign awready_0 = !aw_hold_valid && !aw_route_full;
    assign aw_accept_ext = awvalid_0 && awready_0;
    assign awvalid_ch0 = aw_hold_valid && !aw_hold_channel_sel;
    assign awvalid_ch1 = aw_hold_valid &&  aw_hold_channel_sel;
    assign aw_hold_fire = aw_hold_valid && (aw_hold_channel_sel ? awready_ch1 : awready_ch0);
    assign aw_route_push = aw_hold_fire;

    assign wvalid_ch0 = wvalid_0 && !w_route_empty && !w_route_front;
    assign wvalid_ch1 = wvalid_0 && !w_route_empty &&  w_route_front;
    assign wready_0 = !w_route_empty && (w_route_front ? wready_ch1 : wready_ch0);
    assign w_route_pop = wvalid_0 && wready_0 && wlast_0;

    assign b_route_sel = b_route_front;
    assign b_route_valid = !b_route_empty && (b_route_sel ? bvalid_ch1 : bvalid_ch0);
    assign bvalid_0 = b_route_valid;
    assign bid_0 = b_route_sel ? bid_ch1 : bid_ch0;
    assign bresp_0 = b_route_sel ? bresp_ch1 : bresp_ch0;
    assign buser_0 = b_route_sel ? buser_ch1 : buser_ch0;
    assign bready_ch0 = bready_0 && b_route_valid && !b_route_sel;
    assign bready_ch1 = bready_0 && b_route_valid &&  b_route_sel;
    assign b_route_hs = bvalid_0 && bready_0;
    assign b_route_pop = b_route_hs;

    assign arready_0 = !ar_hold_valid && !r_route_full;
    assign ar_accept_ext = arvalid_0 && arready_0;
    assign arvalid_ch0 = ar_hold_valid && !ar_hold_channel_sel;
    assign arvalid_ch1 = ar_hold_valid &&  ar_hold_channel_sel;
    assign ar_hold_fire = ar_hold_valid && (ar_hold_channel_sel ? arready_ch1 : arready_ch0);
    assign ar_route_push = ar_hold_fire;

    assign r_route_sel = r_route_front;
    assign r_route_valid = !r_route_empty && (r_route_sel ? rvalid_ch1 : rvalid_ch0);
    assign rvalid_0 = r_route_valid;
    assign rid_0 = r_route_sel ? rid_ch1 : rid_ch0;
    assign rdata_0 = r_route_sel ? rdata_ch1 : rdata_ch0;
    assign rresp_0 = r_route_sel ? rresp_ch1 : rresp_ch0;
    assign ruser_0 = r_route_sel ? ruser_ch1 : ruser_ch0;
    assign rlast_0 = r_route_sel ? rlast_ch1 : rlast_ch0;
    assign rready_ch0 = rready_0 && r_route_valid && !r_route_sel;
    assign rready_ch1 = rready_0 && r_route_valid &&  r_route_sel;
    assign r_route_hs = rvalid_0 && rready_0;
    assign r_route_pop = r_route_hs && rlast_0;

    assign apb_init_current = apb_init_cmd(apb_init_index);
    assign apb_pready = apb_pready_ch0 && apb_pready_ch1;
    assign apb_prdata = apb_init_current.is_poll ? (apb_prdata_ch0 & apb_prdata_ch1) :
                                                   apb_prdata_ch0;
    assign apb_pslverr = apb_pslverr_ch0 | apb_pslverr_ch1;

    always_ff @(posedge aclk_0 or negedge aresetn_0) begin
        if (!aresetn_0) begin
            core_clk_phase_accum <= '0;
            core_ddrc_core_clk <= 1'b0;
        end else begin
            // 3 GHz aclk_0 -> average 800 MHz core clock: 8 posedges per 30 cycles.
            if (core_clk_phase_accum >=
                (CORE_CLK_ACLK_WINDOW_CYCLES - CORE_CLK_POSEDGES_PER_WINDOW)) begin
                core_clk_phase_accum <=
                    core_clk_phase_accum + CORE_CLK_POSEDGES_PER_WINDOW -
                    CORE_CLK_ACLK_WINDOW_CYCLES;
                core_ddrc_core_clk <= 1'b1;
            end else begin
                core_clk_phase_accum <= core_clk_phase_accum + CORE_CLK_POSEDGES_PER_WINDOW;
                core_ddrc_core_clk <= 1'b0;
            end
        end
    end

    always_ff @(posedge core_ddrc_core_clk or negedge aresetn_0) begin
        if (!aresetn_0) begin
            core_ddrc_resetn_meta <= 1'b0;
            core_ddrc_resetn <= 1'b0;
        end else begin
            core_ddrc_resetn_meta <= 1'b1;
            core_ddrc_resetn <= core_ddrc_resetn_meta;
        end
    end

    always_ff @(posedge aclk_0 or negedge aresetn_0) begin
        if (!aresetn_0) begin
            apb_init_done_aclk_meta <= 1'b0;
            apb_init_done_aclk <= 1'b0;
        end else begin
            apb_init_done_aclk_meta <= apb_init_done;
            apb_init_done_aclk <= apb_init_done_aclk_meta;
        end
    end

    always_comb begin
        apb_paddr = apb_init_current.addr;
        apb_pwdata = apb_init_current.data;
        apb_pwrite = apb_init_current.is_write;
        apb_psel = 1'b0;
        apb_penable = 1'b0;

        unique case (apb_init_state)
            APB_INIT_SETUP: begin
                apb_psel = 1'b1;
            end
            APB_INIT_ACCESS: begin
                apb_psel = 1'b1;
                apb_penable = 1'b1;
            end
            default: begin
            end
        endcase
    end

    always_ff @(posedge core_ddrc_core_clk or negedge core_ddrc_resetn) begin
        if (!core_ddrc_resetn) begin
            apb_init_state <= APB_INIT_IDLE;
            apb_init_index <= '0;
            apb_init_done <= 1'b0;
        end else begin
            unique case (apb_init_state)
                APB_INIT_IDLE: begin
                    if (!apb_init_done) begin
                        apb_init_state <= APB_INIT_SETUP;
                    end
                end
                APB_INIT_SETUP: begin
                    apb_init_state <= APB_INIT_ACCESS;
                end
                APB_INIT_ACCESS: begin
                    if (apb_pready) begin
                        if (!apb_init_current.is_poll ||
                            ((apb_prdata & apb_init_current.mask) == apb_init_current.data)) begin
`ifdef VERILATOR
                            if ($test$plusargs("yuquan_apb_debug")) begin
                                $display("[YUQUAN_APB] op=%0d write=%0d poll=%0d addr=0x%03h wdata=0x%08h rdata=0x%08h pslverr=%0d",
                                         apb_init_index,
                                         apb_init_current.is_write,
                                         apb_init_current.is_poll,
                                         apb_init_current.addr,
                                         apb_init_current.data,
                                         apb_prdata,
                                         apb_pslverr);
                            end
`endif
                            if (apb_init_index == APB_INIT_SEQ_LEN - 1) begin
                                apb_init_done <= 1'b1;
                                apb_init_state <= APB_INIT_DONE;
`ifdef VERILATOR
                                if ($test$plusargs("yuquan_apb_debug")) begin
                                    $display("[YUQUAN_APB] init done");
                                end
`endif
                            end else begin
                                apb_init_index <= apb_init_index + 1'b1;
                                apb_init_state <= APB_INIT_SETUP;
                            end
                        end else begin
                            apb_init_state <= APB_INIT_SETUP;
                        end
                    end
                end
                APB_INIT_DONE: begin
                    apb_init_state <= APB_INIT_DONE;
                end
                default: begin
                    apb_init_state <= APB_INIT_IDLE;
                end
            endcase
        end
    end

    always_ff @(posedge aclk_0 or negedge aresetn_0) begin
        if (!aresetn_0) begin
            aw_hold_valid <= 1'b0;
            aw_hold_channel_sel <= 1'b0;
            aw_hold_id <= '0;
            aw_hold_addr <= '0;
            aw_hold_len <= '0;
            aw_hold_size <= '0;
            aw_hold_burst <= '0;
            aw_hold_cache <= '0;
            aw_hold_prot <= '0;
            aw_hold_user <= '0;
            aw_hold_qos <= '0;
            ar_hold_valid <= 1'b0;
            ar_hold_channel_sel <= 1'b0;
            ar_hold_id <= '0;
            ar_hold_addr <= '0;
            ar_hold_len <= '0;
            ar_hold_size <= '0;
            ar_hold_burst <= '0;
            ar_hold_user <= '0;
            ar_hold_qos <= '0;
            w_route_fifo <= '0;
            b_route_fifo <= '0;
            r_route_fifo <= '0;
            w_route_rptr <= '0;
            w_route_wptr <= '0;
            b_route_rptr <= '0;
            b_route_wptr <= '0;
            r_route_rptr <= '0;
            r_route_wptr <= '0;
            w_route_count <= '0;
            b_route_count <= '0;
            r_route_count <= '0;
        end else begin
            unique case ({aw_accept_ext, aw_hold_fire})
                2'b10: begin
                    aw_hold_valid <= 1'b1;
                    aw_hold_channel_sel <= awaddr_0[AXI_CHANNEL_ADDR_BIT];
                    aw_hold_id <= awid_0;
                    aw_hold_addr <= awaddr_0;
                    aw_hold_len <= awlen_0;
                    aw_hold_size <= awsize_0;
                    aw_hold_burst <= awburst_0;
                    aw_hold_cache <= awcache_0;
                    aw_hold_prot <= awprot_0;
                    aw_hold_user <= awuser_0;
                    aw_hold_qos <= awqos_0;
                end
                2'b01: begin
                    aw_hold_valid <= 1'b0;
                end
                default: begin
                end
            endcase

            unique case ({ar_accept_ext, ar_hold_fire})
                2'b10: begin
                    ar_hold_valid <= 1'b1;
                    ar_hold_channel_sel <= araddr_0[AXI_CHANNEL_ADDR_BIT];
                    ar_hold_id <= arid_0;
                    ar_hold_addr <= araddr_0;
                    ar_hold_len <= arlen_0;
                    ar_hold_size <= arsize_0;
                    ar_hold_burst <= arburst_0;
                    ar_hold_user <= aruser_0;
                    ar_hold_qos <= arqos_0;
                end
                2'b01: begin
                    ar_hold_valid <= 1'b0;
                end
                default: begin
                end
            endcase

            if (aw_route_push) begin
                w_route_fifo[w_route_wptr] <= aw_hold_channel_sel;
                b_route_fifo[b_route_wptr] <= aw_hold_channel_sel;
                w_route_wptr <= w_route_wptr + 1'b1;
                b_route_wptr <= b_route_wptr + 1'b1;
            end
            if (w_route_pop) begin
                w_route_rptr <= w_route_rptr + 1'b1;
            end
            if (b_route_pop) begin
                b_route_rptr <= b_route_rptr + 1'b1;
            end

            unique case ({aw_route_push, w_route_pop})
                2'b10: w_route_count <= w_route_count + 1'b1;
                2'b01: w_route_count <= w_route_count - 1'b1;
                default: begin
                end
            endcase
            unique case ({aw_route_push, b_route_pop})
                2'b10: b_route_count <= b_route_count + 1'b1;
                2'b01: b_route_count <= b_route_count - 1'b1;
                default: begin
                end
            endcase

            if (ar_route_push) begin
                r_route_fifo[r_route_wptr] <= ar_hold_channel_sel;
                r_route_wptr <= r_route_wptr + 1'b1;
            end
            if (r_route_pop) begin
                r_route_rptr <= r_route_rptr + 1'b1;
            end
            unique case ({ar_route_push, r_route_pop})
                2'b10: r_route_count <= r_route_count + 1'b1;
                2'b01: r_route_count <= r_route_count - 1'b1;
                default: begin
                end
            endcase
        end
    end

    yuquan_axi_async_bridge #(
        .AXI_ADDRW(AXI_ADDRW),
        .AXI_IDW(AXI_IDW),
        .AXI_USERW(AXI_USERW),
        .AXI_DATAW(256),
        .FIFO_DEPTH(AXI_ASYNC_FIFO_DEPTH)
    ) u_yuquan_axi_async_ch0 (
        .s_clk(aclk_0),
        .s_resetn(aresetn_0),
        .m_clk(core_ddrc_core_clk),
        .m_resetn(core_ddrc_resetn),
        .s_awid(aw_hold_id),
        .s_awaddr(axi_channel_local_addr(aw_hold_addr)),
        .s_awlen(aw_hold_len),
        .s_awsize(aw_hold_size),
        .s_awburst(aw_hold_burst),
        .s_awuser(aw_hold_user),
        .s_awqos(aw_hold_qos),
        .s_awvalid(awvalid_ch0),
        .s_awready(awready_ch0),
        .s_wuser(wuser_0),
        .s_wdata(wdata_0),
        .s_wstrb(wstrb_0),
        .s_wlast(wlast_0),
        .s_wvalid(wvalid_ch0),
        .s_wready(wready_ch0),
        .s_bid(bid_ch0),
        .s_bresp(bresp_ch0),
        .s_buser(buser_ch0),
        .s_bvalid(bvalid_ch0),
        .s_bready(bready_ch0),
        .s_arid(ar_hold_id),
        .s_araddr(axi_channel_local_addr(ar_hold_addr)),
        .s_arlen(ar_hold_len),
        .s_arsize(ar_hold_size),
        .s_arburst(ar_hold_burst),
        .s_aruser(ar_hold_user),
        .s_arqos(ar_hold_qos),
        .s_arvalid(arvalid_ch0),
        .s_arready(arready_ch0),
        .s_rid(rid_ch0),
        .s_ruser(ruser_ch0),
        .s_rdata(rdata_ch0),
        .s_rresp(rresp_ch0),
        .s_rlast(rlast_ch0),
        .s_rvalid(rvalid_ch0),
        .s_rready(rready_ch0),
        .m_awid(awid_mc_ch0),
        .m_awaddr(awaddr_mc_ch0),
        .m_awlen(awlen_mc_ch0),
        .m_awsize(awsize_mc_ch0),
        .m_awburst(awburst_mc_ch0),
        .m_awuser(awuser_mc_ch0),
        .m_awqos(awqos_mc_ch0),
        .m_awvalid(awvalid_mc_ch0),
        .m_awready(awready_mc_ch0),
        .m_wuser(wuser_mc_ch0),
        .m_wdata(wdata_mc_ch0),
        .m_wstrb(wstrb_mc_ch0),
        .m_wlast(wlast_mc_ch0),
        .m_wvalid(wvalid_mc_ch0),
        .m_wready(wready_mc_ch0),
        .m_bid(bid_mc_ch0),
        .m_bresp(bresp_mc_ch0),
        .m_buser(buser_mc_ch0),
        .m_bvalid(bvalid_mc_ch0),
        .m_bready(bready_mc_ch0),
        .m_arid(arid_mc_ch0),
        .m_araddr(araddr_mc_ch0),
        .m_arlen(arlen_mc_ch0),
        .m_arsize(arsize_mc_ch0),
        .m_arburst(arburst_mc_ch0),
        .m_aruser(aruser_mc_ch0),
        .m_arqos(arqos_mc_ch0),
        .m_arvalid(arvalid_mc_ch0),
        .m_arready(arready_mc_ch0),
        .m_rid(rid_mc_ch0),
        .m_ruser(ruser_mc_ch0),
        .m_rdata(rdata_mc_ch0),
        .m_rresp(rresp_mc_ch0),
        .m_rlast(rlast_mc_ch0),
        .m_rvalid(rvalid_mc_ch0),
        .m_rready(rready_mc_ch0)
    );

    yuquan_axi_async_bridge #(
        .AXI_ADDRW(AXI_ADDRW),
        .AXI_IDW(AXI_IDW),
        .AXI_USERW(AXI_USERW),
        .AXI_DATAW(256),
        .FIFO_DEPTH(AXI_ASYNC_FIFO_DEPTH)
    ) u_yuquan_axi_async_ch1 (
        .s_clk(aclk_0),
        .s_resetn(aresetn_0),
        .m_clk(core_ddrc_core_clk),
        .m_resetn(core_ddrc_resetn),
        .s_awid(aw_hold_id),
        .s_awaddr(axi_channel_local_addr(aw_hold_addr)),
        .s_awlen(aw_hold_len),
        .s_awsize(aw_hold_size),
        .s_awburst(aw_hold_burst),
        .s_awuser(aw_hold_user),
        .s_awqos(aw_hold_qos),
        .s_awvalid(awvalid_ch1),
        .s_awready(awready_ch1),
        .s_wuser(wuser_0),
        .s_wdata(wdata_0),
        .s_wstrb(wstrb_0),
        .s_wlast(wlast_0),
        .s_wvalid(wvalid_ch1),
        .s_wready(wready_ch1),
        .s_bid(bid_ch1),
        .s_bresp(bresp_ch1),
        .s_buser(buser_ch1),
        .s_bvalid(bvalid_ch1),
        .s_bready(bready_ch1),
        .s_arid(ar_hold_id),
        .s_araddr(axi_channel_local_addr(ar_hold_addr)),
        .s_arlen(ar_hold_len),
        .s_arsize(ar_hold_size),
        .s_arburst(ar_hold_burst),
        .s_aruser(ar_hold_user),
        .s_arqos(ar_hold_qos),
        .s_arvalid(arvalid_ch1),
        .s_arready(arready_ch1),
        .s_rid(rid_ch1),
        .s_ruser(ruser_ch1),
        .s_rdata(rdata_ch1),
        .s_rresp(rresp_ch1),
        .s_rlast(rlast_ch1),
        .s_rvalid(rvalid_ch1),
        .s_rready(rready_ch1),
        .m_awid(awid_mc_ch1),
        .m_awaddr(awaddr_mc_ch1),
        .m_awlen(awlen_mc_ch1),
        .m_awsize(awsize_mc_ch1),
        .m_awburst(awburst_mc_ch1),
        .m_awuser(awuser_mc_ch1),
        .m_awqos(awqos_mc_ch1),
        .m_awvalid(awvalid_mc_ch1),
        .m_awready(awready_mc_ch1),
        .m_wuser(wuser_mc_ch1),
        .m_wdata(wdata_mc_ch1),
        .m_wstrb(wstrb_mc_ch1),
        .m_wlast(wlast_mc_ch1),
        .m_wvalid(wvalid_mc_ch1),
        .m_wready(wready_mc_ch1),
        .m_bid(bid_mc_ch1),
        .m_bresp(bresp_mc_ch1),
        .m_buser(buser_mc_ch1),
        .m_bvalid(bvalid_mc_ch1),
        .m_bready(bready_mc_ch1),
        .m_arid(arid_mc_ch1),
        .m_araddr(araddr_mc_ch1),
        .m_arlen(arlen_mc_ch1),
        .m_arsize(arsize_mc_ch1),
        .m_arburst(arburst_mc_ch1),
        .m_aruser(aruser_mc_ch1),
        .m_arqos(arqos_mc_ch1),
        .m_arvalid(arvalid_mc_ch1),
        .m_arready(arready_mc_ch1),
        .m_rid(rid_mc_ch1),
        .m_ruser(ruser_mc_ch1),
        .m_rdata(rdata_mc_ch1),
        .m_rresp(rresp_mc_ch1),
        .m_rlast(rlast_mc_ch1),
        .m_rvalid(rvalid_mc_ch1),
        .m_rready(rready_mc_ch1)
    );

    yuquan_mc_channel #(
        .CHANNEL_ID(0)
    ) u_yuquan_ch0 (
        .clk(core_ddrc_core_clk),
        .resetn(core_ddrc_resetn),
        .awid(awid_mc_ch0),
        .awaddr(awaddr_mc_ch0),
        .awlen(awlen_mc_ch0),
        .awsize(awsize_mc_ch0),
        .awburst(awburst_mc_ch0),
        .awuser(awuser_mc_ch0),
        .awqos(awqos_mc_ch0),
        .awvalid(awvalid_mc_ch0),
        .awready(awready_mc_ch0),
        .wuser(wuser_mc_ch0),
        .wdata(wdata_mc_ch0),
        .wstrb(wstrb_mc_ch0),
        .wlast(wlast_mc_ch0),
        .wvalid(wvalid_mc_ch0),
        .wready(wready_mc_ch0),
        .bid(bid_mc_ch0),
        .bresp(bresp_mc_ch0),
        .buser(buser_mc_ch0),
        .bvalid(bvalid_mc_ch0),
        .bready(bready_mc_ch0),
        .arid(arid_mc_ch0),
        .araddr(araddr_mc_ch0),
        .arlen(arlen_mc_ch0),
        .arsize(arsize_mc_ch0),
        .arburst(arburst_mc_ch0),
        .aruser(aruser_mc_ch0),
        .arqos(arqos_mc_ch0),
        .arvalid(arvalid_mc_ch0),
        .arready(arready_mc_ch0),
        .rid(rid_mc_ch0),
        .ruser(ruser_mc_ch0),
        .rdata(rdata_mc_ch0),
        .rresp(rresp_mc_ch0),
        .rlast(rlast_mc_ch0),
        .rvalid(rvalid_mc_ch0),
        .rready(rready_mc_ch0),
        .paddr(apb_paddr),
        .pwdata(apb_pwdata),
        .pwrite(apb_pwrite),
        .psel(apb_psel),
        .penable(apb_penable),
        .pready(apb_pready_ch0),
        .prdata(apb_prdata_ch0),
        .pslverr(apb_pslverr_ch0)
    );

    yuquan_mc_channel #(
        .CHANNEL_ID(1)
    ) u_yuquan_ch1 (
        .clk(core_ddrc_core_clk),
        .resetn(core_ddrc_resetn),
        .awid(awid_mc_ch1),
        .awaddr(awaddr_mc_ch1),
        .awlen(awlen_mc_ch1),
        .awsize(awsize_mc_ch1),
        .awburst(awburst_mc_ch1),
        .awuser(awuser_mc_ch1),
        .awqos(awqos_mc_ch1),
        .awvalid(awvalid_mc_ch1),
        .awready(awready_mc_ch1),
        .wuser(wuser_mc_ch1),
        .wdata(wdata_mc_ch1),
        .wstrb(wstrb_mc_ch1),
        .wlast(wlast_mc_ch1),
        .wvalid(wvalid_mc_ch1),
        .wready(wready_mc_ch1),
        .bid(bid_mc_ch1),
        .bresp(bresp_mc_ch1),
        .buser(buser_mc_ch1),
        .bvalid(bvalid_mc_ch1),
        .bready(bready_mc_ch1),
        .arid(arid_mc_ch1),
        .araddr(araddr_mc_ch1),
        .arlen(arlen_mc_ch1),
        .arsize(arsize_mc_ch1),
        .arburst(arburst_mc_ch1),
        .aruser(aruser_mc_ch1),
        .arqos(arqos_mc_ch1),
        .arvalid(arvalid_mc_ch1),
        .arready(arready_mc_ch1),
        .rid(rid_mc_ch1),
        .ruser(ruser_mc_ch1),
        .rdata(rdata_mc_ch1),
        .rresp(rresp_mc_ch1),
        .rlast(rlast_mc_ch1),
        .rvalid(rvalid_mc_ch1),
        .rready(rready_mc_ch1),
        .paddr(apb_paddr),
        .pwdata(apb_pwdata),
        .pwrite(apb_pwrite),
        .psel(apb_psel),
        .penable(apb_penable),
        .pready(apb_pready_ch1),
        .prdata(apb_prdata_ch1),
        .pslverr(apb_pslverr_ch1)
    );
endmodule

module yuquan_mc_channel #(
    parameter int unsigned CHANNEL_ID = 0
) (
    input  logic         clk,
    input  logic         resetn,

    input  logic [17:0]  awid,
    input  logic [35:0]  awaddr,
    input  logic [7:0]   awlen,
    input  logic [2:0]   awsize,
    input  logic [1:0]   awburst,
    input  logic [7:0]   awuser,
    input  logic [3:0]   awqos,
    input  logic         awvalid,
    output logic         awready,

    input  logic [7:0]   wuser,
    input  logic [255:0] wdata,
    input  logic [31:0]  wstrb,
    input  logic         wlast,
    input  logic         wvalid,
    output logic         wready,

    output logic [17:0]  bid,
    output logic [1:0]   bresp,
    output logic [7:0]   buser,
    output logic         bvalid,
    input  logic         bready,

    input  logic [17:0]  arid,
    input  logic [35:0]  araddr,
    input  logic [7:0]   arlen,
    input  logic [2:0]   arsize,
    input  logic [1:0]   arburst,
    input  logic [7:0]   aruser,
    input  logic [3:0]   arqos,
    input  logic         arvalid,
    output logic         arready,

    output logic [17:0]  rid,
    output logic [7:0]   ruser,
    output logic [255:0] rdata,
    output logic [1:0]   rresp,
    output logic         rlast,
    output logic         rvalid,
    input  logic         rready,

    input  logic [11:0]  paddr,
    input  logic [31:0]  pwdata,
    input  logic         pwrite,
    input  logic         psel,
    input  logic         penable,
    output logic         pready,
    output logic [31:0]  prdata,
    output logic         pslverr
);
    localparam int DFI_FREQ_RATIO = 2;
    localparam int DFI_RANKS = 2;
    localparam int DFI_BANK_BITS = 2;
    localparam int DFI_BG_BITS = 2;
    localparam int DFI_ADDR_BITS = 18;
    localparam int DFI_DATA_BITS = 256;
    localparam int unsigned DFI_DDR4_FREQUENCY_POINT = 5;
    localparam int DFI_RANK_INVALID = 32'hffff_ffff;

`ifdef VERILATOR
    import "DPI-C" function void dfi_dpi_reset(input int unsigned channel);
    import "DPI-C" function void dfi_dpi_cycle(
        input int unsigned channel,
        input int unsigned frequency,
        input int unsigned ctrlupd_req,
        input int unsigned lp_req,
        input int unsigned cmd_valid,
        input int unsigned cmd_kind,
        input int unsigned cmd_rank,
        input int unsigned cmd_bank,
        input longint unsigned cmd_arg0,
        input longint unsigned cmd_arg1,
        input int unsigned rddata_en,
        input int unsigned wrdata_en,
        input int unsigned wrdata_mask,
        input bit [DFI_DATA_BITS-1:0] wrdata,
        output bit [DFI_DATA_BITS-1:0] rddata,
        output int unsigned rddata_valid,
        output int unsigned rddata_dbi,
        output int unsigned ctrlupd_ack,
        output int unsigned ctrlupd_ack2,
        output int unsigned lp_ack);
`endif

    typedef enum logic [3:0] {
        DFI_CMD_NOP,
        DFI_CMD_ACTIVE,
        DFI_CMD_READ,
        DFI_CMD_WRITE,
        DFI_CMD_PRECHARGE,
        DFI_CMD_REFRESH,
        DFI_CMD_MRS,
        DFI_CMD_ZQ,
        DFI_CMD_UNKNOWN
    } dfi_cmd_kind_t;

    logic [17:0] by_bid;
    logic by_buser;
    logic [17:0] by_rid;
    logic by_ruser;

    logic [DFI_ADDR_BITS-1:0] dfi_address_0;
    logic [DFI_ADDR_BITS-1:0] dfi_address_1;
    logic [DFI_BANK_BITS-1:0] dfi_bank_0;
    logic [DFI_BANK_BITS-1:0] dfi_bank_1;
    logic dfi_ras_n_0;
    logic dfi_ras_n_1;
    logic dfi_cas_n_0;
    logic dfi_cas_n_1;
    logic dfi_we_n_0;
    logic dfi_we_n_1;
    logic dfi_cs_n_0_0;
    logic dfi_cs_n_0_1;
    logic dfi_cs_n_1_0;
    logic dfi_cs_n_1_1;
    logic dfi_act_n_0;
    logic dfi_act_n_1;
    logic [DFI_BG_BITS-1:0] dfi_bg_0;
    logic [DFI_BG_BITS-1:0] dfi_bg_1;
    logic [15:0] dfi_wrdata_en;
    logic [DFI_DATA_BITS-1:0] dfi_wdata;
    logic [31:0] dfi_wdata_cs_n;
    logic [31:0] dfi_wdata_mask;
    logic [15:0] dfi_rddata_en;
    logic [31:0] dfi_rddata_cs_n;
    logic dfi_ctrlupd_req;
    logic dfi_phyupd_ack;
    logic dfi_init_start;
    logic dfi_lp_ctrl_req;
    logic dfi_lp_data_req;

    logic [DFI_DATA_BITS-1:0] dfi_rddata;
    logic [15:0] dfi_rddata_valid;
    logic [31:0] dfi_rddata_dbi_n;
    logic dfi_ctrlupd_ack;
    logic dfi_lp_ack;

    bit [DFI_DATA_BITS-1:0] dfi_dpi_rddata;
    bit [DFI_DATA_BITS-1:0] dfi_dpi_wrdata;
    int unsigned dfi_dpi_rddata_valid;
    int unsigned dfi_dpi_rddata_dbi;
    int unsigned dfi_dpi_ctrlupd_ack;
    int unsigned dfi_dpi_ctrlupd_ack2;
    int unsigned dfi_dpi_lp_ack;

    logic [DFI_FREQ_RATIO-1:0] dfi_act_n_bus;
    logic [DFI_FREQ_RATIO-1:0] dfi_ras_n_bus;
    logic [DFI_FREQ_RATIO-1:0] dfi_cas_n_bus;
    logic [DFI_FREQ_RATIO-1:0] dfi_we_n_bus;
    logic [(DFI_FREQ_RATIO*DFI_RANKS)-1:0] dfi_cs_bus;
    logic [(DFI_FREQ_RATIO*DFI_BANK_BITS)-1:0] dfi_bank_bus;
    logic [(DFI_FREQ_RATIO*DFI_BG_BITS)-1:0] dfi_bg_bus;
    logic [(DFI_FREQ_RATIO*DFI_ADDR_BITS)-1:0] dfi_address_bus;

    assign bid = by_bid;
    assign buser = {7'b0, by_buser};
    assign rid = by_rid;
    assign ruser = {7'b0, by_ruser};

    assign dfi_dpi_wrdata = dfi_wdata;
    assign dfi_act_n_bus = {dfi_act_n_1, dfi_act_n_0};
    assign dfi_ras_n_bus = {dfi_ras_n_1, dfi_ras_n_0};
    assign dfi_cas_n_bus = {dfi_cas_n_1, dfi_cas_n_0};
    assign dfi_we_n_bus = {dfi_we_n_1, dfi_we_n_0};
    assign dfi_cs_bus = {dfi_cs_n_1_1, dfi_cs_n_1_0, dfi_cs_n_0_1, dfi_cs_n_0_0};
    assign dfi_bank_bus = {dfi_bank_1, dfi_bank_0};
    assign dfi_bg_bus = {dfi_bg_1, dfi_bg_0};
    assign dfi_address_bus = {dfi_address_1, dfi_address_0};

    function automatic dfi_cmd_kind_t dfi_decode_cmd(
        input logic act_n,
        input logic ras_n,
        input logic cas_n,
        input logic we_n
    );
        if (!act_n) begin
            dfi_decode_cmd = DFI_CMD_ACTIVE;
        end else begin
            unique case ({act_n, ras_n, cas_n, we_n})
                4'b1111: dfi_decode_cmd = DFI_CMD_NOP;
                4'b1101: dfi_decode_cmd = DFI_CMD_READ;
                4'b1100: dfi_decode_cmd = DFI_CMD_WRITE;
                4'b1010: dfi_decode_cmd = DFI_CMD_PRECHARGE;
                4'b1001: dfi_decode_cmd = DFI_CMD_REFRESH;
                4'b1000: dfi_decode_cmd = DFI_CMD_MRS;
                4'b1110: dfi_decode_cmd = DFI_CMD_ZQ;
                default: dfi_decode_cmd = DFI_CMD_UNKNOWN;
            endcase
        end
    endfunction

    function automatic int unsigned dfi_decode_rank(input logic [DFI_RANKS-1:0] cs_bits);
        if (cs_bits[0] == 1'b0) begin
            dfi_decode_rank = 0;
        end else if (cs_bits[1] == 1'b0) begin
            dfi_decode_rank = 1;
        end else begin
            dfi_decode_rank = DFI_RANK_INVALID;
        end
    endfunction

    function automatic int unsigned dfi_decode_bank(
        input logic [DFI_BG_BITS-1:0] bg_bits,
        input logic [DFI_BANK_BITS-1:0] bank_bits
    );
        dfi_decode_bank = ({30'b0, bg_bits[1:0]} << 2) | {30'b0, bank_bits[1:0]};
    endfunction

    function automatic int unsigned dfi_decode_yuquan_col(input logic [DFI_ADDR_BITS-1:0] addr);
        logic [11:0] col;
        begin
            col = 12'h000;
            col[0] = addr[2];
            col[9:3] = addr[9:3];
            dfi_decode_yuquan_col = {20'b0, col};
        end
    endfunction

    function automatic int unsigned dfi_decode_mr_index(
        input logic [DFI_BG_BITS-1:0] bg_bits,
        input logic [DFI_BANK_BITS-1:0] bank_bits
    );
        dfi_decode_mr_index = {29'b0, bg_bits[0], bank_bits[1:0]};
    endfunction

    function automatic int unsigned dfi_extend_addr(input logic [DFI_ADDR_BITS-1:0] addr);
        dfi_extend_addr = {{(32-DFI_ADDR_BITS){1'b0}}, addr};
    endfunction

`ifdef VERILATOR
    task automatic dfi_dpi_cycle_one(
        input int unsigned channel,
        input logic [DFI_FREQ_RATIO-1:0] i_dfi_act_n,
        input logic [DFI_FREQ_RATIO-1:0] i_dfi_ras_n,
        input logic [DFI_FREQ_RATIO-1:0] i_dfi_cas_n,
        input logic [DFI_FREQ_RATIO-1:0] i_dfi_we_n,
        input logic [(DFI_FREQ_RATIO*DFI_RANKS)-1:0] i_dfi_cs,
        input logic [(DFI_FREQ_RATIO*DFI_BANK_BITS)-1:0] i_dfi_bank,
        input logic [(DFI_FREQ_RATIO*DFI_BG_BITS)-1:0] i_dfi_bg,
        input logic [(DFI_FREQ_RATIO*DFI_ADDR_BITS)-1:0] i_dfi_address,
        input int unsigned i_dfi_rddata_en,
        input int unsigned i_dfi_wrdata_en,
        input int unsigned i_dfi_wrdata_mask,
        input bit [DFI_DATA_BITS-1:0] i_dfi_wrdata,
        output bit [DFI_DATA_BITS-1:0] o_dfi_rddata,
        output int unsigned o_dfi_rddata_valid,
        output int unsigned o_dfi_rddata_dbi,
        output int unsigned o_dfi_ctrlupd_ack,
        output int unsigned o_dfi_ctrlupd_ack2,
        output int unsigned o_dfi_lp_ack
    );
        int unsigned dfi_cycle_cmd_valid;
        int unsigned dfi_cycle_cmd_kind;
        int unsigned dfi_cycle_cmd_rank;
        int unsigned dfi_cycle_cmd_bank;
        longint unsigned dfi_cycle_cmd_arg0;
        longint unsigned dfi_cycle_cmd_arg1;

        dfi_cycle_cmd_valid = '0;
        dfi_cycle_cmd_kind = '0;
        dfi_cycle_cmd_rank = '0;
        dfi_cycle_cmd_bank = '0;
        dfi_cycle_cmd_arg0 = '0;
        dfi_cycle_cmd_arg1 = '0;

        for (int unsigned dfi_phase = 0; dfi_phase < DFI_FREQ_RATIO; dfi_phase++) begin
            dfi_cmd_kind_t dfi_cmd_kind;
            logic [DFI_RANKS-1:0] dfi_phase_cs;
            logic [DFI_BANK_BITS-1:0] dfi_phase_bank;
            logic [DFI_BG_BITS-1:0] dfi_phase_bg;
            logic [DFI_ADDR_BITS-1:0] dfi_phase_addr;
            int unsigned dfi_phase_rank;
            int unsigned dfi_phase_bank_id;
            int unsigned dfi_phase_arg0;
            int unsigned dfi_phase_arg1;

            dfi_cmd_kind = dfi_decode_cmd(
                i_dfi_act_n[dfi_phase],
                i_dfi_ras_n[dfi_phase],
                i_dfi_cas_n[dfi_phase],
                i_dfi_we_n[dfi_phase]
            );
            dfi_phase_cs = i_dfi_cs[(dfi_phase * DFI_RANKS) +: DFI_RANKS];
            dfi_phase_rank = dfi_decode_rank(dfi_phase_cs);

            if (dfi_phase_rank != DFI_RANK_INVALID) begin
                dfi_phase_bank = i_dfi_bank[(dfi_phase * DFI_BANK_BITS) +: DFI_BANK_BITS];
                dfi_phase_bg = i_dfi_bg[(dfi_phase * DFI_BG_BITS) +: DFI_BG_BITS];
                dfi_phase_addr = i_dfi_address[(dfi_phase * DFI_ADDR_BITS) +: DFI_ADDR_BITS];
                dfi_phase_bank_id = dfi_decode_bank(dfi_phase_bg, dfi_phase_bank);
                dfi_phase_arg0 = '0;
                dfi_phase_arg1 = '0;

                unique case (dfi_cmd_kind)
                    DFI_CMD_ACTIVE: begin
                        dfi_phase_arg0 = dfi_extend_addr(dfi_phase_addr);
                        dfi_cycle_cmd_valid[dfi_phase] = 1'b1;
                    end
                    DFI_CMD_READ,
                    DFI_CMD_WRITE: begin
                        dfi_phase_arg0 = dfi_decode_yuquan_col(dfi_phase_addr);
                        dfi_cycle_cmd_valid[dfi_phase] = 1'b1;
                    end
                    DFI_CMD_PRECHARGE: begin
                        dfi_phase_arg0 = {31'b0, dfi_phase_addr[10]};
                        dfi_cycle_cmd_valid[dfi_phase] = 1'b1;
                    end
                    DFI_CMD_REFRESH,
                    DFI_CMD_ZQ: begin
                        dfi_cycle_cmd_valid[dfi_phase] = 1'b1;
                    end
                    DFI_CMD_MRS: begin
                        dfi_phase_arg0 = dfi_decode_mr_index(dfi_phase_bg, dfi_phase_bank);
                        dfi_phase_arg1 = {18'b0, dfi_phase_addr[13:0]};
                        dfi_cycle_cmd_valid[dfi_phase] = 1'b1;
                    end
                    default: begin
                    end
                endcase

                if (dfi_cycle_cmd_valid[dfi_phase]) begin
                    dfi_cycle_cmd_kind[(dfi_phase * 4) +: 4] = dfi_cmd_kind;
                    dfi_cycle_cmd_rank[(dfi_phase * 2) +: 2] = dfi_phase_rank[1:0];
                    dfi_cycle_cmd_bank[(dfi_phase * 4) +: 4] = dfi_phase_bank_id[3:0];
                    dfi_cycle_cmd_arg0[(dfi_phase * 32) +: 32] = dfi_phase_arg0;
                    dfi_cycle_cmd_arg1[(dfi_phase * 32) +: 32] = dfi_phase_arg1;
                end
            end
        end

        dfi_dpi_cycle(
            channel,
            DFI_DDR4_FREQUENCY_POINT,
            {31'b0, dfi_ctrlupd_req},
            {30'b0, dfi_lp_data_req, dfi_lp_ctrl_req},
            dfi_cycle_cmd_valid,
            dfi_cycle_cmd_kind,
            dfi_cycle_cmd_rank,
            dfi_cycle_cmd_bank,
            dfi_cycle_cmd_arg0,
            dfi_cycle_cmd_arg1,
            i_dfi_rddata_en,
            i_dfi_wrdata_en,
            i_dfi_wrdata_mask,
            i_dfi_wrdata,
            o_dfi_rddata,
            o_dfi_rddata_valid,
            o_dfi_rddata_dbi,
            o_dfi_ctrlupd_ack,
            o_dfi_ctrlupd_ack2,
            o_dfi_lp_ack
        );
    endtask
`endif

    always_ff @(posedge clk or negedge resetn) begin
        if (!resetn) begin
            dfi_rddata <= '0;
            dfi_rddata_valid <= '0;
            dfi_rddata_dbi_n <= '0;
            dfi_ctrlupd_ack <= 1'b0;
            dfi_lp_ack <= 1'b0;
`ifdef VERILATOR
            dfi_dpi_rddata <= '0;
            dfi_dpi_rddata_valid <= 0;
            dfi_dpi_rddata_dbi <= 0;
            dfi_dpi_ctrlupd_ack <= 0;
            dfi_dpi_ctrlupd_ack2 <= 0;
            dfi_dpi_lp_ack <= 0;
            dfi_dpi_reset(CHANNEL_ID);
`endif
        end else begin
`ifdef VERILATOR
            dfi_dpi_cycle_one(
                CHANNEL_ID,
                dfi_act_n_bus,
                dfi_ras_n_bus,
                dfi_cas_n_bus,
                dfi_we_n_bus,
                dfi_cs_bus,
                dfi_bank_bus,
                dfi_bg_bus,
                dfi_address_bus,
                {16'b0, dfi_rddata_en},
                {16'b0, dfi_wrdata_en},
                dfi_wdata_mask,
                dfi_dpi_wrdata,
                dfi_dpi_rddata,
                dfi_dpi_rddata_valid,
                dfi_dpi_rddata_dbi,
                dfi_dpi_ctrlupd_ack,
                dfi_dpi_ctrlupd_ack2,
                dfi_dpi_lp_ack
            );
            dfi_rddata <= dfi_dpi_rddata;
            dfi_rddata_valid <= dfi_dpi_rddata_valid[15:0];
            dfi_rddata_dbi_n <= dfi_dpi_rddata_dbi[31:0];
            dfi_ctrlupd_ack <= dfi_dpi_ctrlupd_ack[0];
            dfi_lp_ack <= dfi_dpi_lp_ack[0];
`else
            dfi_rddata <= '0;
            dfi_rddata_valid <= '0;
            dfi_rddata_dbi_n <= '0;
            dfi_ctrlupd_ack <= 1'b0;
            dfi_lp_ack <= 1'b0;
`endif
        end
    end

    BY_mc_top u_mc_top (
        .io_clk(clk),
        .io_rst(!resetn),
        .io_awio_awid(awid),
        .io_awio_awaddr(awaddr),
        .io_awio_awlen(awlen),
        .io_awio_awsize(awsize),
        .io_awio_awburst(awburst),
        .io_awio_awuser(awuser[0]),
        .io_awio_awqos(awqos),
        .io_awio_awvalid(awvalid),
        .io_awio_awready(awready),
        .io_wio_wuser(wuser[0]),
        .io_wio_wdata(wdata),
        .io_wio_wstrb(wstrb),
        .io_wio_wlast(wlast),
        .io_wio_wvalid(wvalid),
        .io_wio_wready(wready),
        .io_bio_bid(by_bid),
        .io_bio_bresp(bresp),
        .io_bio_buser(by_buser),
        .io_bio_bvalid(bvalid),
        .io_bio_bready(bready),
        .io_ario_arid(arid),
        .io_ario_araddr(araddr),
        .io_ario_arlen(arlen),
        .io_ario_arsize(arsize),
        .io_ario_arburst(arburst),
        .io_ario_aruser(aruser[0]),
        .io_ario_arqos(arqos),
        .io_ario_arvalid(arvalid),
        .io_ario_arready(arready),
        .io_rio_rid(by_rid),
        .io_rio_ruser(by_ruser),
        .io_rio_rdata(rdata),
        .io_rio_rresp(rresp),
        .io_rio_rlast(rlast),
        .io_rio_rvalid(rvalid),
        .io_rio_rready(rready),
        .io_dfi_dfictrl_dfi_address_0(dfi_address_0),
        .io_dfi_dfictrl_dfi_address_1(dfi_address_1),
        .io_dfi_dfictrl_dfi_bank_0(dfi_bank_0),
        .io_dfi_dfictrl_dfi_bank_1(dfi_bank_1),
        .io_dfi_dfictrl_dfi_ras_n_0(dfi_ras_n_0),
        .io_dfi_dfictrl_dfi_ras_n_1(dfi_ras_n_1),
        .io_dfi_dfictrl_dfi_cas_n_0(dfi_cas_n_0),
        .io_dfi_dfictrl_dfi_cas_n_1(dfi_cas_n_1),
        .io_dfi_dfictrl_dfi_we_n_0(dfi_we_n_0),
        .io_dfi_dfictrl_dfi_we_n_1(dfi_we_n_1),
        .io_dfi_dfictrl_dfi_cs_n_0_0(dfi_cs_n_0_0),
        .io_dfi_dfictrl_dfi_cs_n_0_1(dfi_cs_n_0_1),
        .io_dfi_dfictrl_dfi_cs_n_1_0(dfi_cs_n_1_0),
        .io_dfi_dfictrl_dfi_cs_n_1_1(dfi_cs_n_1_1),
        .io_dfi_dfictrl_dfi_act_n_0(dfi_act_n_0),
        .io_dfi_dfictrl_dfi_act_n_1(dfi_act_n_1),
        .io_dfi_dfictrl_dfi_bg_0(dfi_bg_0),
        .io_dfi_dfictrl_dfi_bg_1(dfi_bg_1),
        .io_dfi_dfictrl_dfi_cid(),
        .io_dfi_dfictrl_dfi_cke_0_0(),
        .io_dfi_dfictrl_dfi_cke_0_1(),
        .io_dfi_dfictrl_dfi_cke_1_0(),
        .io_dfi_dfictrl_dfi_cke_1_1(),
        .io_dfi_dfictrl_dfi_odt_0_0(),
        .io_dfi_dfictrl_dfi_odt_0_1(),
        .io_dfi_dfictrl_dfi_odt_1_0(),
        .io_dfi_dfictrl_dfi_odt_1_1(),
        .io_dfi_dfictrl_dfi_reset_n_0_0(),
        .io_dfi_dfictrl_dfi_reset_n_0_1(),
        .io_dfi_dfictrl_dfi_reset_n_1_0(),
        .io_dfi_dfictrl_dfi_reset_n_1_1(),
        .io_dfi_dfiwrdata_dfi_wrdata_en(dfi_wrdata_en),
        .io_dfi_dfiwrdata_dfi_wdata(dfi_wdata),
        .io_dfi_dfiwrdata_dfi_wdata_cs_n(dfi_wdata_cs_n),
        .io_dfi_dfiwrdata_dfi_wdata_mask(dfi_wdata_mask),
        .io_dfi_dfirddata_dfi_rddata_en(dfi_rddata_en),
        .io_dfi_dfirddata_dfi_rddata(dfi_rddata),
        .io_dfi_dfirddata_dfi_rddata_cs_n(dfi_rddata_cs_n),
        .io_dfi_dfirddata_dfi_rddata_valid(dfi_rddata_valid),
        .io_dfi_dfirddata_dfi_rddata_dbi_n(dfi_rddata_dbi_n),
        .io_dfi_dfiupdate_dfi_ctrlupd_req(dfi_ctrlupd_req),
        .io_dfi_dfiupdate_dfi_ctrlupd_ack(dfi_ctrlupd_ack),
        .io_dfi_dfiupdate_dfi_phyupd_req(1'b0),
        .io_dfi_dfiupdate_dfi_phyupd_type(2'b00),
        .io_dfi_dfiupdate_dfi_phyupd_ack(dfi_phyupd_ack),
        .io_dfi_dfistatus_dfi_data_byte_disable(),
        .io_dfi_dfistatus_dfi_dram_clk_disable(),
        .io_dfi_dfistatus_dfi_freq_ratio(),
        .io_dfi_dfistatus_dfi_init_start(dfi_init_start),
        .io_dfi_dfistatus_dfi_init_complete(1'b1),
        .io_dfi_dfistatus_dfi_parity_in(),
        .io_dfi_dfistatus_dfi_alert_n(2'b11),
        .io_dfi_dfitraining_dfi_rdlvl_req(1'b0),
        .io_dfi_dfitraining_dfi_phy_rdlvl_cs_n(2'b11),
        .io_dfi_dfitraining_dfi_rdlvl_en(),
        .io_dfi_dfitraining_dfi_rdlvl_resp(4'b0000),
        .io_dfi_dfitraining_dfi_rdlvl_gate_req(1'b0),
        .io_dfi_dfitraining_dfi_phy_rdlvl_gate_cs_n(2'b11),
        .io_dfi_dfitraining_dfi_rdlvl_gate_en(),
        .io_dfi_dfitraining_dfi_wrlvl_req(4'b0000),
        .io_dfi_dfitraining_dfi_phy_wrlvl_cs_n(2'b11),
        .io_dfi_dfitraining_dfi_wrlvl_en(),
        .io_dfi_dfitraining_dfi_wrlvl_strobe(),
        .io_dfi_dfitraining_dfi_wrlvl_resp(4'b0000),
        .io_dfi_dfitraining_dfi_lvl_pattern(),
        .io_dfi_dfitraining_dfi_lvl_periodic(),
        .io_dfi_dfitraining_dfi_phylvl_req_cs_n(1'b1),
        .io_dfi_dfitraining_dfi_phylvl_ack_cs_n(),
        .io_dfi_dfilp_dfi_lp_ctrl_req(dfi_lp_ctrl_req),
        .io_dfi_dfilp_dfi_lp_data_req(dfi_lp_data_req),
        .io_dfi_dfilp_dfi_lp_wakeup(),
        .io_dfi_dfilp_dfi_lp_ack(dfi_lp_ack),
        .io_apb_pclk(clk),
        .io_apb_presetn(resetn),
        .io_apb_paddr(paddr),
        .io_apb_pwdata(pwdata),
        .io_apb_pwrite(pwrite),
        .io_apb_psel(psel),
        .io_apb_penable(penable),
        .io_apb_pready(pready),
        .io_apb_prdata(prdata),
        .io_apb_pslverr(pslverr),
        .io_mig_phy_done(1'b1)
    );
endmodule
