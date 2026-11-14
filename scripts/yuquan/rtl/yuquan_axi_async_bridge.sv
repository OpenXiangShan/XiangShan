module yuquan_axi_async_bridge #(
    parameter int unsigned AXI_ADDRW = 36,
    parameter int unsigned AXI_IDW = 16,
    parameter int unsigned AXI_USERW = 8,
    parameter int unsigned AXI_DATAW = 256,
    parameter int unsigned AXI_STRBW = AXI_DATAW / 8,
    parameter int unsigned FIFO_DEPTH = 32
) (
    input  logic                  s_clk,
    input  logic                  s_resetn,
    input  logic                  m_clk,
    input  logic                  m_resetn,

    input  logic [AXI_IDW-1:0]    s_awid,
    input  logic [AXI_ADDRW-1:0]  s_awaddr,
    input  logic [7:0]            s_awlen,
    input  logic [2:0]            s_awsize,
    input  logic [1:0]            s_awburst,
    input  logic [AXI_USERW-1:0]  s_awuser,
    input  logic [3:0]            s_awqos,
    input  logic                  s_awvalid,
    output logic                  s_awready,

    input  logic [AXI_USERW-1:0]  s_wuser,
    input  logic [AXI_DATAW-1:0]  s_wdata,
    input  logic [AXI_STRBW-1:0]  s_wstrb,
    input  logic                  s_wlast,
    input  logic                  s_wvalid,
    output logic                  s_wready,

    output logic [AXI_IDW-1:0]    s_bid,
    output logic [1:0]            s_bresp,
    output logic [AXI_USERW-1:0]  s_buser,
    output logic                  s_bvalid,
    input  logic                  s_bready,

    input  logic [AXI_IDW-1:0]    s_arid,
    input  logic [AXI_ADDRW-1:0]  s_araddr,
    input  logic [7:0]            s_arlen,
    input  logic [2:0]            s_arsize,
    input  logic [1:0]            s_arburst,
    input  logic [AXI_USERW-1:0]  s_aruser,
    input  logic [3:0]            s_arqos,
    input  logic                  s_arvalid,
    output logic                  s_arready,

    output logic [AXI_IDW-1:0]    s_rid,
    output logic [AXI_USERW-1:0]  s_ruser,
    output logic [AXI_DATAW-1:0]  s_rdata,
    output logic [1:0]            s_rresp,
    output logic                  s_rlast,
    output logic                  s_rvalid,
    input  logic                  s_rready,

    output logic [AXI_IDW-1:0]    m_awid,
    output logic [AXI_ADDRW-1:0]  m_awaddr,
    output logic [7:0]            m_awlen,
    output logic [2:0]            m_awsize,
    output logic [1:0]            m_awburst,
    output logic [AXI_USERW-1:0]  m_awuser,
    output logic [3:0]            m_awqos,
    output logic                  m_awvalid,
    input  logic                  m_awready,

    output logic [AXI_USERW-1:0]  m_wuser,
    output logic [AXI_DATAW-1:0]  m_wdata,
    output logic [AXI_STRBW-1:0]  m_wstrb,
    output logic                  m_wlast,
    output logic                  m_wvalid,
    input  logic                  m_wready,

    input  logic [AXI_IDW-1:0]    m_bid,
    input  logic [1:0]            m_bresp,
    input  logic [AXI_USERW-1:0]  m_buser,
    input  logic                  m_bvalid,
    output logic                  m_bready,

    output logic [AXI_IDW-1:0]    m_arid,
    output logic [AXI_ADDRW-1:0]  m_araddr,
    output logic [7:0]            m_arlen,
    output logic [2:0]            m_arsize,
    output logic [1:0]            m_arburst,
    output logic [AXI_USERW-1:0]  m_aruser,
    output logic [3:0]            m_arqos,
    output logic                  m_arvalid,
    input  logic                  m_arready,

    input  logic [AXI_IDW-1:0]    m_rid,
    input  logic [AXI_USERW-1:0]  m_ruser,
    input  logic [AXI_DATAW-1:0]  m_rdata,
    input  logic [1:0]            m_rresp,
    input  logic                  m_rlast,
    input  logic                  m_rvalid,
    output logic                  m_rready
);
    localparam int unsigned AW_PAYLOAD_W =
        AXI_IDW + AXI_ADDRW + 8 + 3 + 2 + AXI_USERW + 4;
    localparam int unsigned W_PAYLOAD_W =
        AXI_USERW + AXI_DATAW + AXI_STRBW + 1;
    localparam int unsigned B_PAYLOAD_W =
        AXI_IDW + 2 + AXI_USERW;
    localparam int unsigned AR_PAYLOAD_W =
        AXI_IDW + AXI_ADDRW + 8 + 3 + 2 + AXI_USERW + 4;
    localparam int unsigned R_PAYLOAD_W =
        AXI_IDW + AXI_USERW + AXI_DATAW + 2 + 1;
    localparam int unsigned WRITE_ORDER_COUNTW = $clog2(FIFO_DEPTH + 2);

    logic [AW_PAYLOAD_W-1:0] aw_wdata;
    logic [AW_PAYLOAD_W-1:0] aw_rdata;
    logic                    aw_fifo_rvalid;
    logic                    aw_fifo_rready;

    logic [W_PAYLOAD_W-1:0]  w_wdata;
    logic [W_PAYLOAD_W-1:0]  w_rdata;
    logic                    w_fifo_rvalid;
    logic                    w_fifo_rready;

    logic [B_PAYLOAD_W-1:0]  b_wdata;
    logic [B_PAYLOAD_W-1:0]  b_rdata;

    logic [AR_PAYLOAD_W-1:0] ar_wdata;
    logic [AR_PAYLOAD_W-1:0] ar_rdata;

    logic [R_PAYLOAD_W-1:0]  r_wdata;
    logic [R_PAYLOAD_W-1:0]  r_rdata;

    logic [WRITE_ORDER_COUNTW-1:0] m_write_order_count;
    logic                          m_aw_fire;
    logic                          m_w_fire;
    logic                          m_w_allowed;

    assign aw_wdata = {
        s_awid,
        s_awaddr,
        s_awlen,
        s_awsize,
        s_awburst,
        s_awuser,
        s_awqos
    };

    assign {
        m_awid,
        m_awaddr,
        m_awlen,
        m_awsize,
        m_awburst,
        m_awuser,
        m_awqos
    } = aw_rdata;

    assign m_awvalid = aw_fifo_rvalid;
    assign aw_fifo_rready = m_awready;
    assign m_aw_fire = m_awvalid && m_awready;

    yuquan_async_fifo #(
        .WIDTH(AW_PAYLOAD_W),
        .DEPTH(FIFO_DEPTH)
    ) u_aw_fifo (
        .wclk(s_clk),
        .wrstn(s_resetn),
        .wvalid(s_awvalid),
        .wready(s_awready),
        .wdata(aw_wdata),
        .rclk(m_clk),
        .rrstn(m_resetn),
        .rvalid(aw_fifo_rvalid),
        .rready(aw_fifo_rready),
        .rdata(aw_rdata)
    );

    assign w_wdata = {
        s_wuser,
        s_wdata,
        s_wstrb,
        s_wlast
    };

    assign {
        m_wuser,
        m_wdata,
        m_wstrb,
        m_wlast
    } = w_rdata;

    assign m_w_allowed = (m_write_order_count != '0) || m_aw_fire;
    assign m_wvalid = w_fifo_rvalid && m_w_allowed;
    assign w_fifo_rready = m_wready && m_w_allowed;
    assign m_w_fire = m_wvalid && m_wready;

    always_ff @(posedge m_clk or negedge m_resetn) begin
        if (!m_resetn) begin
            m_write_order_count <= '0;
        end else begin
            unique case ({m_aw_fire, m_w_fire && m_wlast})
                2'b10: m_write_order_count <= m_write_order_count + 1'b1;
                2'b01: m_write_order_count <= m_write_order_count - 1'b1;
                default: begin
                end
            endcase
        end
    end

    yuquan_async_fifo #(
        .WIDTH(W_PAYLOAD_W),
        .DEPTH(FIFO_DEPTH)
    ) u_w_fifo (
        .wclk(s_clk),
        .wrstn(s_resetn),
        .wvalid(s_wvalid),
        .wready(s_wready),
        .wdata(w_wdata),
        .rclk(m_clk),
        .rrstn(m_resetn),
        .rvalid(w_fifo_rvalid),
        .rready(w_fifo_rready),
        .rdata(w_rdata)
    );

    assign b_wdata = {
        m_bid,
        m_bresp,
        m_buser
    };

    assign {
        s_bid,
        s_bresp,
        s_buser
    } = b_rdata;

    yuquan_async_fifo #(
        .WIDTH(B_PAYLOAD_W),
        .DEPTH(FIFO_DEPTH)
    ) u_b_fifo (
        .wclk(m_clk),
        .wrstn(m_resetn),
        .wvalid(m_bvalid),
        .wready(m_bready),
        .wdata(b_wdata),
        .rclk(s_clk),
        .rrstn(s_resetn),
        .rvalid(s_bvalid),
        .rready(s_bready),
        .rdata(b_rdata)
    );

    assign ar_wdata = {
        s_arid,
        s_araddr,
        s_arlen,
        s_arsize,
        s_arburst,
        s_aruser,
        s_arqos
    };

    assign {
        m_arid,
        m_araddr,
        m_arlen,
        m_arsize,
        m_arburst,
        m_aruser,
        m_arqos
    } = ar_rdata;

    yuquan_async_fifo #(
        .WIDTH(AR_PAYLOAD_W),
        .DEPTH(FIFO_DEPTH)
    ) u_ar_fifo (
        .wclk(s_clk),
        .wrstn(s_resetn),
        .wvalid(s_arvalid),
        .wready(s_arready),
        .wdata(ar_wdata),
        .rclk(m_clk),
        .rrstn(m_resetn),
        .rvalid(m_arvalid),
        .rready(m_arready),
        .rdata(ar_rdata)
    );

    assign r_wdata = {
        m_rid,
        m_ruser,
        m_rdata,
        m_rresp,
        m_rlast
    };

    assign {
        s_rid,
        s_ruser,
        s_rdata,
        s_rresp,
        s_rlast
    } = r_rdata;

    yuquan_async_fifo #(
        .WIDTH(R_PAYLOAD_W),
        .DEPTH(FIFO_DEPTH)
    ) u_r_fifo (
        .wclk(m_clk),
        .wrstn(m_resetn),
        .wvalid(m_rvalid),
        .wready(m_rready),
        .wdata(r_wdata),
        .rclk(s_clk),
        .rrstn(s_resetn),
        .rvalid(s_rvalid),
        .rready(s_rready),
        .rdata(r_rdata)
    );
endmodule

module yuquan_async_fifo #(
    parameter int unsigned WIDTH = 1,
    parameter int unsigned DEPTH = 16
) (
    input  logic             wclk,
    input  logic             wrstn,
    input  logic             wvalid,
    output logic             wready,
    input  logic [WIDTH-1:0] wdata,
    input  logic             rclk,
    input  logic             rrstn,
    output logic             rvalid,
    input  logic             rready,
    output logic [WIDTH-1:0] rdata
);
    localparam int unsigned PTRW = $clog2(DEPTH);

    logic [WIDTH-1:0] mem [0:DEPTH-1];

    logic [PTRW:0] wbin;
    logic [PTRW:0] wbin_next;
    logic [PTRW:0] wgray;
    logic [PTRW:0] wgray_next;
    logic [PTRW:0] rgray_wclk_meta;
    logic [PTRW:0] rgray_wclk_sync;
    logic          wfull;
    logic          winc;

    logic [PTRW:0] rbin;
    logic [PTRW:0] rbin_next;
    logic [PTRW:0] rgray;
    logic [PTRW:0] rgray_next;
    logic [PTRW:0] wgray_rclk_meta;
    logic [PTRW:0] wgray_rclk_sync;
    logic          rempty;
    logic          rinc;

    initial begin
        if (DEPTH < 2 || (DEPTH & (DEPTH - 1)) != 0) begin
            $fatal(1, "yuquan_async_fifo DEPTH must be a power of two and at least 2");
        end
    end

    assign wready = !wfull;
    assign winc = wvalid && wready;
    assign wbin_next = wbin + {{PTRW{1'b0}}, winc};
    assign wgray_next = (wbin_next >> 1) ^ wbin_next;

    always_ff @(posedge wclk or negedge wrstn) begin
        if (!wrstn) begin
            wbin <= '0;
            wgray <= '0;
            rgray_wclk_meta <= '0;
            rgray_wclk_sync <= '0;
            wfull <= 1'b0;
        end else begin
            rgray_wclk_meta <= rgray;
            rgray_wclk_sync <= rgray_wclk_meta;

            if (winc) begin
                mem[wbin[PTRW-1:0]] <= wdata;
            end

            wbin <= wbin_next;
            wgray <= wgray_next;
            wfull <= (wgray_next == {
                ~rgray_wclk_sync[PTRW:PTRW-1],
                rgray_wclk_sync[PTRW-2:0]
            });
        end
    end

    assign rvalid = !rempty;
    assign rinc = rvalid && rready;
    assign rbin_next = rbin + {{PTRW{1'b0}}, rinc};
    assign rgray_next = (rbin_next >> 1) ^ rbin_next;
    assign rdata = mem[rbin[PTRW-1:0]];

    always_ff @(posedge rclk or negedge rrstn) begin
        if (!rrstn) begin
            rbin <= '0;
            rgray <= '0;
            wgray_rclk_meta <= '0;
            wgray_rclk_sync <= '0;
            rempty <= 1'b1;
        end else begin
            wgray_rclk_meta <= wgray;
            wgray_rclk_sync <= wgray_rclk_meta;

            rbin <= rbin_next;
            rgray <= rgray_next;
            rempty <= (rgray_next == wgray_rclk_sync);
        end
    end
endmodule
