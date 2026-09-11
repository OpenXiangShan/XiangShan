module iommu_atd_cache_ram_wrap #(//{{{
    parameter RAM_WIDTH               = 64,
    parameter ADDR_WIDTH              = 6
)(
   CLK,
   D,
   Q,
   CEN,
   WEN,
   A
 );
//----------------------------------------------------------------------------
// port definitions
//----------------------------------------------------------------------------
input  wire                         CLK; // clock
input  wire [RAM_WIDTH-1:0]         D;   // Write data to TLB RAM
output wire [RAM_WIDTH-1:0]         Q;   // Read data from TLB RAM
input  wire                         CEN; // chip enable to TLB RAM
input  wire                         WEN; // write enable to TLB RAM
input  wire [ADDR_WIDTH-1:0]        A;   // TLB Address

localparam RAM_DEPTH = 2**ADDR_WIDTH;

`ifdef IOMMU_IMPLEMENTATION //{{{
    genvar i,j;
    generate
        //--- use ff if depth is too small
        if(ADDR_WIDTH<4 | RAM_WIDTH<31) begin
            iommu_atd_cache_sp_ram_model #(RAM_WIDTH, ADDR_WIDTH) U_ram_SMALL_CFG(
            .CLK        (CLK  ),
            .D          (D    ),
            .Q          (Q    ),
            .CEN        (CEN  ),
            .WEN        (WEN  ),
            .A          (A    )
            );
        end
        //--- ATTENTION: modify following IMPLEMENTATION branch code with real ram lib
        else if(RAM_WIDTH==62 && ADDR_WIDTH==4) begin
            sram_acd_16x62 U_ram_DONTTOUCH(
                .RW0_clk    (CLK    ),
                .RW0_addr   (A      ),
                .RW0_en     (~CEN    ),
                .RW0_wmode  (~WEN    ),
                .RW0_wdata  (D      ),
                .RW0_rdata  (Q      )
            );
        end
        else if(RAM_WIDTH==84  && ADDR_WIDTH==4) begin
            sram_acd_16x84 U_ram_DONTTOUCH(
                .RW0_clk    (CLK    ),
                .RW0_addr   (A      ),
                .RW0_en     (~CEN    ),
                .RW0_wmode  (~WEN    ),
                .RW0_wdata  (D      ),
                .RW0_rdata  (Q      )
            );
        end
        else if(RAM_WIDTH==106  && ADDR_WIDTH==4) begin
            sram_acd_16x106 U_ram_DONTTOUCH(
                .RW0_clk    (CLK    ),
                .RW0_addr   (A      ),
                .RW0_en     (~CEN    ),
                .RW0_wmode  (~WEN    ),
                .RW0_wdata  (D      ),
                .RW0_rdata  (Q      )
            );
        end
        else if(RAM_WIDTH==79  && ADDR_WIDTH==4) begin
            sram_acd_16x79 U_ram_DONTTOUCH(
                .RW0_clk    (CLK    ),
                .RW0_addr   (A      ),
                .RW0_en     (~CEN    ),
                .RW0_wmode  (~WEN    ),
                .RW0_wdata  (D      ),
                .RW0_rdata  (Q      )
            );
        end
        else if(RAM_WIDTH==53  && ADDR_WIDTH==4) begin
            sram_acd_16x53 U_ram_DONTTOUCH(
                .RW0_clk    (CLK    ),
                .RW0_addr   (A      ),
                .RW0_en     (~CEN    ),
                .RW0_wmode  (~WEN    ),
                .RW0_wdata  (D      ),
                .RW0_rdata  (Q      )
            );
        end
        else if(RAM_WIDTH==306  && ADDR_WIDTH==4) begin
            sram_acd_16x306 U_ram_DONTTOUCH(
                .RW0_clk    (CLK    ),
                .RW0_addr   (A      ),
                .RW0_en     (~CEN    ),
                .RW0_wmode  (~WEN    ),
                .RW0_wdata  (D      ),
                .RW0_rdata  (Q      )
            );
        end
        else if(RAM_WIDTH==32  && ADDR_WIDTH==4) begin
            sram_acd_16x32 U_ram_DONTTOUCH(
                .RW0_clk    (CLK    ),
                .RW0_addr   (A      ),
                .RW0_en     (~CEN    ),
                .RW0_wmode  (~WEN    ),
                .RW0_wdata  (D      ),
                .RW0_rdata  (Q      )
            );
        end
        else begin
            iommu_atd_cache_sp_ram_model #(RAM_WIDTH, ADDR_WIDTH) U_ram_ERROR_CFG(
            .CLK        (CLK  ),
            .D          (D    ),
            .Q          (Q    ),
            .CEN        (CEN  ),
            .WEN        (WEN  ),
            .A          (A    )
            );
        end
    endgenerate
    //}}}
`else //{{{
    iommu_atd_cache_sp_ram_model #(RAM_WIDTH, ADDR_WIDTH) U_ram_DONTTOUCH(
    .CLK        (CLK  ),
    .D          (D    ),
    .Q          (Q    ),
    .CEN        (CEN  ),
    .WEN        (WEN  ),
    .A          (A    )
    );
    //}}}
`endif
endmodule //}}}

module iommu_atd_cache_sp_ram_model #(//{{{
    parameter RAM_WIDTH               = 64,
    parameter ADDR_WIDTH              = 6
)(
   CLK,
   D,
   Q,
   CEN,
   WEN,
   A
 );
//----------------------------------------------------------------------------
// parameters
//----------------------------------------------------------------------------
localparam RAM_DEPTH               = 2**ADDR_WIDTH;
//----------------------------------------------------------------------------
// port definitions
//----------------------------------------------------------------------------
input  wire                         CLK; // clock
input  wire [RAM_WIDTH-1:0]         D;   // Write data to TLB RAM
output reg  [RAM_WIDTH-1:0]         Q;   // Read data from TLB RAM
input  wire                         CEN; // chip enable to TLB RAM
input  wire                         WEN; // write enable to TLB RAM
input  wire [ADDR_WIDTH-1:0]        A;   // TLB Address

// RAM for modeling the Register File
//`ifdef SYNTHESIS
//(* ram_style="block" *) reg   [RAM_WIDTH-1:0] RF_MODEL [RAM_DEPTH-1:0];
//`else
reg   [RAM_WIDTH-1:0] RF_MODEL [RAM_DEPTH-1:0];
//`endif
// D-input of Q
wire  [RAM_WIDTH-1:0]   nxt_Q;

// Store the Address if both CEN and WEN are valid
always @(posedge CLK)
begin
  if (~CEN & ~WEN) begin
    RF_MODEL[A] <= D;
  end
end

//`ifdef SYNTHESIS
//assign nxt_Q =                           RF_MODEL[A];
//`else
// IF CEN is valid drive valid data, else X
assign nxt_Q = CEN ? {RAM_WIDTH{1'b0}} : RF_MODEL[A];
//`endif

// Clock process for Q
always @(posedge CLK)
begin
  Q <= nxt_Q;
end

endmodule //}}}

