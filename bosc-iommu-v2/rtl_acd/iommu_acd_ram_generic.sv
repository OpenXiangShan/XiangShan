module iommu_acd_tlb_ram_wrap #(//{{{
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
        if(ADDR_WIDTH<4 | RAM_WIDTH<87) begin
            iommu_acd_sp_ram_model #(RAM_WIDTH, ADDR_WIDTH) U_ram_SMALL_CFG(
            .CLK        (CLK  ),
            .D          (D    ),
            .Q          (Q    ),
            .CEN        (CEN  ),
            .WEN        (WEN  ),
            .A          (A    )
            );
        end
        //--- ATTENTION: modify following IMPLEMENTATION branch code with real ram lib, tlb_ram_wrap
        else if(RAM_WIDTH==121 && ADDR_WIDTH==4) begin
            sram_acd_16x121 U_ram_DONTTOUCH(
                .RW0_clk    (CLK    ),
                .RW0_addr   (A      ),
                .RW0_en     (~CEN    ),
                .RW0_wmode  (~WEN    ),
                .RW0_wdata  (D      ),
                .RW0_rdata  (Q      )
            );
        end
        else if(RAM_WIDTH==87  && ADDR_WIDTH==4) begin
            sram_acd_16x87 U_ram_DONTTOUCH(
                .RW0_clk    (CLK    ),
                .RW0_addr   (A      ),
                .RW0_en     (~CEN    ),
                .RW0_wmode  (~WEN    ),
                .RW0_wdata  (D      ),
                .RW0_rdata  (Q      )
            );
        end
        else if(RAM_WIDTH==95  && ADDR_WIDTH==4) begin
            sram_acd_16x95 U_ram_DONTTOUCH(
                .RW0_clk    (CLK    ),
                .RW0_addr   (A      ),
                .RW0_en     (~CEN    ),
                .RW0_wmode  (~WEN    ),
                .RW0_wdata  (D      ),
                .RW0_rdata  (Q      )
            );
        end
        else begin
            iommu_acd_sp_ram_model #(RAM_WIDTH, ADDR_WIDTH) U_ram_ERROR_CFG(
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
    iommu_acd_sp_ram_model #(RAM_WIDTH, ADDR_WIDTH) U_ram_DONTTOUCH(
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

module iommu_acd_sp_ram_model #(//{{{
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

module iommu_acd_wbuf_ram_wrap #( //{{{
    parameter int  unsigned  AWIDTH     = 4,
    parameter int  unsigned  DWIDTH     = 128
) (
    CLK,
    WEA,
    MEA,
    ADDRA,
    DINA,
    MEB,
    ADDRB,
    DOUTB
);
    input  wire                         CLK;
    input  wire                         WEA;
    input  wire                         MEA;
    input  wire [AWIDTH-1:0]            ADDRA;
    input  wire [DWIDTH-1:0]            DINA;
    input  wire                         MEB;
    input  wire [AWIDTH-1:0]            ADDRB;
    output wire [DWIDTH-1:0]            DOUTB;

`ifdef IOMMU_IMPLEMENTATION //{{{
    genvar i,j;
    generate
        //--- use ff if depth is too small
        if(AWIDTH<5) begin
            iommu_acd_dpram_model #(
            /*parameter int  unsigned */ .AWIDTH        (AWIDTH     ), //= 4,
            /*parameter int  unsigned */ .DWIDTH        (DWIDTH     )  //= 128
            ) U_ram_SMALL_CFG(
            /*input  logic                          */  .CLK        (CLK        ),
            /*input  logic                          */  .WEA        (WEA        ),
            /*input  logic                          */  .MEA        (MEA        ),
            /*input  logic [AWIDTH-1:0]             */  .ADDRA      (ADDRA      ),
            /*input  logic [DWIDTH-1:0]             */  .DINA       (DINA       ),
            /*input  logic                          */  .MEB        (MEB        ),
            /*input  logic [AWIDTH-1:0]             */  .ADDRB      (ADDRB      ),
            /*output logic [DWIDTH-1:0]             */  .DOUTB      (DOUTB      )
            );
        end
        //--- ATTENTION: modify following IMPLEMENTATION branch code with real ram lib, wbuf_ram_wrap
        else if(DWIDTH==297 && AWIDTH==5) begin
            sram_acd_2p32x297 U_ram_DONTTOUCH(
                .RW0_clk    (CLK        ),
                .RW0_addra  (ADDRA      ),
                .RW0_addrb  (ADDRB      ),
                .RW0_wen    (~MEA & ~WEA),
                .RW0_ren    (~MEB       ),
                .RW0_wdata  (DINA       ),
                .RW0_rdata  (DOUTB      )
            );
        end
        else if(DWIDTH==333 && AWIDTH==5) begin
            sram_acd_2p32x333 U_ram_DONTTOUCH(
                .RW0_clk    (CLK        ),
                .RW0_addra  (ADDRA      ),
                .RW0_addrb  (ADDRB      ),
                .RW0_wen    (~MEA & ~WEA),
                .RW0_ren    (~MEB       ),
                .RW0_wdata  (DINA       ),
                .RW0_rdata  (DOUTB      )
            );
        end
        //-- ATTENTION END
        else begin
            iommu_acd_dpram_model #(
            /*parameter int  unsigned */ .AWIDTH        (AWIDTH     ), //= 4,
            /*parameter int  unsigned */ .DWIDTH        (DWIDTH     )  //= 128
            ) U_ram_ERR_CFG(
            /*input  logic                          */  .CLK        (CLK        ),
            /*input  logic                          */  .WEA        (WEA        ),
            /*input  logic                          */  .MEA        (MEA        ),
            /*input  logic [AWIDTH-1:0]             */  .ADDRA      (ADDRA      ),
            /*input  logic [DWIDTH-1:0]             */  .DINA       (DINA       ),
            /*input  logic                          */  .MEB        (MEB        ),
            /*input  logic [AWIDTH-1:0]             */  .ADDRB      (ADDRB      ),
            /*output logic [DWIDTH-1:0]             */  .DOUTB      (DOUTB      )
            );
        end
    endgenerate
    //}}}
`else //{{{
    generate
        iommu_acd_dpram_model #(
        /*parameter int  unsigned */ .AWIDTH        (AWIDTH     ), //= 4,
        /*parameter int  unsigned */ .DWIDTH        (DWIDTH     )  //= 128
        ) U_ram_DONTTOUCH(
        /*input  logic                          */  .CLK        (CLK        ),
        /*input  logic                          */  .WEA        (WEA        ),
        /*input  logic                          */  .MEA        (MEA        ),
        /*input  logic [AWIDTH-1:0]             */  .ADDRA      (ADDRA      ),
        /*input  logic [DWIDTH-1:0]             */  .DINA       (DINA       ),
        /*input  logic                          */  .MEB        (MEB        ),
        /*input  logic [AWIDTH-1:0]             */  .ADDRB      (ADDRB      ),
        /*output logic [DWIDTH-1:0]             */  .DOUTB      (DOUTB      )
        );
    endgenerate
    //}}}
`endif
endmodule //}}}

module iommu_acd_dpram_model #( //{{{
    parameter int  unsigned  AWIDTH     = 4,
    parameter int  unsigned  DWIDTH     = 128
) (
    input  logic                        CLK,
    input  logic                        WEA,
    input  logic                        MEA,
    input  logic [AWIDTH-1:0]           ADDRA,
    input  logic [DWIDTH-1:0]           DINA,
    input  logic                        MEB,
    input  logic [AWIDTH-1:0]           ADDRB,
    output logic [DWIDTH-1:0]           DOUTB
);
//`ifdef SYNTHESIS
//(* ram_style="block" *) reg [DWIDTH-1:0]  mem[(1<<AWIDTH)-1:0];
//reg [DWIDTH-1:0]  memreg;
//
//assign DOUTB = memreg;
//
//always@(posedge CLK) begin
//    if (~WEA & ~MEA) begin
//        mem[ADDRA] <= DINA;
//    end
//end
//
//
//always@(posedge CLK) begin
//    if (~MEB) begin
//        memreg <= mem[ADDRB];
//    end
////    else begin
////        memreg <= 'bx;
////    end
//end
//`else
logic [DWIDTH-1:0]  mem[(1<<AWIDTH)-1:0];
logic [DWIDTH-1:0]  memreg;

assign DOUTB = memreg;

always@(posedge CLK) begin
    if (~WEA & ~MEA) begin
        mem[ADDRA] <= DINA;
    end
end


always@(posedge CLK) begin
    if (~MEB) begin
        memreg <= mem[ADDRB];
    end
    else begin
        memreg <= 'b0;
    end
end
//`endif


endmodule //}}}

