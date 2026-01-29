//------------------------------------------------------------------------------
/*
    Copyright all by the Beijing Institute of Open Source Chip

    Filename    : cdb_ingress_channel
    File-history: 
       (1) Zhou Shize created in 20250610: Build CDB channel;

*/
//------------------------------------------------------------------------------
`include "common_defines.sv"
`include "router_define.sv"

module cdb_ingress_channel #(
    parameter CDB_FIFO_DEPTH            = 8         ,
    parameter CDB_BP_FIFO_DEPTH         = 4         ,
    parameter CDB_FLIT_WIDTH            = 8         ,
    parameter CDB_BYPASS                = 1         ,
    parameter CHANNEL                   = 0   

)(
    clk_in              ,
    rstn_in             ,
    rx_flitpend         ,
    rx_flitv            ,
    rx_flit             ,
    rxcrdv              ,
    rxcrd_full          ,
    en_crdv             ,
    rptr_r_e2in         ,
    wptr_r_in2e         ,
    cdb_fifo_data_in2e       
);

    //*** PARAMETER ****************************************************************

    localparam  FLIT_OPCODE_LEFT  = CHANNEL == `CHANNEL_REQ ? `DSU_CHI_REQ_FLIT_OPCODE_LEFT : CHANNEL == `CHANNEL_RSP ? `DSU_CHI_RSP_FLIT_OPCODE_LEFT : CHANNEL == `CHANNEL_SNP ? `DSU_CHI_SNP_FLIT_OPCODE_LEFT : `DSU_CHI_DAT_FLIT_OPCODE_LEFT;
    localparam  FLIT_OPCODE_RIGHT = CHANNEL == `CHANNEL_REQ ? `DSU_CHI_REQ_FLIT_OPCODE_RIGHT : CHANNEL == `CHANNEL_RSP ? `DSU_CHI_RSP_FLIT_OPCODE_RIGHT : CHANNEL == `CHANNEL_SNP ? `DSU_CHI_SNP_FLIT_OPCODE_RIGHT : `DSU_CHI_DAT_FLIT_OPCODE_RIGHT;


    //*** INPUT/OUTPUT *************************************************************

    input  wire                                         clk_in             ;
    input  wire                                         rstn_in            ;
    input  wire                                         rx_flitpend        ;
    input  wire                                         rx_flitv           ;
    input  wire [CDB_FLIT_WIDTH                -1:0]    rx_flit            ;
    output wire                                         rxcrdv             ;
    output wire                                         rxcrd_full         ;
    input  wire                                         en_crdv            ;
    output wire [CDB_FLIT_WIDTH*CDB_FIFO_DEPTH -1:0]    cdb_fifo_data_in2e ;
    input  wire [CDB_FIFO_DEPTH                -1:0]    rptr_r_e2in        ;
    output wire [CDB_FIFO_DEPTH                -1:0]    wptr_r_in2e        ;







    //*** WIRE/REG *****************************************************************
    wire                                is_link_flit                    ;
    wire                                real_flitv                      ;
    wire                                bp_flitv                        ;
    reg   [CDB_BP_FIFO_DEPTH     -1:0]  crd2send_ns                     ;
    reg   [CDB_BP_FIFO_DEPTH     -1:0]  crd2send                        ;
    wire                                bp_fifo_push                    ;
    wire                                bp_fifo_pop                     ;
    wire                                bp_fifo_nempty                  ;
    wire  [CDB_FLIT_WIDTH        -1:0]  bp_fifo_dout                    ;
    wire                                cdb_fifo_wen                    ;
    wire                                cdb_fifo_full_i                 ;
    wire                                cdb_fifo_empty_i                ;
    wire  [CDB_FIFO_DEPTH        -1:0]  wr_en                           ;
    wire  [CDB_FIFO_DEPTH        -1:0]  wptr                            ;
    wire  [CDB_FIFO_DEPTH        -1:0]  wptr_oh                         ;
    wire  [CDB_FIFO_DEPTH        -1:0]  sync_rptr                       ;
    reg   [CDB_FIFO_DEPTH        -1:0]  wptr_r                          ;
    reg   [CDB_FIFO_DEPTH        -1:0]  wptr_oh_r                       ;
    wire  [CDB_FLIT_WIDTH        -1:0]  cdb_flit_in                     ;
    reg   [CDB_FLIT_WIDTH        -1:0]  cdb_fifo_q [CDB_FIFO_DEPTH-1 :0];

    //*** MAIN BODY ****************************************************************

    //IN_DOMAIN
    assign is_link_flit = rx_flitv & (~|rx_flit[FLIT_OPCODE_LEFT : FLIT_OPCODE_RIGHT]);
    assign real_flitv   = rx_flitv & (|rx_flit[FLIT_OPCODE_LEFT : FLIT_OPCODE_RIGHT]) ;



    always@(*) begin 
        if ((crd2send[0] & en_crdv ) & ~bp_flitv) begin
            crd2send_ns = {1'b0,crd2send[CDB_BP_FIFO_DEPTH-1:1]};
        end else if ((~crd2send[0] & bp_flitv) | is_link_flit) begin
            crd2send_ns = {crd2send[CDB_BP_FIFO_DEPTH-2:0],1'b1};
        end else begin 
            crd2send_ns = crd2send                              ;
        end
    end

    always @(posedge clk_in or negedge rstn_in) begin
        if (!rstn_in) begin
            crd2send <= #`NOC_DELAY {CDB_BP_FIFO_DEPTH{1'b1}};
        end else begin
            crd2send <= #`NOC_DELAY crd2send_ns              ;
        end
    end

    assign rxcrd_full = crd2send[CDB_BP_FIFO_DEPTH-1];
    assign rxcrdv     = crd2send[0] & en_crdv        ;



    fifo #(
        .CLOCK_ENABLE    (0             ),
        .FIFO_WIDTH      (CDB_FLIT_WIDTH),
        .FIFO_DEPTH      (4             )
    ) U_BP_FIFO(
        .clock      (clk_in             ),
        .rstn       (rstn_in            ),
        .push       (bp_fifo_push       ),
        .pop        (bp_fifo_pop        ),
        .din        (rx_flit            ),
        .cg_en      (1'b1               ),
        .DFT_SE     (1'b0               ),
        .fifo_cnt   (                   ),
        .full       (                   ),
        .nempty     (bp_fifo_nempty     ),
        .dout       (bp_fifo_dout       )
    );


    assign cdb_fifo_wen     = (CDB_BYPASS == 1) ? (bp_fifo_nempty & ~cdb_fifo_full_i) | (real_flitv & ~bp_fifo_push)
                                                :  bp_fifo_nempty & ~cdb_fifo_full_i  ;

    assign bp_fifo_push     = (CDB_BYPASS == 1) ? real_flitv & (cdb_fifo_full_i | bp_fifo_nempty ) 
                                                : real_flitv ;
    assign bp_fifo_pop      = bp_fifo_nempty & ~cdb_fifo_full_i         ;

    assign bp_flitv         = cdb_fifo_wen                              ;

    assign cdb_flit_in      = (CDB_BYPASS == 1) ? bp_fifo_nempty ? bp_fifo_dout : rx_flit   
                                                : bp_fifo_dout   ;

    assign cdb_fifo_full_i  = (wptr_r ==  ~sync_rptr)          ;
    assign cdb_fifo_empty_i = (wptr_r ==   sync_rptr)          ;

    assign wptr[CDB_FIFO_DEPTH-1:0] = {wptr_r[CDB_FIFO_DEPTH-2:0], ~wptr_r[CDB_FIFO_DEPTH-1]};

    always @(posedge clk_in or negedge rstn_in) begin
        if (!rstn_in) begin
            wptr_r <= #`NOC_DELAY {CDB_FIFO_DEPTH{1'b0}};
        end else if (cdb_fifo_wen) begin
            wptr_r <= #`NOC_DELAY wptr                  ;
        end
    end

    assign wptr_r_in2e = wptr_r;

    assign wptr_oh[CDB_FIFO_DEPTH-1:0] = {wptr_oh_r[CDB_FIFO_DEPTH-2:0], wptr_oh_r[CDB_FIFO_DEPTH-1]};

    always @(posedge clk_in or negedge rstn_in) begin
        if (!rstn_in) begin
            wptr_oh_r <= #`NOC_DELAY {{(CDB_FIFO_DEPTH-1){1'b0}},1'b1}  ;
        end else if (cdb_fifo_wen) begin
            wptr_oh_r <= #`NOC_DELAY wptr_oh                            ;
        end
    end

    genvar i;
    generate
        for( i = 0;i < CDB_FIFO_DEPTH;i = i + 1 ) begin: DFF_CDB_FIFO
            assign wr_en[i] = wptr_oh_r[i] & cdb_fifo_wen;
            always @(posedge clk_in) begin 
                if( wr_en[i] == 1'b1)
                    cdb_fifo_q[i][CDB_FLIT_WIDTH-1:0] <= #`NOC_DELAY cdb_flit_in[CDB_FLIT_WIDTH-1:0];
            end
            assign cdb_fifo_data_in2e[CDB_FLIT_WIDTH*(i+1)-1:CDB_FLIT_WIDTH*i] = cdb_fifo_q[i];
        end
    endgenerate



    sync_dff #(.WIDTH(CDB_FIFO_DEPTH)) U_SYNC_DFF_RPTR (.clk(clk_in), .rstn(rstn_in), .sync_i (rptr_r_e2in[CDB_FIFO_DEPTH-1:0]), .sync_o(sync_rptr[CDB_FIFO_DEPTH-1:0]));








   //*** DEBUG ********************************************************************


endmodule
