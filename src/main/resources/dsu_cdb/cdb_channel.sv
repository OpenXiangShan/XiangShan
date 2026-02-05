//------------------------------------------------------------------------------
/*
    Copyright all by the Beijing Institute of Open Source Chip

    Filename    : cdb_channel
    File-history: 
       (1) Zhou Shize created in 20250610: Build CDB channel;

*/
//------------------------------------------------------------------------------
`include "common_defines.sv"
`include "router_define.sv"

module cdb_channel #(
    parameter CDB_FIFO_DEPTH            = 8         ,
    parameter CDB_BP_FIFO_DEPTH         = 4         ,
    parameter CDB_FLIT_WIDTH            = 8         ,
    parameter CDB_BYPASS                = 1         ,
    parameter CHANNEL                   = 0   

)(
    clk_in              ,
    rstn_in             ,
    clk_out             ,
    rstn_out            ,
    rx_flitpend         ,
    rx_flitv            ,
    rx_flit             ,
    rxcrdv              ,
    tx_flitpend         ,
    tx_flitv            ,
    tx_flit             ,
    txcrdv              ,
    rxcrd_full          ,
    cdb_fifo_nempty     ,
    en_crdv             ,
    en_flitv            ,
    link_deactive 
);

    //*** PARAMETER ****************************************************************

    localparam  FLIT_OPCODE_LEFT  = CHANNEL == `CHANNEL_REQ ? `DSU_CHI_REQ_FLIT_OPCODE_LEFT : CHANNEL == `CHANNEL_RSP ? `DSU_CHI_RSP_FLIT_OPCODE_LEFT : CHANNEL == `CHANNEL_SNP ? `DSU_CHI_SNP_FLIT_OPCODE_LEFT : `DSU_CHI_DAT_FLIT_OPCODE_LEFT;
    localparam  FLIT_OPCODE_RIGHT = CHANNEL == `CHANNEL_REQ ? `DSU_CHI_REQ_FLIT_OPCODE_RIGHT : CHANNEL == `CHANNEL_RSP ? `DSU_CHI_RSP_FLIT_OPCODE_RIGHT : CHANNEL == `CHANNEL_SNP ? `DSU_CHI_SNP_FLIT_OPCODE_RIGHT : `DSU_CHI_DAT_FLIT_OPCODE_RIGHT;


    //*** INPUT/OUTPUT *************************************************************

    input  wire                         clk_in             ;
    input  wire                         rstn_in            ;
    input  wire                         clk_out            ;
    input  wire                         rstn_out           ;
    input  wire                         rx_flitpend        ;
    input  wire                         rx_flitv           ;
    input  wire [CDB_FLIT_WIDTH -1:0]   rx_flit            ;
    output wire                         rxcrdv             ;
    output wire                         tx_flitpend        ;
    output wire                         tx_flitv           ;
    output wire [CDB_FLIT_WIDTH -1:0]   tx_flit            ;  
    input  wire                         txcrdv             ;
    output wire                         rxcrd_full         ;
    output wire                         cdb_fifo_nempty    ;
    input  wire                         en_crdv            ;
    input  wire                         en_flitv           ;
    input  wire                         link_deactive      ;







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
    wire                                txlcrd_avail                    ;
    wire                                cdb_fifo_wen                    ;
    wire                                cdb_fifo_ren                    ;
    wire                                cdb_fifo_full_i                 ;
    wire                                cdb_fifo_empty_i                ;
    wire                                cdb_fifo_full_o                 ;
    wire                                cdb_fifo_empty_o                ;
    wire  [CDB_FIFO_DEPTH        -1:0]  wr_en                           ;
    wire                                rd_en                           ;
    wire  [CDB_FIFO_DEPTH        -1:0]  wptr                            ;
    wire  [CDB_FIFO_DEPTH        -1:0]  wptr_oh                         ;
    wire  [CDB_FIFO_DEPTH        -1:0]  rptr                            ;
    wire  [CDB_FIFO_DEPTH        -1:0]  rptr_oh                         ;
    wire  [CDB_FIFO_DEPTH        -1:0]  sync_wptr                       ;
    wire  [CDB_FIFO_DEPTH        -1:0]  sync_rptr                       ;
    reg   [CDB_FIFO_DEPTH        -1:0]  wptr_r                          ;
    reg   [CDB_FIFO_DEPTH        -1:0]  wptr_oh_r                       ;
    reg   [CDB_FIFO_DEPTH        -1:0]  rptr_r                          ;
    reg   [CDB_FIFO_DEPTH        -1:0]  rptr_oh_r                       ;
    wire  [CDB_FLIT_WIDTH        -1:0]  cdb_flit_in                     ;
    reg   [CDB_FLIT_WIDTH        -1:0]  cdb_fifo_q [CDB_FIFO_DEPTH-1 :0];
    reg   [CDB_FLIT_WIDTH        -1:0]  cdb_dout                        ;
    reg                                 tx_flitv_w                      ;
    reg   [CDB_FLIT_WIDTH        -1:0]  tx_flit_w                       ;

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
        end
    endgenerate

    sync_dff #(.WIDTH(CDB_FIFO_DEPTH)) U_SYNC_DFF_RPTR (.clk(clk_in), .rstn(rstn_in), .sync_i (rptr_r[CDB_FIFO_DEPTH-1:0]), .sync_o(sync_rptr[CDB_FIFO_DEPTH-1:0]));







    //OUT_DOMAIN

    sync_dff #(.WIDTH(CDB_FIFO_DEPTH)) U_SYNC_DFF_WPTR (.clk(clk_out), .rstn(rstn_out), .sync_i (wptr_r[CDB_FIFO_DEPTH-1:0]), .sync_o(sync_wptr[CDB_FIFO_DEPTH-1:0]));


    assign cdb_fifo_ren     = ~cdb_fifo_empty_o & ((txlcrd_avail & en_flitv) | ~tx_flitv_w) ;

    assign cdb_fifo_full_o  = (rptr_r ==  ~sync_wptr)          ;
    assign cdb_fifo_empty_o = (rptr_r ==   sync_wptr)          ;

    assign cdb_fifo_nempty  = ~cdb_fifo_empty_o                ;

    assign rd_en            = cdb_fifo_ren                     ;

    assign rptr[CDB_FIFO_DEPTH-1:0] = {rptr_r[CDB_FIFO_DEPTH-2:0], ~rptr_r[CDB_FIFO_DEPTH-1]};

    always @(posedge clk_out or negedge rstn_out) begin
        if (!rstn_out) begin
            rptr_r <= #`NOC_DELAY {CDB_FIFO_DEPTH{1'b0}};
        end else if (cdb_fifo_ren) begin
            rptr_r <= #`NOC_DELAY rptr                  ;
        end
    end

    assign rptr_oh[CDB_FIFO_DEPTH-1:0] = {rptr_oh_r[CDB_FIFO_DEPTH-2:0], rptr_oh_r[CDB_FIFO_DEPTH-1]};

    always @(posedge clk_out or negedge rstn_out) begin
        if (!rstn_out) begin
            rptr_oh_r <= #`NOC_DELAY {{(CDB_FIFO_DEPTH-1){1'b0}},1'b1}  ;
        end else if (cdb_fifo_ren) begin
            rptr_oh_r <= #`NOC_DELAY rptr_oh                            ;
        end
    end

    always @(*) begin : CDB_RD_DOUT
        cdb_dout[CDB_FLIT_WIDTH-1:0] = {CDB_FLIT_WIDTH{1'b0}};
        for( integer j = 0;j < CDB_FIFO_DEPTH;j = j + 1 ) begin
            cdb_dout[CDB_FLIT_WIDTH-1:0] = cdb_dout[CDB_FLIT_WIDTH-1:0] | (cdb_fifo_q[j][CDB_FLIT_WIDTH-1:0] & {CDB_FLIT_WIDTH{rptr_oh_r[j]}});
        end
    end


    always @(posedge clk_out or negedge rstn_out) begin
        if (!rstn_out) begin
            tx_flitv_w <= #`NOC_DELAY 1'b0    ;
        end else if (rd_en) begin
            tx_flitv_w <= #`NOC_DELAY 1'b1    ;
        end else if (txlcrd_avail & en_flitv) begin
            tx_flitv_w <= #`NOC_DELAY 1'b0    ;
        end
    end


    always @(posedge clk_out) begin
        if (rd_en) begin
            tx_flit_w  <= #`NOC_DELAY cdb_dout;
        end
    end

    assign tx_flitpend = rd_en      | tx_flitv_w                                                ;
    assign tx_flitv  =  (tx_flitv_w & txlcrd_avail & en_flitv) | (link_deactive & txlcrd_avail) ;
    assign tx_flit   =  tx_flit_w   & {CDB_FLIT_WIDTH{~link_deactive}}                          ;

    credit_manager #(
        .CRD_CNT_MAX  (`DEVICE_CRD_WIDTH),
        .CRD_CNT_SEL  (0)
    ) U_CREDIT_MANAGER_CDB (
        .clock       (clk_out            ),
        .rstn        (rstn_out           ),
        .inc         (txcrdv             ), 
        .dec         (tx_flitv           ),
        .crd_full    (                   ),
        .crd_avail   (txlcrd_avail       ),
        .crd_cnt_bin (                   )
    );


   //*** DEBUG ********************************************************************


endmodule
