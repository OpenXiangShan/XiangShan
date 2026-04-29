//------------------------------------------------------------------------------
/*
    Copyright all by the Beijing Institute of Open Source Chip

    Filename    : cdb_egress_channel
    File-history: 
       (1) Zhou Shize created in 20250610: Build CDB channel;

*/
//------------------------------------------------------------------------------
`include "common_defines.sv"
`include "router_define.sv"

module cdb_egress_channel #(
    parameter CDB_FIFO_DEPTH            = 8         ,
    parameter CDB_BP_FIFO_DEPTH         = 4         ,
    parameter CDB_FLIT_WIDTH            = 8         ,
    parameter CDB_BYPASS                = 1         ,
    parameter CHANNEL                   = 0   

)(
    clk_out             ,
    rstn_out            ,
    tx_flitpend         ,
    tx_flitv            ,
    tx_flit             ,
    txcrdv              ,
    cdb_fifo_nempty     ,
    en_flitv            ,
    rptr_r_e2in         ,
    wptr_r_in2e         ,
    cdb_fifo_data_in2e  ,    
    link_deactive 
);

    //*** PARAMETER ****************************************************************

    localparam  FLIT_OPCODE_LEFT  = CHANNEL == `CHANNEL_REQ ? `DSU_CHI_REQ_FLIT_OPCODE_LEFT : CHANNEL == `CHANNEL_RSP ? `DSU_CHI_RSP_FLIT_OPCODE_LEFT : CHANNEL == `CHANNEL_SNP ? `DSU_CHI_SNP_FLIT_OPCODE_LEFT : `DSU_CHI_DAT_FLIT_OPCODE_LEFT;
    localparam  FLIT_OPCODE_RIGHT = CHANNEL == `CHANNEL_REQ ? `DSU_CHI_REQ_FLIT_OPCODE_RIGHT : CHANNEL == `CHANNEL_RSP ? `DSU_CHI_RSP_FLIT_OPCODE_RIGHT : CHANNEL == `CHANNEL_SNP ? `DSU_CHI_SNP_FLIT_OPCODE_RIGHT : `DSU_CHI_DAT_FLIT_OPCODE_RIGHT;


    //*** INPUT/OUTPUT *************************************************************

    input  wire                                         clk_out            ;
    input  wire                                         rstn_out           ;
    output wire                                         tx_flitpend        ;
    output wire                                         tx_flitv           ;
    output wire [CDB_FLIT_WIDTH                -1:0]    tx_flit            ;  
    input  wire                                         txcrdv             ;
    output wire                                         cdb_fifo_nempty    ;
    input  wire                                         en_flitv           ;
    input  wire                                         link_deactive      ;
    input  wire [CDB_FLIT_WIDTH*CDB_FIFO_DEPTH -1:0]    cdb_fifo_data_in2e ;
    output wire [CDB_FIFO_DEPTH                -1:0]    rptr_r_e2in        ;
    input  wire [CDB_FIFO_DEPTH                -1:0]    wptr_r_in2e        ;







    //*** WIRE/REG *****************************************************************
    wire                                txlcrd_avail                    ;
    wire                                cdb_fifo_ren                    ;
    wire                                cdb_fifo_full_o                 ;
    wire                                cdb_fifo_empty_o                ;
    wire                                rd_en                           ;
    wire  [CDB_FIFO_DEPTH        -1:0]  rptr                            ;
    wire  [CDB_FIFO_DEPTH        -1:0]  rptr_oh                         ;
    wire  [CDB_FIFO_DEPTH        -1:0]  sync_wptr                       ;
    reg   [CDB_FIFO_DEPTH        -1:0]  rptr_r                          ;
    reg   [CDB_FIFO_DEPTH        -1:0]  rptr_oh_r                       ;
    reg   [CDB_FLIT_WIDTH        -1:0]  cdb_dout                        ;
    reg                                 tx_flitv_w                      ;
    reg   [CDB_FLIT_WIDTH        -1:0]  tx_flit_w                       ;
    wire  [CDB_FLIT_WIDTH        -1:0]  cdb_fifo_q [CDB_FIFO_DEPTH-1 :0];

    //*** MAIN BODY ****************************************************************

    //OUT_DOMAIN

    sync_dff #(.WIDTH(CDB_FIFO_DEPTH)) U_SYNC_DFF_WPTR (.clk(clk_out), .rstn(rstn_out), .sync_i (wptr_r_in2e[CDB_FIFO_DEPTH-1:0]), .sync_o(sync_wptr[CDB_FIFO_DEPTH-1:0]));


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

    assign rptr_r_e2in = rptr_r;

    assign rptr_oh[CDB_FIFO_DEPTH-1:0] = {rptr_oh_r[CDB_FIFO_DEPTH-2:0], rptr_oh_r[CDB_FIFO_DEPTH-1]};

    always @(posedge clk_out or negedge rstn_out) begin
        if (!rstn_out) begin
            rptr_oh_r <= #`NOC_DELAY {{(CDB_FIFO_DEPTH-1){1'b0}},1'b1}  ;
        end else if (cdb_fifo_ren) begin
            rptr_oh_r <= #`NOC_DELAY rptr_oh                            ;
        end
    end


    genvar i;
    generate
        for( i = 0;i < CDB_FIFO_DEPTH;i = i + 1 ) begin
            assign cdb_fifo_q[i] = cdb_fifo_data_in2e[CDB_FLIT_WIDTH*(i+1)-1:CDB_FLIT_WIDTH*i];
        end
    endgenerate


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
