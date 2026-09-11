////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////
module iommu_acd_bus_handler_mrif_agent #(//{{{
    parameter   BUS_INFLY_TOKEN_WIDTH   = 6,
    parameter   BUS_INFLY_TOKEN_NUM     = 2**BUS_INFLY_TOKEN_WIDTH,
    parameter   BUS_ID_WIDTH            = 8,
    parameter   BUS_ADDR_WIDTH          = 64,
    parameter   BUS_USER_WIDTH          = 8,
    parameter   BUS_DATA_WIDTH          = 128,
    parameter   BUS_SIZE_WIDTH          = 3,
    parameter   BUS_STRB_WIDTH          = BUS_DATA_WIDTH/8,
    parameter type          BUS_CH_AX_TYPE          = iommu_acd_pkg::ch_ax_t,
    parameter type          BUS_CH_W_TYPE           = iommu_acd_pkg::ch_w_t,
    parameter type          BUS_CH_B_TYPE           = iommu_acd_pkg::ch_b_t,
    parameter   SPARE_PARAM             = 0
)(
//{{{IO
    input  logic                                    clk             ,
    input  logic                                    rstn            ,
    // TRANS_QUEUE
    output logic                                    mrif_done_o     ,
    input  logic                                    mrif_valid_i    ,
    input  logic [55:12]                            mrif_nppn_i     ,
    input  logic [10:0]                             mrif_nid_i      ,
    input  logic [BUS_ID_WIDTH-1:0]                 mrif_axid_i     ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        mrif_token_i    ,
    // R_BUF
    input  logic                                    r2m_arvalid_i   ,
    output logic                                    r2m_arready_o   ,
    input  BUS_CH_AX_TYPE                           r2m_arpayld_i   ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        r2m_token_i     ,
    input  logic                                    r2m_fault_i     ,
    input  logic                                    r2m_mrif_i      ,
    // MST_IF
    output logic                                    r2m_arvalid_o   ,
    input  logic                                    r2m_arready_i   ,
    output BUS_CH_AX_TYPE                           r2m_arpayld_o   ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        r2m_token_o     ,
    output logic                                    r2m_fault_o     ,
    output logic                                    r2m_mrif_o      ,
    // W_BUF
    input  logic                                    w2m_awvalid_i   ,
    output logic                                    w2m_awready_o   ,
    input  BUS_CH_AX_TYPE                           w2m_awpayld_i   ,
    input  logic                                    w2m_wvalid_i    ,
    output logic                                    w2m_wready_o    ,
    input  BUS_CH_W_TYPE                            w2m_wpayld_i    ,
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        w2m_token_i     ,
    input  logic                                    w2m_fault_i     ,
    input  logic                                    w2m_mrif_i      ,
    // MST_IF
    output logic                                    w2m_awvalid_o   ,
    input  logic                                    w2m_awready_i   ,
    output BUS_CH_AX_TYPE                           w2m_awpayld_o   ,
    output logic                                    w2m_wvalid_o    ,
    input  logic                                    w2m_wready_i    ,
    output BUS_CH_W_TYPE                            w2m_wpayld_o    ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        w2m_token_o     ,
    output logic                                    w2m_fault_o     ,
    output logic                                    w2m_mrif_o      ,
    input  logic                                    m2b_bvalid_i    ,
    output logic                                    m2b_bready_o    ,
    input  BUS_CH_B_TYPE                            m2b_bpayld_i    ,
    
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        m2b_token_i     ,
    input  logic                                    m2b_mrif_i      ,
    // B_BUF
    output logic                                    m2b_bvalid_o    ,
    input  logic                                    m2b_bready_i    ,
    output BUS_CH_B_TYPE                            m2b_bpayld_o    ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        m2b_token_o     ,
    output logic                                    m2b_mrif_o      ,
    //
    input  logic                                    spare_in        
//}}}
);
//=== Declare === {{{
    logic                                   mrif_ar_address_invalid, mrif_ar_container_size_invalid;
    logic [BUS_SIZE_WIDTH+8+1-1:0]          mrif_ar_container_size;
    logic [BUS_SIZE_WIDTH+8+1-1:0]          mrif_ar_burst_length;

    logic                                   mrif_aw_address_invalid, mrif_aw_container_size_invalid;
    logic [BUS_SIZE_WIDTH+8+1-1:0]          mrif_aw_container_size;
    logic [BUS_SIZE_WIDTH+8+1-1:0]          mrif_aw_burst_length;

    logic                                   bypp_w2m_awvalid_i;
    logic                                   bypp_w2m_awready_o;
    BUS_CH_AX_TYPE                          bypp_w2m_awpayld_i;
    logic                                   bypp_w2m_wvalid_i ;
    logic                                   bypp_w2m_wready_o ;
    BUS_CH_W_TYPE                           bypp_w2m_wpayld_i ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       bypp_w2m_token_i  ;
    logic                                   bypp_w2m_fault_i  ;
    logic                                   bypp_w2m_mrif_i   ;

    logic                                   bypp_w2m_awvalid_o;
    logic                                   bypp_w2m_awready_i;
    BUS_CH_AX_TYPE                          bypp_w2m_awpayld_o;
    logic                                   bypp_w2m_wvalid_o ;
    logic                                   bypp_w2m_wready_i ;
    BUS_CH_W_TYPE                           bypp_w2m_wpayld_o ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       bypp_w2m_token_o  ;
    logic                                   bypp_w2m_fault_o  ;
    logic                                   bypp_w2m_mrif_o   ;

    logic                                   mrif_w2m_awvalid_i;
    logic                                   mrif_w2m_awready_o;
    BUS_CH_AX_TYPE                          mrif_w2m_awpayld_i;
    logic                                   mrif_w2m_wvalid_i ;
    logic                                   mrif_w2m_wready_o ;
    BUS_CH_W_TYPE                           mrif_w2m_wpayld_i ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       mrif_w2m_token_i  ;
    logic                                   mrif_w2m_fault_i  ;
    logic                                   mrif_w2m_mrif_i   ;

    logic                                   mrif_w2m_awvalid_o;
    logic                                   mrif_w2m_awready_i;
    BUS_CH_AX_TYPE                          mrif_w2m_awpayld_o;
    logic                                   mrif_w2m_wvalid_o ;
    logic                                   mrif_w2m_wready_i ;
    BUS_CH_W_TYPE                           mrif_w2m_wpayld_o ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       mrif_w2m_token_o  ;
    logic                                   mrif_w2m_fault_o  ;
    logic                                   mrif_w2m_mrif_o   ;

    logic                                   mrif_awvalid;
    BUS_CH_AX_TYPE                          mrif_awpayld;
    logic                                   mrif_wvalid;
    BUS_CH_W_TYPE                           mrif_wpayld;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       mrif_token;

    logic [31:0]                            mrif_interrupt_id;
    logic                                   mrif_interrupt_id_invalid;
    logic [4:0]                             mrif_interrupt_id_row_idx;
    logic [5:0]                             mrif_interrupt_id_col_idx;
    logic [31:0]                            mrif_interrupt_id_bits;
    logic [BUS_DATA_WIDTH-1:0]              mrif_wdata_final;
    logic [BUS_STRB_WIDTH-1:0]              mrif_wstrb_final;

    logic                                   bypp_m2b_bvalid_i;
    logic                                   bypp_m2b_bready_o;
    BUS_CH_B_TYPE                           bypp_m2b_bpayld_i;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       bypp_m2b_token_i ;
    logic                                   bypp_m2b_mrif_i  ;

    logic                                   mrif_m2b_bvalid_i;
    logic                                   mrif_m2b_bready_o;
    BUS_CH_B_TYPE                           mrif_m2b_bpayld_i;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       mrif_m2b_token_i ;
    logic                                   mrif_m2b_mrif_i  ;

    logic                                   mrif_m2b_bvalid_o;
    logic                                   mrif_m2b_bready_i;
    BUS_CH_B_TYPE                           mrif_m2b_bpayld_o;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       mrif_m2b_token_o ;
    logic                                   mrif_m2b_mrif_o  ;

    logic [53:10]                           mrif_nppn;
    logic [10:0]                            mrif_nid;
    logic [BUS_ID_WIDTH-1:0]                mrif_axid;
    //logic [BUS_INFLY_TOKEN_WIDTH-1:0]       mrif_token;

    logic                                   nmsi_w2m_awvalid_o;
    logic                                   nmsi_w2m_awready_i;
    BUS_CH_AX_TYPE                          nmsi_w2m_awpayld_o;
    logic                                   nmsi_w2m_wvalid_o ;
    logic                                   nmsi_w2m_wready_i ;
    BUS_CH_W_TYPE                           nmsi_w2m_wpayld_o ;
    logic [BUS_INFLY_TOKEN_WIDTH-1:0]       nmsi_w2m_token_o  ;
    logic                                   nmsi_w2m_fault_o  ;
    logic                                   nmsi_w2m_mrif_o   ;

    logic [1:0]                             w2m_mux_sel, w2m_mux_sel_int;
    logic [1:0]                             w2m_mux_start_arb;
    logic                                   w2m_mux_mrif_ongoing, w2m_mux_bypp_ongoing, w2m_mux_nmsi_ongoing, w2m_mux_any_ongoing;

//}}}

//=== Main Code === {{{
//R MRIF {{{
    assign mrif_ar_address_invalid          = (r2m_arpayld_i.axaddr[1:0]!=2'b00);
    assign mrif_ar_burst_length             = {(BUS_SIZE_WIDTH+1)'(0), r2m_arpayld_i.axlen} + 1'b1;
    assign mrif_ar_container_size           = mrif_ar_burst_length << r2m_arpayld_i.axsize;
    assign mrif_ar_container_size_invalid   = mrif_ar_container_size != (BUS_SIZE_WIDTH+8+1)'(4);

    assign r2m_arvalid_o= r2m_arvalid_i;
    assign r2m_arready_o= r2m_arready_i;
    assign r2m_arpayld_o= r2m_arpayld_i;
    assign r2m_token_o  = r2m_token_i;
    assign r2m_fault_o  = r2m_fault_i | (r2m_mrif_i & (mrif_ar_address_invalid | mrif_ar_container_size_invalid));
    assign r2m_mrif_o   = r2m_fault_o ? 1'b0 : r2m_mrif_i;
//}}}

//W MRIF {{{
//                __    __    __    __    __    __    __    __    __    __    __    __    __    __
// clk          _|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |__|  |_
//                                                                                                  
//                ___________                               _____
// awvalid      _|           |_____________________________|     |__________________________________
//                                                                                                  
//                      _____                               ________________________________________
// awready      _______|     |_____________________________|
//                                                                                                  
//              _ _________________________________________ ___________________________________
// awaddr       _X_________________________________________X___________________________________/////
//                                                                                                  
//                            _____________________________       _____________________________
// wvalid(FF)   _____________|                             |_____|                             |____
//                      ___________________________________ ___________________________________
//       (RAM)  _______|                                   |                                   |____
//              _____________ _____ _____ _____ _____ ___________ _____ _____ _____ _____ _____
// wdata        _____________X_____X_____X_____X_____X___________X_____X_____X_____X_____X_____/////
//                                                                                                  
//                                                    _____                               _____
// wlast        _____________________________________|     |_____________________________|     |____
//
// AW & W from W_BUF always following waveform above

// AW&W DEMUX {{{

    assign bypp_w2m_awvalid_i   = (w2m_fault_i | ~w2m_mrif_i) ? w2m_awvalid_i : 'd0;
    assign bypp_w2m_awready_o   = bypp_w2m_awready_i;
    assign bypp_w2m_awpayld_i   = (w2m_fault_i | ~w2m_mrif_i) ? w2m_awpayld_i : 'd0;
    assign bypp_w2m_wvalid_i    = (w2m_fault_i | ~w2m_mrif_i) ? w2m_wvalid_i  : 'd0;
    assign bypp_w2m_wready_o    = bypp_w2m_wready_i;
    assign bypp_w2m_wpayld_i    = (w2m_fault_i | ~w2m_mrif_i) ? w2m_wpayld_i  : 'd0;
    assign bypp_w2m_token_i     = (w2m_fault_i | ~w2m_mrif_i) ? w2m_token_i   : 'd0;
    assign bypp_w2m_fault_i     = (w2m_fault_i | ~w2m_mrif_i) ? w2m_fault_i   : 'd0;
    assign bypp_w2m_mrif_i      = 1'b0;

    assign bypp_w2m_awvalid_o   = bypp_w2m_awvalid_i;
    assign bypp_w2m_awpayld_o   = bypp_w2m_awpayld_i;
    assign bypp_w2m_wvalid_o    = bypp_w2m_wvalid_i ;
    assign bypp_w2m_wpayld_o    = bypp_w2m_wpayld_i ;
    assign bypp_w2m_token_o     = bypp_w2m_token_i  ;
    assign bypp_w2m_fault_o     = bypp_w2m_fault_i  ;
    assign bypp_w2m_mrif_o      = bypp_w2m_mrif_i   ;

    assign mrif_w2m_awvalid_i   = (~w2m_fault_i & w2m_mrif_i) ? w2m_awvalid_i : 'd0;
    assign mrif_w2m_awready_o   = 1'b1;
    assign mrif_w2m_awpayld_i   = (~w2m_fault_i & w2m_mrif_i) ? w2m_awpayld_i : 'd0;
    assign mrif_w2m_wvalid_i    = (~w2m_fault_i & w2m_mrif_i) ? w2m_wvalid_i  : 'd0;
    assign mrif_w2m_wready_o    = 1'b1;
    assign mrif_w2m_wpayld_i    = (~w2m_fault_i & w2m_mrif_i) ? w2m_wpayld_i  : 'd0;
    assign mrif_w2m_token_i     = (~w2m_fault_i & w2m_mrif_i) ? w2m_token_i   : 'd0;
    assign mrif_w2m_fault_i     = 1'b0;
    assign mrif_w2m_mrif_i      = 1'b1;

    assign w2m_awready_o        = (w2m_fault_i | ~w2m_mrif_i) ? bypp_w2m_awready_o : mrif_w2m_awready_o;
    assign w2m_wready_o         = (w2m_fault_i | ~w2m_mrif_i) ? bypp_w2m_wready_o  : mrif_w2m_wready_o;
//}}}

// MRIF {{{
    // store the AW&W info, only 1 outstanding bynow, use FIFO if more unstranding supported
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            mrif_awvalid<= 'd0;
            mrif_awpayld<= 'd0;
            mrif_wvalid <= 'd0;
            mrif_wpayld <= 'd0;
            mrif_token  <= 'd0;
        end
        else begin
            mrif_awvalid <= mrif_w2m_awvalid_i;
            if(mrif_w2m_awvalid_i) begin
                mrif_awpayld<= mrif_w2m_awpayld_i;
                mrif_token  <= mrif_w2m_token_i;
            end

            if(mrif_wvalid & mrif_wpayld.wlast) begin               // MRIF W2M will go
                mrif_wvalid <= 1'b0;
            end
            else if(mrif_w2m_awvalid_i & mrif_w2m_wvalid_i) begin   // FF type WBUF, get the first transfer
                mrif_wvalid <= 1'b1;
                mrif_wpayld <= mrif_w2m_wpayld_i;
            end
            else if(mrif_awvalid & mrif_w2m_wvalid_i) begin         // RAM type WBUF, get the first transfer
                mrif_wvalid <= 1'b1;
                mrif_wpayld <= mrif_w2m_wpayld_i;
            end
            else if(mrif_w2m_wvalid_i) begin
                mrif_wpayld.wlast <= mrif_w2m_wpayld_i.wlast;
            end
        end
    end

    assign mrif_aw_address_invalid          = (mrif_awpayld.axaddr[1:0]!=2'b00);
    assign mrif_aw_burst_length             = {(BUS_SIZE_WIDTH+1)'(0), mrif_awpayld.axlen} + 1'b1;
    assign mrif_aw_container_size           = mrif_aw_burst_length << mrif_awpayld.axsize;
    // bynow, do not support narrow_transfer or unalign_transfer in MRIF_transaction, and the WDATA should be at least 32bit width
    // means all WDATA in sigle one transfer in MRIF_transaction
    assign mrif_aw_container_size_invalid   = (mrif_aw_container_size != (BUS_SIZE_WIDTH+8+1)'(4)) | mrif_awpayld.axlen!='d0 | mrif_awpayld.axsize!='d2;

generate // mrif_interrupt_id {{{
    if(BUS_DATA_WIDTH==32) begin: wdata32_intid_gen
        assign mrif_interrupt_id = mrif_wpayld.wdata;
    end
    else if(BUS_DATA_WIDTH==64) begin: wdata64_intid_gen
        always@(*) begin
            case(mrif_awpayld.axaddr[2])
                1'b0    : mrif_interrupt_id = mrif_wpayld.wdata[32*1-1:32*0];
                1'b1    : mrif_interrupt_id = mrif_wpayld.wdata[32*2-1:32*1];
                default : mrif_interrupt_id = mrif_wpayld.wdata[31*1-1:32*0];
            endcase
        end
    end
    else if(BUS_DATA_WIDTH==128) begin: wdata128_intid_gen
        always@(*) begin
            case(mrif_awpayld.axaddr[3:2])
                2'b00   : mrif_interrupt_id = mrif_wpayld.wdata[32*1-1:32*0];
                2'b01   : mrif_interrupt_id = mrif_wpayld.wdata[32*2-1:32*1];
                2'b10   : mrif_interrupt_id = mrif_wpayld.wdata[32*3-1:32*2];
                2'b11   : mrif_interrupt_id = mrif_wpayld.wdata[32*4-1:32*3];
                default : mrif_interrupt_id = mrif_wpayld.wdata[32*1-1:32*0];
            endcase
        end
    end
    else if(BUS_DATA_WIDTH==256) begin: wdata256_intid_gen
        always@(*) begin
            case(mrif_awpayld.axaddr[4:2])
                3'b000  : mrif_interrupt_id = mrif_wpayld.wdata[32*1-1:32*0];
                3'b001  : mrif_interrupt_id = mrif_wpayld.wdata[32*2-1:32*1];
                3'b010  : mrif_interrupt_id = mrif_wpayld.wdata[32*3-1:32*2];
                3'b011  : mrif_interrupt_id = mrif_wpayld.wdata[32*4-1:32*3];
                3'b100  : mrif_interrupt_id = mrif_wpayld.wdata[32*5-1:32*4];
                3'b101  : mrif_interrupt_id = mrif_wpayld.wdata[32*6-1:32*5];
                3'b110  : mrif_interrupt_id = mrif_wpayld.wdata[32*7-1:32*6];
                3'b111  : mrif_interrupt_id = mrif_wpayld.wdata[32*8-1:32*7];
                default : mrif_interrupt_id = mrif_wpayld.wdata[32*1-1:32*0];
            endcase
        end
    end
    else if(BUS_DATA_WIDTH==512) begin: wdata512_intid_gen
        always@(*) begin
            case(mrif_awpayld.axaddr[5:2])
                4'b0000 : mrif_interrupt_id = mrif_wpayld.wdata[32*1 -1:32*0 ];
                4'b0001 : mrif_interrupt_id = mrif_wpayld.wdata[32*2 -1:32*1 ];
                4'b0010 : mrif_interrupt_id = mrif_wpayld.wdata[32*3 -1:32*2 ];
                4'b0011 : mrif_interrupt_id = mrif_wpayld.wdata[32*4 -1:32*3 ];
                4'b0100 : mrif_interrupt_id = mrif_wpayld.wdata[32*5 -1:32*4 ];
                4'b0101 : mrif_interrupt_id = mrif_wpayld.wdata[32*6 -1:32*5 ];
                4'b0110 : mrif_interrupt_id = mrif_wpayld.wdata[32*7 -1:32*6 ];
                4'b0111 : mrif_interrupt_id = mrif_wpayld.wdata[32*8 -1:32*7 ];
                4'b1000 : mrif_interrupt_id = mrif_wpayld.wdata[32*9 -1:32*8 ];
                4'b1001 : mrif_interrupt_id = mrif_wpayld.wdata[32*10-1:32*9 ];
                4'b1010 : mrif_interrupt_id = mrif_wpayld.wdata[32*11-1:32*10];
                4'b1011 : mrif_interrupt_id = mrif_wpayld.wdata[32*12-1:32*11];
                4'b1100 : mrif_interrupt_id = mrif_wpayld.wdata[32*13-1:32*12];
                4'b1101 : mrif_interrupt_id = mrif_wpayld.wdata[32*14-1:32*13];
                4'b1110 : mrif_interrupt_id = mrif_wpayld.wdata[32*15-1:32*14];
                4'b1111 : mrif_interrupt_id = mrif_wpayld.wdata[32*16-1:32*15];
                default : mrif_interrupt_id = mrif_wpayld.wdata[32*1 -1:32*0 ];
            endcase
        end
    end
    else if(BUS_DATA_WIDTH==1024) begin: wdata1024_intid_gen
        always@(*) begin
            case(mrif_awpayld.axaddr[6:2])
                5'b00000: mrif_interrupt_id = mrif_wpayld.wdata[32*1 -1:32*0 ];
                5'b00001: mrif_interrupt_id = mrif_wpayld.wdata[32*2 -1:32*1 ];
                5'b00010: mrif_interrupt_id = mrif_wpayld.wdata[32*3 -1:32*2 ];
                5'b00011: mrif_interrupt_id = mrif_wpayld.wdata[32*4 -1:32*3 ];
                5'b00100: mrif_interrupt_id = mrif_wpayld.wdata[32*5 -1:32*4 ];
                5'b00101: mrif_interrupt_id = mrif_wpayld.wdata[32*6 -1:32*5 ];
                5'b00110: mrif_interrupt_id = mrif_wpayld.wdata[32*7 -1:32*6 ];
                5'b00111: mrif_interrupt_id = mrif_wpayld.wdata[32*8 -1:32*7 ];
                5'b01000: mrif_interrupt_id = mrif_wpayld.wdata[32*9 -1:32*8 ];
                5'b01001: mrif_interrupt_id = mrif_wpayld.wdata[32*10-1:32*9 ];
                5'b01010: mrif_interrupt_id = mrif_wpayld.wdata[32*11-1:32*10];
                5'b01011: mrif_interrupt_id = mrif_wpayld.wdata[32*12-1:32*11];
                5'b01100: mrif_interrupt_id = mrif_wpayld.wdata[32*13-1:32*12];
                5'b01101: mrif_interrupt_id = mrif_wpayld.wdata[32*14-1:32*13];
                5'b01110: mrif_interrupt_id = mrif_wpayld.wdata[32*15-1:32*14];
                5'b01111: mrif_interrupt_id = mrif_wpayld.wdata[32*16-1:32*15];
                5'b10000: mrif_interrupt_id = mrif_wpayld.wdata[32*17-1:32*16];
                5'b10001: mrif_interrupt_id = mrif_wpayld.wdata[32*18-1:32*17];
                5'b10010: mrif_interrupt_id = mrif_wpayld.wdata[32*19-1:32*18];
                5'b10011: mrif_interrupt_id = mrif_wpayld.wdata[32*20-1:32*19];
                5'b10100: mrif_interrupt_id = mrif_wpayld.wdata[32*21-1:32*20];
                5'b10101: mrif_interrupt_id = mrif_wpayld.wdata[32*22-1:32*21];
                5'b10110: mrif_interrupt_id = mrif_wpayld.wdata[32*23-1:32*22];
                5'b10111: mrif_interrupt_id = mrif_wpayld.wdata[32*24-1:32*23];
                5'b11000: mrif_interrupt_id = mrif_wpayld.wdata[32*25-1:32*24];
                5'b11001: mrif_interrupt_id = mrif_wpayld.wdata[32*26-1:32*25];
                5'b11010: mrif_interrupt_id = mrif_wpayld.wdata[32*27-1:32*26];
                5'b11011: mrif_interrupt_id = mrif_wpayld.wdata[32*28-1:32*27];
                5'b11100: mrif_interrupt_id = mrif_wpayld.wdata[32*29-1:32*28];
                5'b11101: mrif_interrupt_id = mrif_wpayld.wdata[32*30-1:32*29];
                5'b11110: mrif_interrupt_id = mrif_wpayld.wdata[32*31-1:32*30];
                5'b11111: mrif_interrupt_id = mrif_wpayld.wdata[32*32-1:32*31];
                default : mrif_interrupt_id = mrif_wpayld.wdata[32*1 -1:32*0 ];
            endcase
        end
    end
    else begin: illegal_intid_gen
        assign mrif_interrupt_id = mrif_wpayld.wdata;
    end
endgenerate//}}}

    assign mrif_interrupt_id_invalid= (mrif_interrupt_id[31:11]!='d0);
    assign mrif_interrupt_id_row_idx= mrif_interrupt_id[10:6];
    assign mrif_interrupt_id_col_idx= mrif_interrupt_id[5:0];
    assign mrif_interrupt_id_bits   = 32'b1 << mrif_interrupt_id_col_idx[5:0];

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            mrif_w2m_awvalid_o  <= 1'b0;
            mrif_w2m_wvalid_o   <= 1'b0;
        end
        else begin
            if(mrif_w2m_awvalid_o & ~mrif_w2m_awready_i) begin
                mrif_w2m_awvalid_o <= 1'b1;
            end
            else if(mrif_wvalid & mrif_wpayld.wlast) begin
                mrif_w2m_awvalid_o  <= 1'b1;
            end
            else begin
                mrif_w2m_awvalid_o  <= 1'b0;
            end

            if(mrif_w2m_wvalid_o & ~mrif_w2m_wready_i) begin
                mrif_w2m_wvalid_o   <= 1'b1;
            end
            else if(mrif_w2m_awvalid_o & mrif_w2m_awready_i) begin
                mrif_w2m_wvalid_o   <= 1'b1;
            end
            else begin
                mrif_w2m_wvalid_o   <= 1'b0;
            end
        end
    end

generate // mrif_wdata/wstrb{{{
    if(BUS_ADDR_WIDTH==32) begin: wdata32_mrifout_gen
        assign mrif_wdata_final = mrif_interrupt_id_bits;
        assign mrif_wstrb_final = 4'b1111;
    end
    else if(BUS_ADDR_WIDTH==64|BUS_ADDR_WIDTH==128) begin: wdata64_128_mrifout_gen
        always@(*) begin
            case(mrif_interrupt_id_col_idx[5])
            1'b0    : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0); end
            1'b1    : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1); end
            default : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0); end
            endcase
        end
    end
    else if(BUS_ADDR_WIDTH==256) begin: wdata256_mrifout_gen
        always@(*) begin
            case({mrif_interrupt_id_row_idx[0],mrif_interrupt_id_col_idx[5]})
            2'b00   : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*0); end
            2'b01   : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*0); end
            2'b10   : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*1); end
            2'b11   : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*1); end
            default : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*0); end
            endcase
        end
    end
    else if(BUS_ADDR_WIDTH==512) begin: wdata512_mrifout_gen
        always@(*) begin
            case({mrif_interrupt_id_row_idx[1:0],mrif_interrupt_id_col_idx[5]})
            3'b000  : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*0+256*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*0+32*0); end
            3'b001  : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*0+256*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*0+32*0); end
            3'b010  : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*1+256*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*1+32*0); end
            3'b011  : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*1+256*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*1+32*0); end
            3'b100  : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*0+256*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*0+32*1); end
            3'b101  : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*0+256*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*0+32*1); end
            3'b110  : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*1+256*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*1+32*1); end
            3'b111  : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*1+256*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*1+32*1); end
            default : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*0+256*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*0+32*0); end
            endcase
        end
    end
    else if(BUS_ADDR_WIDTH==1024) begin: wdata1024_mrifout_gen
        always@(*) begin
            case({mrif_interrupt_id_row_idx[2:0],mrif_interrupt_id_col_idx[5]})
            4'b0000 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*0+256*0+512*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*0+32*0+64*0); end
            4'b0001 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*0+256*0+512*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*0+32*0+64*0); end
            4'b0010 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*1+256*0+512*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*1+32*0+64*0); end
            4'b0011 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*1+256*0+512*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*1+32*0+64*0); end
            4'b0100 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*0+256*1+512*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*0+32*1+64*0); end
            4'b0101 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*0+256*1+512*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*0+32*1+64*0); end
            4'b0110 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*1+256*1+512*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*1+32*1+64*0); end
            4'b0111 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*1+256*1+512*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*1+32*1+64*0); end
            4'b1000 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*0+256*0+512*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*0+32*0+64*1); end
            4'b1001 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*0+256*0+512*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*0+32*0+64*1); end
            4'b1010 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*1+256*0+512*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*1+32*0+64*1); end
            4'b1011 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*1+256*0+512*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*1+32*0+64*1); end
            4'b1100 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*0+256*1+512*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*0+32*1+64*1); end
            4'b1101 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*0+256*1+512*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*0+32*1+64*1); end
            4'b1110 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*1+256*1+512*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*1+32*1+64*1); end
            4'b1111 : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*1+128*1+256*1+512*1); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*1+16*1+32*1+64*1); end
            default : begin mrif_wdata_final = {{(BUS_DATA_WIDTH-32){1'b0}},mrif_interrupt_id_bits} << (32*0+128*0+256*0+512*0); mrif_wstrb_final = {{(BUS_STRB_WIDTH-4){1'b0}},4'b1111} << (4*0+16*0+32*0+64*0); end
            endcase
        end
    end
    else begin: illegal_mrifout_gen
        assign mrif_wdata_final = 'd0;
        assign mrif_wstrb_final = 'd0;
    end
endgenerate //}}}

    assign mrif_w2m_awpayld_o.axid      = mrif_awpayld.axid    ;
    assign mrif_w2m_awpayld_o.axaddr    = {mrif_awpayld.axaddr[BUS_ADDR_WIDTH-1:9], mrif_interrupt_id_row_idx, mrif_interrupt_id_col_idx[5], 3'b000};//mrif_awpayld.axaddr  ;
    assign mrif_w2m_awpayld_o.axlen     = 'd0;//mrif_awpayld.axlen   ;
    assign mrif_w2m_awpayld_o.axsize    = 'd2;//mrif_awpayld.axsize  ;
    assign mrif_w2m_awpayld_o.axburst   = 'd1;//mrif_awpayld.axburst ;
    assign mrif_w2m_awpayld_o.axlock    = 'd0;//mrif_awpayld.axlock  ;
    assign mrif_w2m_awpayld_o.axcache   = mrif_awpayld.axcache ;
    assign mrif_w2m_awpayld_o.axprot    = mrif_awpayld.axprot  ;
    assign mrif_w2m_awpayld_o.axregion  = mrif_awpayld.axregion;
    assign mrif_w2m_awpayld_o.axuser    = mrif_awpayld.axuser  ;
    assign mrif_w2m_awpayld_o.axqos     = mrif_awpayld.axqos   ;
    assign mrif_w2m_awpayld_o.axsnoop   = mrif_awpayld.axsnoop ;
    assign mrif_w2m_awpayld_o.axdomain  = mrif_awpayld.axdomain;
    assign mrif_w2m_awpayld_o.axbar     = 'd0;//mrif_awpayld.axbar   ;
    assign mrif_w2m_awpayld_o.axidunq   = mrif_awpayld.axidunq ;
    assign mrif_w2m_awpayld_o.axatop    = 6'b010011;//mrif_awpayld.axatop  ;    // ATOMIC_STORE_Litter_END_Bit_SET
    assign mrif_w2m_awpayld_o.axloop    = mrif_awpayld.axloop  ;

    assign mrif_w2m_wpayld_o.wdata      = mrif_wdata_final;//mrif_wpayld.wdata;
    assign mrif_w2m_wpayld_o.wstrb      = mrif_wstrb_final;//mrif_wpayld.wstrb;
    assign mrif_w2m_wpayld_o.wlast      = 1'b1;//mrif_wpayld.wlast;
    assign mrif_w2m_wpayld_o.wuser      = mrif_wpayld.wuser;

    assign mrif_w2m_token_o             = mrif_token;
    assign mrif_w2m_fault_o             = mrif_aw_container_size_invalid | mrif_interrupt_id_invalid;
    assign mrif_w2m_mrif_o              = mrif_w2m_fault_o ? 1'b0 : 1'b1;
//}}}

// B DEMUX {{{
    assign bypp_m2b_bvalid_i    = (~m2b_mrif_i | m2b_bpayld_i.bresp!='d0) ? m2b_bvalid_i : 'd0;
    assign bypp_m2b_bready_o    = m2b_bready_i;
    assign bypp_m2b_bpayld_i    = (~m2b_mrif_i | m2b_bpayld_i.bresp!='d0) ? m2b_bpayld_i : 'd0;
    assign bypp_m2b_token_i     = (~m2b_mrif_i | m2b_bpayld_i.bresp!='d0) ? m2b_token_i  : 'd0;
    assign bypp_m2b_mrif_i      = 1'b0;

    assign m2b_bvalid_o         = bypp_m2b_bvalid_i;
    assign m2b_bpayld_o         = bypp_m2b_bpayld_i;
    assign m2b_token_o          = bypp_m2b_token_i;
    assign m2b_mrif_o           = 1'b0;

    assign m2b_bready_o         = (m2b_mrif_i  & m2b_bpayld_i.bresp=='d0) ? mrif_m2b_bready_o : bypp_m2b_bready_o;

    assign mrif_m2b_bvalid_i    = (m2b_mrif_i  & m2b_bpayld_i.bresp=='d0) ? m2b_bvalid_i : 'd0;
    assign mrif_m2b_bready_o    = 1'b1;
    assign mrif_m2b_bpayld_i    = (m2b_mrif_i  & m2b_bpayld_i.bresp=='d0) ? m2b_bpayld_i : 'd0;
    assign mrif_m2b_token_i     = (m2b_mrif_i  & m2b_bpayld_i.bresp=='d0) ? m2b_token_i  : 'd0;
    assign mrif_m2b_mrif_i      = 1'b1;
//}}}

// NMSI {{{
    // store the NMSI info, only 1 outstanding bynow, use FIFO if more unstranding supporte
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            mrif_nppn   <= 'd0;
            mrif_nid    <= 'd0;
            mrif_axid   <= 'd0;
        end
        else begin
            if(mrif_valid_i) begin
                mrif_nppn <= mrif_nppn_i;
                mrif_nid  <= mrif_nid_i;
                mrif_axid <= mrif_axid_i;
            end
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            nmsi_w2m_awvalid_o  <= 1'b0;
            nmsi_w2m_wvalid_o   <= 1'b0;
            nmsi_w2m_token_o    <= 'd0;
        end
        else begin
            if(nmsi_w2m_awvalid_o & ~nmsi_w2m_awready_i) begin
                nmsi_w2m_awvalid_o  <= 1'b1;
            end
            else if(mrif_m2b_bvalid_i) begin
                nmsi_w2m_awvalid_o  <= 1'b1;
            end
            else begin
                nmsi_w2m_awvalid_o  <= 1'b0;
            end

            if(nmsi_w2m_wvalid_o & ~nmsi_w2m_wready_i) begin
                nmsi_w2m_wvalid_o   <= 1'b1;
            end
            else if(nmsi_w2m_awvalid_o & nmsi_w2m_awready_i) begin
                nmsi_w2m_wvalid_o   <= 1'b1;
            end
            else begin
                nmsi_w2m_wvalid_o   <= 1'b0;
            end

            if(mrif_m2b_bvalid_i) begin
                nmsi_w2m_token_o    <= mrif_m2b_token_i;
            end
        end
    end

    assign nmsi_w2m_awpayld_o.axid      = mrif_axid;
    assign nmsi_w2m_awpayld_o.axaddr    = {8'd0, mrif_nppn, 12'b0};
    assign nmsi_w2m_awpayld_o.axlen     = 'd0;
    assign nmsi_w2m_awpayld_o.axsize    = 'd2;
    assign nmsi_w2m_awpayld_o.axburst   = 'd1;
    assign nmsi_w2m_awpayld_o.axlock    = 'd0;
    assign nmsi_w2m_awpayld_o.axcache   = mrif_awpayld.axcache;
    assign nmsi_w2m_awpayld_o.axprot    = mrif_awpayld.axprot;
    assign nmsi_w2m_awpayld_o.axregion  = mrif_awpayld.axregion;
    assign nmsi_w2m_awpayld_o.axuser    = mrif_awpayld.axuser;
    assign nmsi_w2m_awpayld_o.axqos     = mrif_awpayld.axqos;
    assign nmsi_w2m_awpayld_o.axsnoop   = mrif_awpayld.axsnoop;
    assign nmsi_w2m_awpayld_o.axdomain  = mrif_awpayld.axdomain;
    assign nmsi_w2m_awpayld_o.axbar     = 'd0;
    assign nmsi_w2m_awpayld_o.axidunq   = mrif_awpayld.axidunq;
    assign nmsi_w2m_awpayld_o.axatop    = 'd0;
    assign nmsi_w2m_awpayld_o.axloop    = mrif_awpayld.axloop;

    assign nmsi_w2m_wpayld_o.wdata  = {(BUS_DATA_WIDTH-11)'(0), mrif_nid};
    assign nmsi_w2m_wpayld_o.wstrb  = 'hf;
    assign nmsi_w2m_wpayld_o.wlast  = 1'b1;
    assign nmsi_w2m_wpayld_o.wuser  = mrif_wpayld.wuser;

    assign nmsi_w2m_fault_o = 1'b0;
    assign nmsi_w2m_mrif_o  = 1'b0;
//}}}

// AW&W 3TO1 MUX {{{
    assign bypp_w2m_awready_i   = (w2m_mux_sel == 2'b01) ? w2m_awready_i : 1'b0;
    assign bypp_w2m_wready_i    = (w2m_mux_sel == 2'b01) ? w2m_wready_i  : 1'b0;
    assign mrif_w2m_awready_i   = (w2m_mux_sel == 2'b10) ? w2m_awready_i : 1'b0;
    assign mrif_w2m_wready_i    = (w2m_mux_sel == 2'b10) ? w2m_wready_i  : 1'b0;
    assign nmsi_w2m_awready_i   = (w2m_mux_sel == 2'b11) ? w2m_awready_i : 1'b0;
    assign nmsi_w2m_wready_i    = (w2m_mux_sel == 2'b11) ? w2m_wready_i  : 1'b0;

    assign w2m_awvalid_o        = (w2m_mux_sel == 2'b01) ? bypp_w2m_awvalid_o : (w2m_mux_sel == 2'b10) ? mrif_w2m_awvalid_o : (w2m_mux_sel == 2'b11) ? nmsi_w2m_awvalid_o: 'd0;
    assign w2m_awpayld_o        = (w2m_mux_sel == 2'b01) ? bypp_w2m_awpayld_o : (w2m_mux_sel == 2'b10) ? mrif_w2m_awpayld_o : (w2m_mux_sel == 2'b11) ? nmsi_w2m_awpayld_o: 'd0;
    assign w2m_wvalid_o         = (w2m_mux_sel == 2'b01) ? bypp_w2m_wvalid_o  : (w2m_mux_sel == 2'b10) ? mrif_w2m_wvalid_o  : (w2m_mux_sel == 2'b11) ? nmsi_w2m_wvalid_o : 'd0;
    assign w2m_wpayld_o         = (w2m_mux_sel == 2'b01) ? bypp_w2m_wpayld_o  : (w2m_mux_sel == 2'b10) ? mrif_w2m_wpayld_o  : (w2m_mux_sel == 2'b11) ? nmsi_w2m_wpayld_o : 'd0;
    assign w2m_token_o          = (w2m_mux_sel == 2'b01) ? bypp_w2m_token_o   : (w2m_mux_sel == 2'b10) ? mrif_w2m_token_o   : (w2m_mux_sel == 2'b11) ? nmsi_w2m_token_o  : 'd0;
    assign w2m_fault_o          = (w2m_mux_sel == 2'b01) ? bypp_w2m_fault_o   : (w2m_mux_sel == 2'b10) ? mrif_w2m_fault_o   : (w2m_mux_sel == 2'b11) ? nmsi_w2m_fault_o  : 'd0;
    assign w2m_mrif_o           = (w2m_mux_sel == 2'b01) ? bypp_w2m_mrif_o    : (w2m_mux_sel == 2'b10) ? mrif_w2m_mrif_o    : (w2m_mux_sel == 2'b11) ? nmsi_w2m_mrif_o   : 'd0;

    always@(*) begin
        w2m_mux_start_arb = w2m_mux_sel_int;
        if(~w2m_mux_any_ongoing) begin
            case({bypp_w2m_awvalid_o, mrif_w2m_awvalid_o, nmsi_w2m_awvalid_o})
            3'b111  :begin w2m_mux_start_arb = 2'b11; end
            3'b110  :begin w2m_mux_start_arb = 2'b10; end
            3'b101  :begin w2m_mux_start_arb = 2'b11; end
            3'b100  :begin w2m_mux_start_arb = 2'b01; end
            3'b011  :begin w2m_mux_start_arb = 2'b11; end
            3'b010  :begin w2m_mux_start_arb = 2'b10; end
            3'b001  :begin w2m_mux_start_arb = 2'b11; end
            3'b000  :begin w2m_mux_start_arb = 2'b00; end
            default :begin w2m_mux_start_arb = w2m_mux_sel_int; end
            endcase
        end
        else begin
            w2m_mux_start_arb = w2m_mux_sel_int;
        end
    end

    assign w2m_mux_any_ongoing = (w2m_mux_mrif_ongoing | w2m_mux_nmsi_ongoing | w2m_mux_bypp_ongoing);

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            w2m_mux_sel_int <= 2'b11;
        else begin
            w2m_mux_sel_int <= w2m_mux_start_arb;
        end
    end

    assign w2m_mux_sel = w2m_mux_any_ongoing ? w2m_mux_sel_int : w2m_mux_start_arb;

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            w2m_mux_mrif_ongoing <= 1'b0;
            w2m_mux_nmsi_ongoing <= 1'b0;
            w2m_mux_bypp_ongoing <= 1'b0;
        end
        else begin
            if(w2m_wvalid_o & w2m_wready_i & w2m_wpayld_o.wlast)
                w2m_mux_bypp_ongoing <= 1'b0;
            else if(~w2m_mux_any_ongoing & bypp_w2m_awvalid_o & w2m_mux_start_arb==2'b01)
                w2m_mux_bypp_ongoing <= 1'b1;

            if(w2m_wvalid_o & w2m_wready_i & w2m_wpayld_o.wlast)
                w2m_mux_mrif_ongoing <= 1'b0;
            else if(~w2m_mux_any_ongoing & mrif_w2m_awvalid_o & w2m_mux_start_arb==2'b10)
                w2m_mux_mrif_ongoing <= 1'b1;

            if(w2m_wvalid_o & w2m_wready_i & w2m_wpayld_o.wlast)
                w2m_mux_nmsi_ongoing <= 1'b0;
            else if(~w2m_mux_any_ongoing & nmsi_w2m_awvalid_o & w2m_mux_start_arb==2'b11)
                w2m_mux_nmsi_ongoing <= 1'b1;
        end
    end
//}}}

// done_o {{{
    always@(posedge clk or negedge rstn) begin
        if(!rstn)
            mrif_done_o <= 1'b0;
        else begin
            if( (mrif_w2m_wvalid_o & mrif_w2m_wready_i & mrif_w2m_wpayld_o.wlast & mrif_w2m_fault_o)    |   // MRIF fault, no real MRIF write
                (m2b_bvalid_i & m2b_bready_o & m2b_mrif_i & m2b_bpayld_i.bresp!='d0)                    |   // MRFI write BRESP fail
                (nmsi_w2m_wvalid_o & nmsi_w2m_wready_i & nmsi_w2m_wpayld_o.wlast)                       |   // NMSI write accept
                1'b0)
                mrif_done_o <= 1'b1;
            else
                mrif_done_o <= 1'b0;
        end 
    end
//}}}

//}}}

//}}}

`ifdef IOMMU_ACD_SIMULATION_ASSERT
    assert property (@(negedge clk) $onehot({w2m_mux_bypp_ongoing, w2m_mux_mrif_ongoing, w2m_mux_nmsi_ongoing}))
    else $error("Violation: Only one w2m ongoing at a time");
`endif

endmodule//}}}
