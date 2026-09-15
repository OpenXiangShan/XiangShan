////////////////////////////////////////////////////////////////////////////////////////////
// iommu_acd_bus_handler_slv_err
//      1. generate B resp for fault AW
//      2. generate R resp for fault AR
//      1. just support 1 outstanding AR AW
//      2. just support AW start aligned with W, next AWshould never input befor current W.LAST
//
// use to generate B resp or R resp when AW/AR is fault
//
// just support 1 outstanding AR AW
// AW.VALID should aligned with corresponding W.VALID
// next AW never input before current W.LAST
//
///////////////////////////////////////////////////////////////////////////////////////////
module iommu_acd_bus_handler_slv_err  #( //{{{
    parameter   BUS_INFLY_TOKEN_WIDTH   = 6,
    parameter   BUS_ADDR_WIDTH          = 64,
    parameter   BUS_DATA_WIDTH          = 128,
    parameter   BUS_SIZE_WIDTH          = 3,
    parameter   BUS_STRB_WIDTH          = BUS_DATA_WIDTH/8,
    parameter   BUS_ID_WIDTH            = 8,
    parameter   BUS_USER_WIDTH          = 8,
    parameter type          BUS_CH_AX_TYPE          = iommu_acd_pkg::ch_ax_t,
    parameter type          BUS_CH_W_TYPE           = iommu_acd_pkg::ch_w_t,
    parameter type          BUS_CH_B_TYPE           = iommu_acd_pkg::ch_b_t,
    parameter type          BUS_CH_R_TYPE           = iommu_acd_pkg::ch_r_t,
    parameter   SPARE_PARAM             = 0
)(
//{{{ IO
    input  logic                                    clk             ,
    input  logic                                    rstn            ,
    //
    input  logic                                    slv_awvalid_i   ,   // AW valid input, should be align with corresponding W valid, guranteed by outside driver
    output logic                                    slv_awready_o   ,
    input  BUS_CH_AX_TYPE                           slv_awpayld_i   ,
    //
    input  logic                                    slv_wvalid_i    ,
    output logic                                    slv_wready_o    ,
    input  BUS_CH_W_TYPE                            slv_wpayld_i    ,
    //
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        slv_aw_token_i  ,
    //
    output logic                                    slv_bvalid_o    ,
    input  logic                                    slv_bready_i    ,
    output BUS_CH_B_TYPE                            slv_bpayld_o    ,
    //
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        slv_b_token_o   ,
    //
    input  logic                                    slv_arvalid_i   ,
    output logic                                    slv_arready_o   ,
    input  BUS_CH_AX_TYPE                           slv_arpayld_i   ,
    //
    input  logic [BUS_INFLY_TOKEN_WIDTH-1:0]        slv_ar_token_i  ,
    input  logic                                    slv_ar_fault_i  ,
    input  logic                                    slv_ar_mrif_i   ,
    //
    output logic                                    slv_rvalid_o    ,
    input  logic                                    slv_rready_i    ,
    output BUS_CH_R_TYPE                            slv_rpayld_o    ,
    //
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        slv_r_token_o   ,
    //
    input  logic                                    spare_in         
//}}}
);
//=== Declare === {{{
//=== W fault, genenrate B resp
    localparam WS_IDLE              = 1'b0;
    localparam WS_RESP              = 1'b1;
    logic                           wcs, wns;       // WRITE FSM: current_state and next_state

//=== R fault, genenrate R resp
    localparam RS_IDLE              = 1'b0;
    localparam RS_RESP              = 1'b1;
    logic                           rcs, rns;       // READ FSM: current_state and next_state
    logic [7:0]                     arlen;
//}}}

//=== B handle === {{{
    always@(posedge clk or negedge rstn) begin                  // FSM state update
        if(~rstn)
            wcs <= WS_IDLE;
        else
            wcs <= wns;
    end

    always@(*) begin                                            // FSM next state
        case(wcs)
        WS_IDLE: begin
            if(slv_wvalid_i & slv_wready_o & slv_wpayld_i.wlast)// wait for the W.LAST and W handshake
                wns = WS_RESP;                                  // generate B resp
            else
                wns = WS_IDLE;
        end
        WS_RESP: begin
            if(slv_bready_i)                                    // wait for B.READY that indicates the B resp has been accepted
                wns = WS_IDLE;
            else
                wns = WS_RESP;
        end
        default:wns = WS_IDLE;
        endcase
    end

    assign slv_wready_o = (wcs == WS_IDLE);                     // can only handle one outstanding transaction
    assign slv_awready_o = slv_wready_o;                        // AW valid should align with W valid, accept AW and W at the same time
    
    always@(posedge clk or negedge rstn) begin                  // B resp generate
        if(~rstn) begin
            slv_bvalid_o    <= 1'b0;
            slv_bpayld_o    <= 'd0;
            slv_b_token_o   <= 'd0;
        end
        else begin
            if((wns == WS_RESP) & (wcs == WS_IDLE)) begin
                slv_bvalid_o        <= 1'b1;
                slv_bpayld_o.bid    <= slv_awpayld_i.axid;
                slv_bpayld_o.buser  <= slv_awpayld_i.axuser;
                slv_bpayld_o.bidunq <= slv_awpayld_i.axidunq;
                slv_bpayld_o.bloop  <= slv_awpayld_i.axloop;
                slv_b_token_o       <= slv_aw_token_i;
            end
            else if((wns == WS_IDLE) & (wcs == WS_RESP)) begin
                slv_bvalid_o        <= 1'b0;
            end

            slv_bpayld_o.bresp <= 2'b11;
        end
    end
//}}}

//=== R handle === {{{
    always@(posedge clk or negedge rstn) begin                  // FSM update
        if(~rstn)
            rcs <= RS_IDLE;
        else
            rcs <= rns;
    end

    always@(*) begin                                            // FSM next state
        case(rcs)
        RS_IDLE: begin
            if(slv_arvalid_i & slv_arready_o)                   // wait fro the AR.VALID
                rns = RS_RESP;                                  // generate R resp
            else
                rns = RS_IDLE;
        end
        RS_RESP: begin
            if(slv_rready_i & (arlen==0))                       // wait for R.READY and the final R.DATA generated
                rns = RS_IDLE;
            else
                rns = RS_RESP;
        end
        default:rns = RS_IDLE;
        endcase
    end

    always@(posedge clk or negedge rstn) begin                  // R resp number control
        if(~rstn)
            arlen <= 'd0;
        else begin
            if((rns == RS_RESP) & (rcs == RS_IDLE))             // store the AR.LEN when handshake
                arlen <= slv_arpayld_i.axlen;
            else if((rcs == RS_RESP) & slv_rready_i)            // minus 1 with each R.RESP generated
                arlen <= arlen - 'd1;
        end
    end

    always@(posedge clk or negedge rstn) begin                  // R resp generated
        if(~rstn) begin
            slv_rvalid_o    <= 'd0;
            slv_rpayld_o    <= 'd0;
            slv_r_token_o   <= 'd0;
        end
        else begin
            if((rns == RS_RESP) & (rcs == RS_IDLE)) begin
                slv_rvalid_o        <= 1'b1;
                slv_rpayld_o.rid    <= slv_arpayld_i.axid;
                slv_rpayld_o.ruser  <= slv_arpayld_i.axuser;
                slv_rpayld_o.ridunq <= slv_arpayld_i.axidunq;
                slv_rpayld_o.rloop  <= slv_arpayld_i.axloop;
                slv_rpayld_o.rlast  <= (slv_arpayld_i.axlen == 'd0);
                slv_r_token_o       <= slv_ar_token_i;
            end
            else if((rns == RS_IDLE) & (rcs == RS_RESP)) begin
                slv_rvalid_o        <= 1'b0;
                slv_rpayld_o.rlast  <= 1'b0;
            end
            else if((rns == RS_RESP) & (arlen == 'd1)) begin
                slv_rpayld_o.rlast  <= 1'b1;
            end

            if((rns == RS_RESP) && (rcs == RS_IDLE)) begin
                if(slv_ar_fault_i)
                    slv_rpayld_o.rresp  <= 2'b11;
                else
                    slv_rpayld_o.rresp  <= 2'b00;
            end
            slv_rpayld_o.rdata  <= 'd0;
        end
    end

    assign slv_arready_o = (rcs == RS_IDLE);                    // can only handle one outstanding transaction
//}}}

endmodule
//}}}




