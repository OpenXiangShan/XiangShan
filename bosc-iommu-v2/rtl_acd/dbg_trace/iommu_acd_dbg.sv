///////////////////////////////////////////////////////////////////////
// iommu_acd_dbg
///////////////////////////////////////////////////////////////////////
module iommu_acd_dbg_top #(//{{{
//{{{ PARAM
    parameter   TRANS_QIDX_WIDTH                = iommu_acd_pkg::TRANS_QIDX_WIDTH,
    parameter   TLB_QIDX_WIDTH                  = iommu_acd_pkg::TLB_QIDX_WIDTH,
    parameter type          TRANSLATE_REQ_TYPE              = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
    parameter type          TRANSLATE_ACK_TYPE              = iommu_acd_pkg::TRANSLATE_ACK_TYPE,
    parameter   SPARE_PARAM                     = 0
//}}}
)(
//{{{ IO
    input  logic                                            clk,
    input  logic                                            rstn,
//    // CFG
//    input  logic [63:12]                                    tr_req_iova_vpn_i,
//    input  logic [23:0]                                     tr_req_ctl_did_i,
//    input  logic                                            tr_req_ctl_pv_i,
//    input  logic [19:0]                                     tr_req_ctl_pid_i,
//    input  logic                                            tr_req_ctl_nw_i,
//    input  logic                                            tr_req_ctl_exe_i,
//    input  logic                                            tr_req_ctl_priv_i,
//    input  logic                                            tr_req_ctl_go_i,
//    output logic                                            tr_req_finish_o,
//    output logic [63:0]                                     tr_req_resp_o,
    // DBG
    input  logic                                            msg_valid_i,
    output logic                                            msg_ready_o,
    input  logic [63:0]                                     msg_wdata_i,
    output logic                                            msg_valid_o,
    input  logic                                            msg_ready_i,
    output iommu_acd_pkg::MSG_DBG_ACK_TYPE                  msg_data_o,
    output logic                                            msg_last_o,
    // TRANS_UNIT
    output logic                                            dbg_translate_req_valid_o,
    input  logic                                            dbg_translate_req_ready_i,
    output TRANSLATE_REQ_TYPE                               dbg_translate_req_o,
    input  logic                                            dbg_translate_ack_valid_i,
    input  TRANSLATE_ACK_TYPE                               dbg_translate_ack_i,
    // IDBG
`ifdef IOMMU_IDBG
    input  logic                                            idbg_psel_i,
    input  logic                                            idbg_penable_i,
    output logic                                            idbg_pready_o,
    input  logic                                            idbg_pwrite_i,
    input  logic [11:0]                                     idbg_paddr_i,
    input  logic [31:0]                                     idbg_pwdata_i,
    output logic [31:0]                                     idbg_prdata_o,
    output logic                                            idbg_pslverr_o,
    output iommu_acd_pkg::IDBG_TYPE_M                       idbg_intf_m_o[3:0],
    input  iommu_acd_pkg::IDBG_TYPE_S                       idbg_intf_s_i[3:0],
`endif
    input  logic                                            spare_in
//}}}
);
//=== Declare === {{{
//}}}

//=== MainCode === {{{

//}}}

//=== RISCV_DEBUG_CTRL INST === {{{
    iommu_acd_riscv_translation_dbg_ctl #(
    /*parameter  */ .TRANS_QIDX_WIDTH                   (TRANS_QIDX_WIDTH                   ), // = iommu_acd_pkg::TRANS_QIDX_WIDTH,
    /*parameter  */ .TLB_QIDX_WIDTH                     (TLB_QIDX_WIDTH                     ), // = iommu_acd_pkg::TLB_QIDX_WIDTH,
    /*parameter type         */ .TRANSLATE_REQ_TYPE                 (TRANSLATE_REQ_TYPE                 ), // = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
    /*parameter type         */ .TRANSLATE_ACK_TYPE                 (TRANSLATE_ACK_TYPE                 ), // = iommu_acd_pkg::TRANSLATE_ACK_TYPE,
    /*parameter  */ .SPARE_PARAM                        (1'b0                               )  // = 0
    ) U_riscv_translation_dbg_ctl(
    /*input  logic                                              */  .clk                                (clk                                ),
    /*input  logic                                              */  .rstn                               (rstn                               ),
//    /*input  logic [63:12]                                      */  .tr_req_iova_vpn_i                  (tr_req_iova_vpn_i                  ),
//    /*input  logic [23:0]                                       */  .tr_req_ctl_did_i                   (tr_req_ctl_did_i                   ),
//    /*input  logic                                              */  .tr_req_ctl_pv_i                    (tr_req_ctl_pv_i                    ),
//    /*input  logic [19:0]                                       */  .tr_req_ctl_pid_i                   (tr_req_ctl_pid_i                   ),
//    /*input  logic                                              */  .tr_req_ctl_nw_i                    (tr_req_ctl_nw_i                    ),
//    /*input  logic                                              */  .tr_req_ctl_exe_i                   (tr_req_ctl_exe_i                   ),
//    /*input  logic                                              */  .tr_req_ctl_priv_i                  (tr_req_ctl_priv_i                  ),
//    /*input  logic                                              */  .tr_req_ctl_go_i                    (tr_req_ctl_go_i                    ),
//    /*output logic                                              */  .tr_req_finish_o                    (tr_req_finish_o                    ),
//    /*output logic [63:0]                                       */  .tr_req_resp_o                      (tr_req_resp_o                      ),
    /*input  logic                                              */  .msg_valid_i                        (msg_valid_i                        ),
    /*output logic                                              */  .msg_ready_o                        (msg_ready_o                        ),
    /*input  logic [63:0]                                       */  .msg_wdata_i                        (msg_wdata_i                        ),
    /*output logic                                              */  .msg_valid_o                        (msg_valid_o                        ),
    /*input  logic                                              */  .msg_ready_i                        (msg_ready_i                        ),
    /*output iommu_acd_pkg::MSG_DBG_ACK_TYPE                    */  .msg_data_o                         (msg_data_o                         ),
    /*output logic                                              */  .msg_last_o                         (msg_last_o                         ),
    /*output logic                                              */  .dbg_translate_req_valid_o          (dbg_translate_req_valid_o          ),
    /*input  logic                                              */  .dbg_translate_req_ready_i          (dbg_translate_req_ready_i          ),
    /*output TRANSLATE_REQ_TYPE                                 */  .dbg_translate_req_o                (dbg_translate_req_o                ),
    /*input  logic                                              */  .dbg_translate_ack_valid_i          (dbg_translate_ack_valid_i          ),
    /*input  TRANSLATE_ACK_TYPE                                 */  .dbg_translate_ack_i                (dbg_translate_ack_i                ),
    /*input  logic                                              */  .spare_in                           (1'b0                               )
    );

//}}}

//=== IDBG === {{{
`ifdef IOMMU_IDBG
    iommu_acd_idbg_top U_idbg_top(
    /*input  logic                                          */  .clk                (clk                ),
    /*input  logic                                          */  .rstn               (rstn               ),
    /*input  logic                                          */  .idbg_psel_i        (idbg_psel_i        ),
    /*input  logic                                          */  .idbg_penable_i     (idbg_penable_i     ),
    /*output logic                                          */  .idbg_pready_o      (idbg_pready_o      ),
    /*input  logic                                          */  .idbg_pwrite_i      (idbg_pwrite_i      ),
    /*input  logic [11:0]                                   */  .idbg_paddr_i       (idbg_paddr_i       ),
    /*input  logic [31:0]                                   */  .idbg_pwdata_i      (idbg_pwdata_i      ),
    /*output logic [31:0]                                   */  .idbg_prdata_o      (idbg_prdata_o      ),
    /*output logic                                          */  .idbg_pslverr_o     (idbg_pslverr_o     ),
    /*output iommu_acd_pkg::IDBG_TYPE_M                     */  .idbg_intf_m_o      (idbg_intf_m_o      ), //[3:0],
    /*input  iommu_acd_pkg::IDBG_TYPE_S                     */  .idbg_intf_s_i      (idbg_intf_s_i      ), //[3:0],
    /*input  logic                                          */  .spare_in           (1'b0               )
    );
`endif
//}}}

endmodule //}}}


///////////////////////////////////////////////////////////////////////
// iommu-acd_riscv_translateion_dbg_ctl
///////////////////////////////////////////////////////////////////////
module iommu_acd_riscv_translation_dbg_ctl #( //{{{
//{{{ PARAM
    parameter   TRANS_QIDX_WIDTH                = iommu_acd_pkg::TRANS_QIDX_WIDTH,
    parameter   TLB_QIDX_WIDTH                  = iommu_acd_pkg::TLB_QIDX_WIDTH,
    parameter type          TRANSLATE_REQ_TYPE              = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
    parameter type          TRANSLATE_ACK_TYPE              = iommu_acd_pkg::TRANSLATE_ACK_TYPE,
    parameter   SPARE_PARAM                     = 0
//}}}
)(
//{{{ IO
    input  logic                                            clk,
    input  logic                                            rstn,

//    input  logic [63:12]                                    tr_req_iova_vpn_i,
//    input  logic [23:0]                                     tr_req_ctl_did_i,
//    input  logic                                            tr_req_ctl_pv_i,
//    input  logic [19:0]                                     tr_req_ctl_pid_i,
//    input  logic                                            tr_req_ctl_nw_i,
//    input  logic                                            tr_req_ctl_exe_i,
//    input  logic                                            tr_req_ctl_priv_i,
//    input  logic                                            tr_req_ctl_go_i,
//    output logic                                            tr_req_finish_o,
//    output logic [63:0]                                     tr_req_resp_o,
    input  logic                                            msg_valid_i,
    output logic                                            msg_ready_o,
    input  logic [63:0]                                     msg_wdata_i,
    output logic                                            msg_valid_o,
    input  logic                                            msg_ready_i,
    output iommu_acd_pkg::MSG_DBG_ACK_TYPE                  msg_data_o,
    output logic                                            msg_last_o,

    output logic                                            dbg_translate_req_valid_o,
    input  logic                                            dbg_translate_req_ready_i,
    output TRANSLATE_REQ_TYPE                               dbg_translate_req_o,
    input  logic                                            dbg_translate_ack_valid_i,
    input  TRANSLATE_ACK_TYPE                               dbg_translate_ack_i,

    input  logic                                            spare_in
//}}}
);
//{{{ Declare
    localparam  S_IDLE  = 4'b0000;
    localparam  S_REQ   = 4'b0001;
    localparam  S_BUSY  = 4'b0010;
    localparam  S_ACK0  = 4'b1111;
    localparam  S_ACK1  = 4'b1110;

    logic [3:0]         ns, cs;
    logic [63:12]       work_ppn;

    logic                                               tr_req_got;
    logic [3:0]                                         incnt;
    logic [63:12]                                       tr_req_iova_vpn_i;
    logic [23:0]                                        tr_req_ctl_did_i;
    logic                                               tr_req_ctl_pv_i;
    logic [19:0]                                        tr_req_ctl_pid_i;
    logic                                               tr_req_ctl_nw_i;
    logic                                               tr_req_ctl_exe_i;
    logic                                               tr_req_ctl_priv_i;
    logic                                               tr_req_ctl_go_i;

    logic [63:0]                                        tr_req_resp_o, tr_req_resp;

//}}}

//{{{ Main Code
//=== incnt {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            incnt <= 'd0;
        else begin
            if(tr_req_got)
                incnt <= 'd0;
            else if(msg_valid_i & msg_ready_o)
                incnt <= incnt + 'd1;
        end
    end

    assign msg_ready_o = 1'b1;
    assign tr_req_got = msg_valid_i & msg_ready_o & (incnt=='d2);
//}}}

//=== tr req {{{
    assign tr_req_iova_vpn_i= msg_wdata_i[63:12];
    assign tr_req_ctl_did_i = msg_wdata_i[63:40];
    assign tr_req_ctl_pv_i  = msg_wdata_i[32];
    assign tr_req_ctl_pid_i = msg_wdata_i[31:12];
    assign tr_req_ctl_nw_i  = msg_wdata_i[3];
    assign tr_req_ctl_exe_i = msg_wdata_i[2];
    assign tr_req_ctl_priv_i= msg_wdata_i[1];
    assign tr_req_ctl_go_i  = tr_req_got;
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            dbg_translate_req_o <= 'd0;
        else begin
            if(msg_valid_i & msg_ready_o) begin
                if(incnt=='d0) begin
                    dbg_translate_req_o.idx             <= {1'b1,  TRANS_QIDX_WIDTH'(0)};
                    dbg_translate_req_o.priv            <= 'd0;
                    dbg_translate_req_o.ext             <= 'd0;
                    dbg_translate_req_o.wr              <= 'd0;
                    dbg_translate_req_o.is_translated   <= 'd0;
                    dbg_translate_req_o.process_id_valid<= 'd0;
                    dbg_translate_req_o.process_id      <= 'd0;
                    dbg_translate_req_o.device_id       <= 'd0;
                    dbg_translate_req_o.va              <= 'd0;
                end
                else if(incnt=='d1) begin
                    dbg_translate_req_o.idx             <= {1'b1,  TRANS_QIDX_WIDTH'(0)};
                    dbg_translate_req_o.priv            <= 'd0;
                    dbg_translate_req_o.ext             <= 'd0;
                    dbg_translate_req_o.wr              <= 'd0;
                    dbg_translate_req_o.is_translated   <= 'd0;
                    dbg_translate_req_o.process_id_valid<= 'd0;
                    dbg_translate_req_o.process_id      <= 'd0;
                    dbg_translate_req_o.device_id       <= 'd0;
                    dbg_translate_req_o.va              <= tr_req_iova_vpn_i;
                end
                else if(incnt=='d2) begin
                    dbg_translate_req_o.idx             <= {1'b1,  TRANS_QIDX_WIDTH'(0)};
                    dbg_translate_req_o.priv            <= tr_req_ctl_priv_i;
                    dbg_translate_req_o.ext             <= tr_req_ctl_exe_i;
                    dbg_translate_req_o.wr              <= tr_req_ctl_nw_i;
                    dbg_translate_req_o.is_translated   <= 'd0;
                    dbg_translate_req_o.process_id_valid<= tr_req_ctl_pv_i;
                    dbg_translate_req_o.process_id      <= tr_req_ctl_pid_i;
                    dbg_translate_req_o.device_id       <= tr_req_ctl_did_i;
                    dbg_translate_req_o.va              <= dbg_translate_req_o.va;
                end
            end
        end
    end
//}}}

//=== FSM {{{
// FSM-Stage1
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            cs <= S_IDLE;
        else
            cs <= ns;
    end
// FSM-Stage2
    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE: begin
            if(tr_req_ctl_go_i)
                ns = S_REQ;
            else
                ns = S_IDLE;
        end
        S_REQ:  begin
            if(dbg_translate_req_ready_i)
                ns = S_BUSY;
            else
                ns = S_REQ;
        end
        S_BUSY: begin
            if(dbg_translate_ack_valid_i)
                ns = S_ACK0;
            else
                ns = S_BUSY;
        end
        S_ACK0: begin
            if(msg_ready_i)
                ns = S_ACK1;
            else
                ns = S_ACK0;
        end
        S_ACK1: begin
            if(msg_ready_i)
                ns = S_IDLE;
            else
                ns = S_ACK1;
        end
        default: ns = cs;
        endcase
    end
// FSM-Stage3
// translate_req
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            dbg_translate_req_valid_o   <= 1'b0;
//            dbg_translate_req_o         <= 'd0;
        end
        else begin
            if(cs==S_REQ & ns==S_BUSY)
                dbg_translate_req_valid_o   <= 1'b0;
            else if(cs==S_IDLE & ns==S_REQ) begin
                dbg_translate_req_valid_o               <= 1'b1;
//                dbg_translate_req_o.idx                 <= {1'b1,  TRANS_QIDX_WIDTH'(0)};            // bit[TRANS_QIDX_WIDTH]==1, indicates the debug request
//                dbg_translate_req_o.priv                <= tr_req_ctl_priv_i;
//                dbg_translate_req_o.ext                 <= tr_req_ctl_exe_i;
//                dbg_translate_req_o.wr                  <= tr_req_ctl_nw_i;
//                dbg_translate_req_o.is_translated       <= 1'b0;
//                dbg_translate_req_o.process_id_valid    <= tr_req_ctl_pv_i;
//                dbg_translate_req_o.process_id          <= tr_req_ctl_pid_i;
//                dbg_translate_req_o.device_id           <= tr_req_ctl_did_i;
//                dbg_translate_req_o.va                  <= tr_req_iova_vpn_i;
            end
        end
    end
//}}}

//=== translate_ack {{{
    assign work_ppn = (dbg_translate_ack_i.trange==3'b000) ?  dbg_translate_ack_i.pa                                                : // 4K
                      (dbg_translate_ack_i.trange==3'b100) ? {dbg_translate_ack_i.pa[63:16],                               4'b0111} : // 64K
                      (dbg_translate_ack_i.trange==3'b001) ? {dbg_translate_ack_i.pa[63:21],                        9'b0_1111_1111} : // 2M
                      (dbg_translate_ack_i.trange==3'b101) ? {dbg_translate_ack_i.pa[63:22],                      10'b01_1111_1111} : // 4M
                      (dbg_translate_ack_i.trange==3'b010) ? {dbg_translate_ack_i.pa[63:30],            18'b01_1111_1111_1111_1111} : // 1G
                      (dbg_translate_ack_i.trange==3'b110) ? {dbg_translate_ack_i.pa[63:32],          20'b0111_1111_1111_1111_1111} : // 4G
                      (dbg_translate_ack_i.trange==3'b011) ? {dbg_translate_ack_i.pa[63:39], 27'b011_1111_1111_1111_1111_1111_1111} : dbg_translate_ack_i.pa;

//    always@(*) begin
//        tr_req_finish_o = 1'b0;
//        if(cs==S_BUSY & dbg_translate_ack_valid_i) begin
//            tr_req_finish_o = 1'b1;
//        end
//    end
    assign tr_req_resp_o = {
                            4'd0,                                               // custom
                            6'd0,                                               // reserved
                            work_ppn[55:12],                                    // PPN
                            {dbg_translate_ack_i.trange==3'b000 ? 1'b0 : 1'b1},  // S
                            dbg_translate_ack_i.pbmt,                           // PBMT
                            6'd0,                                               // reserved
                            {dbg_translate_ack_i.resp==2'b11 ? 1'b1 : 1'b0}     // fault
                            };
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            tr_req_resp <= 'd0;
        else if(dbg_translate_ack_valid_i)
            tr_req_resp <= tr_req_resp_o;
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            msg_valid_o <= 1'b0;
            msg_data_o  <= 'd0;
            msg_last_o  <= 1'b0;
        end
        else begin
            if(ns==S_ACK0 | ns==S_ACK1)
                msg_valid_o <= 1'b1;
            else
                msg_valid_o <= 1'b0;

            if(ns==S_ACK0)
                msg_data_o  <= {60'd0, iommu_acd_pkg::MSGCODE_DBG};
            else if(ns==S_ACK1)
                msg_data_o  <= tr_req_resp;

            if(ns==S_ACK1)
                msg_last_o  <= 1'b1;
            else
                msg_last_o  <= 1'b0;
        end
    end

//}}}

//}}}

endmodule //}}}



///////////////////////////////////////////////////////////////////////
// iommu_acd_idbg_top
///////////////////////////////////////////////////////////////////////
module iommu_acd_idbg_top ( //{{{
//{{{ IO
    input  logic                                            clk,
    input  logic                                            rstn,
    input  logic                                            idbg_psel_i   ,
    input  logic                                            idbg_penable_i,
    output logic                                            idbg_pready_o ,
    input  logic                                            idbg_pwrite_i ,
    input  logic [11:0]                                     idbg_paddr_i  ,
    input  logic [31:0]                                     idbg_pwdata_i ,
    output logic [31:0]                                     idbg_prdata_o ,
    output logic                                            idbg_pslverr_o,
    output iommu_acd_pkg::IDBG_TYPE_M                       idbg_intf_m_o[3:0],
    input  iommu_acd_pkg::IDBG_TYPE_S                       idbg_intf_s_i[3:0],
    input  logic                                            spare_in
//}}}
);
//=== Declare === {{{
    localparam  IDBG_INTF_SEL       = 12'h000;
    localparam  IDBG_WDATA          = 12'h008;
    localparam  IDBG_RDATA          = 12'h100;

    logic                           reg_wr, reg_rd;
    logic                           go;
    logic [7:0]                     intf_sel;
    logic [7:0]                     opcode;
    logic [31:0]                    wdat, rdat_t;
    logic [1023:0]                  rdat;
    logic                           busy;
    logic                           rdat_clr;
    logic                           rdatv;

    logic [4:0]                     rdat_slice_idx;
//}}}

//=== MainCode === {{{
genvar i;
generate
    for(i=0; i<4; i++) begin : gen_idbg_intf_out_con
        always@(*) begin
            idbg_intf_m_o[i].idbg_go    = (intf_sel==i) ? go : 1'b0;
            idbg_intf_m_o[i].idbg_opcode= opcode;
            idbg_intf_m_o[i].idbg_datw  = wdat;
        end
    end
endgenerate

    always@(*) begin
        case(intf_sel)
        8'd0: busy = idbg_intf_s_i[0].idbg_busy;    // TLB_QUEUE
        8'd1: busy = idbg_intf_s_i[1].idbg_busy;    // MicroTLB
        8'd2: busy = idbg_intf_s_i[2].idbg_busy;    // MainTLB
        8'd3: busy = idbg_intf_s_i[3].idbg_busy;    // TranslateIntf
        default: busy = 1'b0;
        endcase
//        busy = idbg_intf[intf_sel].idbg_busy;
    end

    always@(*) begin
        case(intf_sel)
        8'd0: rdat_t = idbg_intf_s_i[0].idbg_datr;
        8'd1: rdat_t = idbg_intf_s_i[1].idbg_datr;
        8'd2: rdat_t = idbg_intf_s_i[2].idbg_datr;
        8'd3: rdat_t = idbg_intf_s_i[3].idbg_datr;
        default: rdat_t = 32'hdead_beef;
        endcase
//        rdat_t = idbg_intf[intf_sel].idbg_datr;
    end

    always@(*) begin
        case(intf_sel)
        8'd0: rdatv = idbg_intf_s_i[0].idbg_datv;    // TLB_QUEUE
        8'd1: rdatv = idbg_intf_s_i[1].idbg_datv;    // MicroTLB
        8'd2: rdatv = idbg_intf_s_i[2].idbg_datv;    // MainTLB
        8'd3: rdatv = idbg_intf_s_i[3].idbg_datv;    // TranslateIntf
        default: rdatv = 1'b0;
        endcase
//        rdatv = idbg_intf[intf_sel].idbg_datv;
    end

    assign idbg_pslverr_o = 1'b0;
    assign idbg_pready_o  = 1'b1;
    assign reg_wr = idbg_psel_i & idbg_penable_i & idbg_pwrite_i;
    assign reg_rd = idbg_psel_i & ~idbg_pwrite_i;

// IDBG_INTF_SEL {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            intf_sel<= 'd0;
            opcode  <= 'd0;
            go      <= 1'b0;
        end
        else begin
            go  <= 1'b0;
            if(reg_wr & (idbg_paddr_i == IDBG_INTF_SEL )) begin
                intf_sel<= idbg_pwdata_i[19:12];
                opcode  <= idbg_pwdata_i[11:4];
                go      <= idbg_pwdata_i[0];
            end
        end
    end
//}}}

// IDBG_WDATA {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            wdat <= 'd0;
        end
        else begin
            if(reg_wr & (idbg_paddr_i == IDBG_WDATA    )) begin
                wdat <= idbg_pwdata_i;
            end
        end
    end
//}}}

// IDBG_RDATA {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            rdat_clr <= 'd0;
        end
        else begin
            rdat_clr <= 1'b0;
            if(reg_wr & (idbg_paddr_i == IDBG_RDATA    )) begin
                rdat_clr <= 1'b1;
            end
        end
    end


    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            rdat <= 'd0;
        end
        else begin
            if(rdat_clr)
                rdat <= 'd0;
            else if(rdatv)
                rdat <= {rdat[991:0], rdat_t};
        end
    end
//}}}


// READ {{{
    assign rdat_slice_idx = idbg_paddr_i[6:2];
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            idbg_prdata_o <= 'd0;
        else begin
            if(reg_rd) begin
                if (idbg_paddr_i < IDBG_RDATA) begin
                    case(idbg_paddr_i)
                    IDBG_INTF_SEL:  idbg_prdata_o <= {12'd0, opcode, intf_sel, 3'b0, busy};
                    IDBG_WDATA:     idbg_prdata_o <= wdat;
                    default:        idbg_prdata_o <= 'd0;
                    endcase
                end
                else begin
                    case(rdat_slice_idx)
                    5'd 0:  idbg_prdata_o <= rdat[ 0*32+31 :  0*32];
                    5'd 1:  idbg_prdata_o <= rdat[ 1*32+31 :  1*32];
                    5'd 2:  idbg_prdata_o <= rdat[ 2*32+31 :  2*32];
                    5'd 3:  idbg_prdata_o <= rdat[ 3*32+31 :  3*32];
                    5'd 4:  idbg_prdata_o <= rdat[ 4*32+31 :  4*32];
                    5'd 5:  idbg_prdata_o <= rdat[ 5*32+31 :  5*32];
                    5'd 6:  idbg_prdata_o <= rdat[ 6*32+31 :  6*32];
                    5'd 7:  idbg_prdata_o <= rdat[ 7*32+31 :  7*32];
                    5'd 8:  idbg_prdata_o <= rdat[ 8*32+31 :  8*32];
                    5'd 9:  idbg_prdata_o <= rdat[ 9*32+31 :  9*32];
                    5'd10:  idbg_prdata_o <= rdat[10*32+31 : 10*32];
                    5'd11:  idbg_prdata_o <= rdat[11*32+31 : 11*32];
                    5'd12:  idbg_prdata_o <= rdat[12*32+31 : 12*32];
                    5'd13:  idbg_prdata_o <= rdat[13*32+31 : 13*32];
                    5'd14:  idbg_prdata_o <= rdat[14*32+31 : 14*32];
                    5'd15:  idbg_prdata_o <= rdat[15*32+31 : 15*32];
                    5'd16:  idbg_prdata_o <= rdat[16*32+31 : 16*32];
                    5'd17:  idbg_prdata_o <= rdat[17*32+31 : 17*32];
                    5'd18:  idbg_prdata_o <= rdat[18*32+31 : 18*32];
                    5'd19:  idbg_prdata_o <= rdat[19*32+31 : 19*32];
                    5'd20:  idbg_prdata_o <= rdat[20*32+31 : 20*32];
                    5'd21:  idbg_prdata_o <= rdat[21*32+31 : 21*32];
                    5'd22:  idbg_prdata_o <= rdat[22*32+31 : 22*32];
                    5'd23:  idbg_prdata_o <= rdat[23*32+31 : 23*32];
                    5'd24:  idbg_prdata_o <= rdat[24*32+31 : 24*32];
                    5'd25:  idbg_prdata_o <= rdat[25*32+31 : 25*32];
                    5'd26:  idbg_prdata_o <= rdat[26*32+31 : 26*32];
                    5'd27:  idbg_prdata_o <= rdat[27*32+31 : 27*32];
                    5'd28:  idbg_prdata_o <= rdat[28*32+31 : 28*32];
                    5'd29:  idbg_prdata_o <= rdat[29*32+31 : 29*32];
                    5'd30:  idbg_prdata_o <= rdat[30*32+31 : 30*32];
                    5'd31:  idbg_prdata_o <= rdat[31*32+31 : 31*32];
                    default:idbg_prdata_o <= 'd0;
                    endcase
                end
            end
        end
    end
//}}}

//}}}


endmodule //}}}


