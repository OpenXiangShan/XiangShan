/////////////////////////////////////////////////////////////////////////////////////////
// iommu_acd_cfg
/////////////////////////////////////////////////////////////////////////////////////////
module iommu_acd_cfg import iommu_acd_pkg::*; #(
    parameter   SPARE_PARAM         = 0
)(                                              
    input  logic                                clk                                         ,
    input  logic                                rstn                                        ,
    // CSR OUT                                  
    output logic                                iommu_fctl_gxl_o                            ,
    output logic [3:0]                          iommu_ddtp_iommu_mode_o                     ,
    output logic                                iommu_ipsr_pmip_clr_o                       ,
    output logic                                acd_fault_ctrl_multi_hit_check_en_o         ,
    output logic                                acd_fault_ctrl_selfdefine_fault_rpt_en_o    ,
//    output logic [63:12]                        tr_req_iova_vpn_o                           ,
//    output logic [23:0]                         tr_req_ctl_did_o                            ,
//    output logic                                tr_req_ctl_pv_o                             ,
//    output logic [19:0]                         tr_req_ctl_pid_o                            ,
//    output logic                                tr_req_ctl_nw_o                             ,
//    output logic                                tr_req_ctl_exe_o                            ,
//    output logic                                tr_req_ctl_priv_o                           ,
//    output logic                                tr_req_ctl_go_o                             ,
    output logic                                iocountinh_o                          [1:31],
    output logic [14:0]                         iohpmevt_eventid_o                    [1:31],
    output logic                                iohpmevt_dmask_o                      [1:31],
    output logic [19:0]                         iohpmevt_pid_pscid_o                  [1:31],
    output logic [23:0]                         iohpmevt_did_gscid_o                  [1:31],
    output logic                                iohpmevt_pv_pscv_o                    [1:31],
    output logic                                iohpmevt_dv_gscv_o                    [1:31],
    output logic                                iohpmevt_idt_o                        [1:31],
    output logic [63:0]                         iohpmctr_counter_o                    [1:31],
    input  logic                                iohpmevt_of_i                         [1:31],
    input  logic [63:0]                         iohpmctr_counter_i                    [1:31],
    output logic [31:1]                         iocountovf_pos_o                            ,
//    // STATE IN                                 
//    input  logic                                tr_req_finish_i                             ,
//    input  logic [63:0]                         tr_req_resp_i                               ,
    // RAW Interrupt                            
    input  logic                                int_multi_hit_check_fail_i                  ,
    input  logic                                int_inv_buf_overflow_i                      ,
    input  logic                                int_fault_buf_overflow_i                    ,
    input  logic                                int_performance_buf_overflow_i              ,
    // Int enable out                           
    output logic                                int_multi_hit_check_fail_en_o               ,
    output logic                                int_inv_buf_overflow_en_o                   ,
    output logic                                int_fault_buf_overflow_en_o                 ,
    output logic                                int_performance_buf_overflow_en_o           ,
    // T2C IF                                   
    input  logic                                msg_valid_i                                 ,
    output logic                                msg_ready_o                                 ,
    input  MSG_CFG_ACCESS_TYPE                  msg_wdata_i                                 ,
    // C2T IF                                   
    output logic                                msg_rvalid_o                                ,
    input  logic                                msg_rready_i                                ,
    output MSG_CFG_ACK_TYPE                     msg_rdata_o                                 ,
    output logic                                msg_rlast_o                                 ,
    //                                          
    input  logic [1:0]                          trans_unit_ecc_err_i                        ,
    output logic [1:0]                          ecc_err_o                                   ,
    //                                          
    input  logic                                spare_in
);
//=== Declare {{{
    localparam IOMMU_FCTL                       = 10'h8;
    localparam IOMMU_DDTP                       = 10'h10;
//    localparam IOMMU_TR_REQ_IOVA_L              = 10'h258;
//    localparam IOMMU_TR_REQ_IOVA_H              = 10'h25c;
//    localparam IOMMU_TR_REQ_CTL_L               = 10'h260;
//    localparam IOMMU_TR_REQ_CTL_H               = 10'h264;
//    localparam IOMMU_TR_RESPONSE_L              = 10'h268;
//    localparam IOMMU_TR_RESPONSE_H              = 10'h26c;
    localparam IOMMU_IPSR                       = 10'h54;
//    localparam IOMMU_IOCOUNTOVF                 = 10'h58;
    localparam IOMMU_IOCOUNTINH                 = 10'h5c;
    localparam IOMMU_IOHPMCTRBASE               = 10'h68;
    localparam IOMMU_IOHPMEVTBASE               = 10'h160;
    localparam ACD_FAULT_CTRL                   = 10'h280;
    localparam ACD_INT_EN                       = 10'h290;
    localparam ACD_INT_ST                       = 10'h298;
                                                
    logic [31:0]                                iommu_fctl     ;
    logic [31:0]                                iommu_ddtp     ;
    logic [31:0]                                acd_fault_ctrl ;
    logic [31:0]                                acd_int_en     ;
    logic [31:0]                                acd_int_st, acd_int_st_ff     ;
//    logic [63:0]                                tr_req_iova    ;
//    logic [63:0]                                tr_req_ctl     ;
//    logic                                       tr_req_ctl_busy;
//    logic [63:0]                                tr_response    ;
    logic                                       iommu_ipsr_pmip_clr;                                            
//    logic                                       ipsr_pmip;
    logic [31:0]                                iocountovf;
    logic [31:0]                                iocountinh            ;
    logic [14:0]                                iohpmevt_eventid      [1:31];
    logic                                       iohpmevt_dmask        [1:31];
    logic [19:0]                                iohpmevt_pid_pscid    [1:31];
    logic [23:0]                                iohpmevt_did_gscid    [1:31];
    logic                                       iohpmevt_pv_pscv      [1:31];
    logic                                       iohpmevt_dv_gscv      [1:31];
    logic                                       iohpmevt_idt          [1:31];
    logic [63:0]                                iohpmevt_rdata        [1:31];
                                                
    logic [63:0]                                iohpmctr_counter      [1:31];
                                                
    logic                                       csr_wr, csr_rd;
    logic                                       msg_wr        ;
    logic [9:0]                                 msg_addr      ;
    logic [31:0]                                msg_wdata     ;

    logic [1:0]                                 ecc_err_int_en;
//}}}

//=== MainCode {{{
    assign msg_wr   = msg_wdata_i.cfg_rw;
    assign msg_addr = {msg_wdata_i.cfg_addr, 2'b00};
    assign msg_wdata= msg_wdata_i.cfg_data;

    assign csr_wr = msg_valid_i & msg_ready_o & msg_wr;
    assign csr_rd = msg_valid_i & msg_ready_o & ~msg_wr;

//=== WR {{{
//= IOMMU_FCTL {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            iommu_fctl <= 'd0;
        end
        else if(csr_wr & (msg_addr == IOMMU_FCTL       )) begin
            iommu_fctl <= msg_wdata;
        end
    end
    
    assign iommu_fctl_gxl_o = iommu_fctl[2];
//}}}

//= IOMMU_DDTP {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            iommu_ddtp <= 'd0;
        end
        else if(csr_wr & (msg_addr == IOMMU_DDTP       )) begin
            iommu_ddtp <= msg_wdata;
        end
    end
    
    assign iommu_ddtp_iommu_mode_o = iommu_ddtp[3:0];
//}}}


//= IOMMU_IPSR {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            iommu_ipsr_pmip_clr <= 1'b0;
        else if(csr_wr & (msg_addr == IOMMU_IPSR       )) 
            iommu_ipsr_pmip_clr <= msg_wdata[2];
        else
            iommu_ipsr_pmip_clr <= 1'b0;
    end
    
    assign iommu_ipsr_pmip_clr_o = iommu_ipsr_pmip_clr;
//}}}


//== ACD_FAULT_CTRL {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            acd_fault_ctrl <= 32'b10;
        end
        else if(csr_wr & (msg_addr == ACD_FAULT_CTRL   )) begin
            if(msg_wdata[0] == 1'b1)
                acd_fault_ctrl[0] <= 1'b0;
            else if(int_fault_buf_overflow_i)
                acd_fault_ctrl[0] <= 1'b1;

            acd_fault_ctrl[2:1] <= msg_wdata[2:1];
            acd_fault_ctrl[31:3] <= 'd0;
        end
    end

    assign acd_fault_ctrl_multi_hit_check_en_o      = acd_fault_ctrl[2];
    assign acd_fault_ctrl_selfdefine_fault_rpt_en_o = acd_fault_ctrl[1];
//}}}

//== ACD_INT_EN {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            acd_int_en <= 32'b0111;
        end
        else if(csr_wr & (msg_addr == ACD_INT_EN       )) begin
            acd_int_en <= msg_wdata;
        end
    end

    assign {
        int_multi_hit_check_fail_en_o,
        int_inv_buf_overflow_en_o    ,
        int_fault_buf_overflow_en_o  ,
        int_performance_buf_overflow_en_o
        } = acd_int_en[3:0];

    assign ecc_err_int_en = acd_int_en[5:4];
//}}}

//== ACD_INT_ST {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            acd_int_st_ff <= 'd0;
            acd_int_st <= 'd0;
        end
        else if(csr_wr & (msg_addr==ACD_FAULT_CTRL  )) begin
            for(int unsigned i=0; i<32; i++) begin
                if(msg_wdata[i] == 1'b1)
                    acd_int_st[i] <= 1'b0;
            end
        end
        else begin
            acd_int_st_ff <= acd_int_st;

            if(int_multi_hit_check_fail_i & int_multi_hit_check_fail_en_o)
                acd_int_st[3]   <= 1'b1;

            if(int_inv_buf_overflow_i & int_inv_buf_overflow_en_o)
                acd_int_st[2]   <= 1'b1;

            if(int_fault_buf_overflow_i & int_fault_buf_overflow_en_o)
                acd_int_st[1]   <= 1'b1;

            if(int_performance_buf_overflow_i & int_performance_buf_overflow_en_o)
                acd_int_st[0]   <= 1'b1;

            if(trans_unit_ecc_err_i[0] & ecc_err_int_en[0])
                acd_int_st[4]   <= 1'b1;

            if(trans_unit_ecc_err_i[1] & ecc_err_int_en[1])
                acd_int_st[5]   <= 1'b1;
        end
    end

    assign ecc_err_o[0] = ~acd_int_st_ff[4] & acd_int_st[4];
    assign ecc_err_o[1] = ~acd_int_st_ff[5] & acd_int_st[5];
//}}}

////= TR_REQ_IOVA {{{
//    always@(posedge clk or negedge rstn) begin
//        if(~rstn) begin
//            tr_req_iova <= 'd0;
//        end
//        else begin
//            if(csr_wr & (msg_addr == IOMMU_TR_REQ_IOVA_L    )) begin
//                tr_req_iova <= {tr_req_iova[63:32], msg_wdata};
//            end
//            else if(csr_wr & (msg_addr == IOMMU_TR_REQ_IOVA_H    )) begin
//                tr_req_iova <= {msg_wdata, tr_req_iova[31:0]};
//            end
//        end
//    end
//    
//    assign tr_req_iova_vpn_o = tr_req_iova[63:12];
//
////}}}

////= TR_REQ_CTL {{{
//    always@(posedge clk or negedge rstn) begin
//        if(~rstn) begin
//            tr_req_ctl      <= 'd0;
//            tr_req_ctl_busy <= 1'b0;
//        end
//        else begin
//            if(csr_wr & (msg_addr == IOMMU_TR_REQ_CTL_L     )) begin
//                tr_req_ctl <= {tr_req_ctl[63:32], msg_wdata};
//            end
//            else if(csr_wr & (msg_addr == IOMMU_TR_REQ_CTL_H     )) begin
//                tr_req_ctl <= {msg_wdata, tr_req_ctl[31:0]};
//            end
//
//            if(tr_req_finish_i)
//                tr_req_ctl_busy <= 1'b0;
//            else if(csr_wr & (msg_addr == IOMMU_TR_REQ_CTL_L     )) begin
//                tr_req_ctl_busy <= msg_wdata[0];
//            end
//        end
//    end
//    
//    assign tr_req_ctl_did_o  = tr_req_ctl[63:40];
//    assign tr_req_ctl_pv_o   = tr_req_ctl[32];
//    assign tr_req_ctl_pid_o  = tr_req_ctl[31:12];
//    assign tr_req_ctl_nw_o   = tr_req_ctl[3];
//    assign tr_req_ctl_exe_o  = tr_req_ctl[2];
//    assign tr_req_ctl_priv_o = tr_req_ctl[1];
//    assign tr_req_ctl_go_o   = tr_req_ctl_busy;
//
////}}}

////= TR_RESPONSE {{{
//    always@(posedge clk or negedge rstn) begin
//        if(~rstn) begin
//            tr_response <= 'd0;
//        end
//        else if(tr_req_finish_i) begin
//            tr_response <= tr_req_resp_i;
//        end
//    end
////}}}

//= IPSR{{{
//    always@(posedge clk or negedge rstn) begin
//        if(~rstn)
//            ipsr_pmip <= 'd0;
//        else begin
//            if(csr_wr & (msg_addr==IOMMU_IPSR   ) & (msg_wdata[2]==1'b1))
//                ipsr_pmip <= 1'b0;
//            else if(|iocountovf_pos_o)
//                ipsr_pmip <= 1'b1;
//        end
//    end
//}}}

//= IOMMU_IOCOUNTOVF & IOHPMEVTx.OF {{{
genvar gof;
generate
    assign iocountovf[0] = 1'b0;
    for(gof=1; gof<32; gof++) begin : iocountovf_gen
        always@(posedge clk or negedge rstn) begin
            if(~rstn) begin
                iocountovf[gof] <= 'd0;
            end
            else begin
                if(csr_wr & (msg_addr==IOMMU_IOHPMEVTBASE+8*gof-4))
                    iocountovf[gof] <= msg_wdata[31];
                else if(iohpmevt_of_i[gof]==1'b1)
                    iocountovf[gof] <= 1'b1;
            end
        end
        assign iocountovf_pos_o[gof] = ((iocountovf[gof]==1'b0) & (iohpmevt_of_i[gof]==1'b1));
    end
endgenerate
    
//}}}

//= IOMMU_IOCOUNTINH {{{
genvar inh;
generate
//    assign iocountinh[0] = 1'b0;
    for(inh=0; inh<32; inh++) begin : iocountinh_gen
        always@(posedge clk or negedge rstn) begin
            if(~rstn) begin
                iocountinh[inh] <= 'd0;
            end
            else begin
                if(csr_wr & (msg_addr==IOMMU_IOCOUNTINH))
                    iocountinh[inh] <= msg_wdata[inh];
            end
        end
        if (inh>0 && inh<32) begin
            assign iocountinh_o[inh] = iocountinh[inh];
        end
    end
endgenerate
//}}}

//= IOHPMCTR {{{
genvar hpmctr;
generate
    for(hpmctr=1; hpmctr<32; hpmctr++) begin : iohpmctr_gen
        always@(posedge clk or negedge rstn) begin
            if(~rstn)
                iohpmctr_counter  [hpmctr] <= 'd0;
            else if(csr_wr & (msg_addr==IOMMU_IOHPMCTRBASE+8*hpmctr-8))
                iohpmctr_counter  [hpmctr] <= {iohpmctr_counter[hpmctr][63:32], msg_wdata};
            else if(csr_wr & (msg_addr==IOMMU_IOHPMCTRBASE+8*hpmctr-4))
                iohpmctr_counter  [hpmctr] <= {msg_wdata, iohpmctr_counter[hpmctr][31:0]};
            else
                iohpmctr_counter  [hpmctr] <= iohpmctr_counter  [hpmctr];
        end

        assign iohpmctr_counter_o  [hpmctr] = iohpmctr_counter  [hpmctr];

    end
endgenerate

//}}}

//= IOHPMEVT {{{
genvar hpmevt;
generate
    for(hpmevt=1; hpmevt<32; hpmevt++) begin : iohpmevt_gen
        always@(posedge clk or negedge rstn) begin
            if(~rstn) begin
                iohpmevt_eventid  [hpmevt] <= 'd0;
                iohpmevt_dmask    [hpmevt] <= 'd0;
                iohpmevt_pid_pscid[hpmevt] <= 'd0;
                iohpmevt_did_gscid[hpmevt] <= 'd0;
                iohpmevt_pv_pscv  [hpmevt] <= 'd0;
                iohpmevt_dv_gscv  [hpmevt] <= 'd0;
                iohpmevt_idt      [hpmevt] <= 'd0;
            end
            else begin
                if(csr_wr & (msg_addr==IOMMU_IOHPMEVTBASE+8*hpmevt-8)) begin
                    iohpmevt_eventid  [hpmevt] <= msg_wdata[14:0];
                    iohpmevt_dmask    [hpmevt] <= msg_wdata[15];
                    iohpmevt_pid_pscid[hpmevt] <= {iohpmevt_pid_pscid[hpmevt][19:16], msg_wdata[31:16]};
                end
                else if(csr_wr & (msg_addr==IOMMU_IOHPMEVTBASE+8*hpmevt-4)) begin
                    iohpmevt_pid_pscid[hpmevt] <= {msg_wdata[3:0], iohpmevt_pid_pscid[hpmevt][15:0]};
                    iohpmevt_did_gscid[hpmevt] <= msg_wdata[27:4];
                    iohpmevt_pv_pscv  [hpmevt] <= msg_wdata[28];
                    iohpmevt_dv_gscv  [hpmevt] <= msg_wdata[29];
                    iohpmevt_idt      [hpmevt] <= msg_wdata[30];
                end
            end
        end
        assign iohpmevt_eventid_o  [hpmevt] = iohpmevt_eventid  [hpmevt];
        assign iohpmevt_dmask_o    [hpmevt] = iohpmevt_dmask    [hpmevt];
        assign iohpmevt_pid_pscid_o[hpmevt] = iohpmevt_pid_pscid[hpmevt];
        assign iohpmevt_did_gscid_o[hpmevt] = iohpmevt_did_gscid[hpmevt];
        assign iohpmevt_pv_pscv_o  [hpmevt] = iohpmevt_pv_pscv  [hpmevt];
        assign iohpmevt_dv_gscv_o  [hpmevt] = iohpmevt_dv_gscv  [hpmevt];
        assign iohpmevt_idt_o      [hpmevt] = iohpmevt_idt      [hpmevt];

        assign iohpmevt_rdata      [hpmevt] = {
                                              iocountovf        [hpmevt],
                                              iohpmevt_idt      [hpmevt],
                                              iohpmevt_dv_gscv  [hpmevt],
                                              iohpmevt_pv_pscv  [hpmevt],
                                              iohpmevt_did_gscid[hpmevt],
                                              iohpmevt_pid_pscid[hpmevt],
                                              iohpmevt_dmask    [hpmevt],
                                              iohpmevt_eventid  [hpmevt]
                                                };
    end
endgenerate
//}}}

//}}}

//=== RD {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            msg_rvalid_o<= 1'b0;
            msg_rdata_o <= 'd0;
        end
        else begin
            if(msg_rvalid_o & ~msg_rready_i) begin
                msg_rvalid_o <= 1'b1;
            end
            else if(csr_rd) begin
                msg_rvalid_o <= 1'b1;
                case(msg_addr)
                IOMMU_FCTL                  : msg_rdata_o <= {iommu_fctl[31:3],3'd0,               19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_DDTP                  : msg_rdata_o <= {iommu_ddtp            ,               19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
//                IOMMU_TR_REQ_IOVA_L         : msg_rdata_o <= {tr_req_iova[31:0]     ,               19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
//                IOMMU_TR_REQ_IOVA_H         : msg_rdata_o <= {tr_req_iova[63:32]    ,               19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
//                IOMMU_TR_REQ_CTL_L          : msg_rdata_o <= {tr_req_ctl[31:1], tr_req_ctl_busy,    19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
//                IOMMU_TR_REQ_CTL_H          : msg_rdata_o <= {tr_req_ctl[63:32]     ,               19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
//                IOMMU_TR_RESPONSE_L         : msg_rdata_o <= {tr_response[31:0]     ,               19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
//                IOMMU_TR_RESPONSE_H         : msg_rdata_o <= {tr_response[63:32]    ,               19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
//                IOMMU_IPSR                  : msg_rdata_o <= {29'd0, ipsr_pmip, 2'b0,               19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
//                IOMMU_IOCOUNTOVF            : msg_rdata_o <= {iocountovf,                           19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOCOUNTINH            : msg_rdata_o <= {iocountinh,                           19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*1 -8) : msg_rdata_o <= {iohpmctr_counter_i[1 ][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*2 -8) : msg_rdata_o <= {iohpmctr_counter_i[2 ][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*3 -8) : msg_rdata_o <= {iohpmctr_counter_i[3 ][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*4 -8) : msg_rdata_o <= {iohpmctr_counter_i[4 ][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*5 -8) : msg_rdata_o <= {iohpmctr_counter_i[5 ][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*6 -8) : msg_rdata_o <= {iohpmctr_counter_i[6 ][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*7 -8) : msg_rdata_o <= {iohpmctr_counter_i[7 ][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*8 -8) : msg_rdata_o <= {iohpmctr_counter_i[8 ][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*9 -8) : msg_rdata_o <= {iohpmctr_counter_i[9 ][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*10-8) : msg_rdata_o <= {iohpmctr_counter_i[10][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*11-8) : msg_rdata_o <= {iohpmctr_counter_i[11][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*12-8) : msg_rdata_o <= {iohpmctr_counter_i[12][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*13-8) : msg_rdata_o <= {iohpmctr_counter_i[13][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*14-8) : msg_rdata_o <= {iohpmctr_counter_i[14][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*15-8) : msg_rdata_o <= {iohpmctr_counter_i[15][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*16-8) : msg_rdata_o <= {iohpmctr_counter_i[16][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*17-8) : msg_rdata_o <= {iohpmctr_counter_i[17][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*18-8) : msg_rdata_o <= {iohpmctr_counter_i[18][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*19-8) : msg_rdata_o <= {iohpmctr_counter_i[19][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*20-8) : msg_rdata_o <= {iohpmctr_counter_i[20][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*21-8) : msg_rdata_o <= {iohpmctr_counter_i[21][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*22-8) : msg_rdata_o <= {iohpmctr_counter_i[22][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*23-8) : msg_rdata_o <= {iohpmctr_counter_i[23][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*24-8) : msg_rdata_o <= {iohpmctr_counter_i[24][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*25-8) : msg_rdata_o <= {iohpmctr_counter_i[25][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*26-8) : msg_rdata_o <= {iohpmctr_counter_i[26][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*27-8) : msg_rdata_o <= {iohpmctr_counter_i[27][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*28-8) : msg_rdata_o <= {iohpmctr_counter_i[28][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*29-8) : msg_rdata_o <= {iohpmctr_counter_i[29][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*30-8) : msg_rdata_o <= {iohpmctr_counter_i[30][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*31-8) : msg_rdata_o <= {iohpmctr_counter_i[31][31:0],         19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*1 -4) : msg_rdata_o <= {iohpmctr_counter_i[1 ][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*2 -4) : msg_rdata_o <= {iohpmctr_counter_i[2 ][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*3 -4) : msg_rdata_o <= {iohpmctr_counter_i[3 ][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*4 -4) : msg_rdata_o <= {iohpmctr_counter_i[4 ][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*5 -4) : msg_rdata_o <= {iohpmctr_counter_i[5 ][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*6 -4) : msg_rdata_o <= {iohpmctr_counter_i[6 ][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*7 -4) : msg_rdata_o <= {iohpmctr_counter_i[7 ][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*8 -4) : msg_rdata_o <= {iohpmctr_counter_i[8 ][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*9 -4) : msg_rdata_o <= {iohpmctr_counter_i[9 ][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*10-4) : msg_rdata_o <= {iohpmctr_counter_i[10][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*11-4) : msg_rdata_o <= {iohpmctr_counter_i[11][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*12-4) : msg_rdata_o <= {iohpmctr_counter_i[12][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*13-4) : msg_rdata_o <= {iohpmctr_counter_i[13][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*14-4) : msg_rdata_o <= {iohpmctr_counter_i[14][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*15-4) : msg_rdata_o <= {iohpmctr_counter_i[15][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*16-4) : msg_rdata_o <= {iohpmctr_counter_i[16][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*17-4) : msg_rdata_o <= {iohpmctr_counter_i[17][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*18-4) : msg_rdata_o <= {iohpmctr_counter_i[18][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*19-4) : msg_rdata_o <= {iohpmctr_counter_i[19][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*20-4) : msg_rdata_o <= {iohpmctr_counter_i[20][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*21-4) : msg_rdata_o <= {iohpmctr_counter_i[21][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*22-4) : msg_rdata_o <= {iohpmctr_counter_i[22][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*23-4) : msg_rdata_o <= {iohpmctr_counter_i[23][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*24-4) : msg_rdata_o <= {iohpmctr_counter_i[24][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*25-4) : msg_rdata_o <= {iohpmctr_counter_i[25][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*26-4) : msg_rdata_o <= {iohpmctr_counter_i[26][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*27-4) : msg_rdata_o <= {iohpmctr_counter_i[27][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*28-4) : msg_rdata_o <= {iohpmctr_counter_i[28][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*29-4) : msg_rdata_o <= {iohpmctr_counter_i[29][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*30-4) : msg_rdata_o <= {iohpmctr_counter_i[30][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMCTRBASE+(8*31-4) : msg_rdata_o <= {iohpmctr_counter_i[31][63:32],        19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*1 -8) : msg_rdata_o <= {iohpmevt_rdata[1 ][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*2 -8) : msg_rdata_o <= {iohpmevt_rdata[2 ][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*3 -8) : msg_rdata_o <= {iohpmevt_rdata[3 ][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*4 -8) : msg_rdata_o <= {iohpmevt_rdata[4 ][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*5 -8) : msg_rdata_o <= {iohpmevt_rdata[5 ][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*6 -8) : msg_rdata_o <= {iohpmevt_rdata[6 ][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*7 -8) : msg_rdata_o <= {iohpmevt_rdata[7 ][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*8 -8) : msg_rdata_o <= {iohpmevt_rdata[8 ][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*9 -8) : msg_rdata_o <= {iohpmevt_rdata[9 ][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*10-8) : msg_rdata_o <= {iohpmevt_rdata[10][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*11-8) : msg_rdata_o <= {iohpmevt_rdata[11][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*12-8) : msg_rdata_o <= {iohpmevt_rdata[12][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*13-8) : msg_rdata_o <= {iohpmevt_rdata[13][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*14-8) : msg_rdata_o <= {iohpmevt_rdata[14][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*15-8) : msg_rdata_o <= {iohpmevt_rdata[15][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*16-8) : msg_rdata_o <= {iohpmevt_rdata[16][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*17-8) : msg_rdata_o <= {iohpmevt_rdata[17][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*18-8) : msg_rdata_o <= {iohpmevt_rdata[18][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*19-8) : msg_rdata_o <= {iohpmevt_rdata[19][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*20-8) : msg_rdata_o <= {iohpmevt_rdata[20][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*21-8) : msg_rdata_o <= {iohpmevt_rdata[21][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*22-8) : msg_rdata_o <= {iohpmevt_rdata[22][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*23-8) : msg_rdata_o <= {iohpmevt_rdata[23][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*24-8) : msg_rdata_o <= {iohpmevt_rdata[24][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*25-8) : msg_rdata_o <= {iohpmevt_rdata[25][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*26-8) : msg_rdata_o <= {iohpmevt_rdata[26][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*27-8) : msg_rdata_o <= {iohpmevt_rdata[27][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*28-8) : msg_rdata_o <= {iohpmevt_rdata[28][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*29-8) : msg_rdata_o <= {iohpmevt_rdata[29][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*30-8) : msg_rdata_o <= {iohpmevt_rdata[30][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*31-8) : msg_rdata_o <= {iohpmevt_rdata[31][31:0],             19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*1 -4) : msg_rdata_o <= {iohpmevt_rdata[1 ][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*2 -4) : msg_rdata_o <= {iohpmevt_rdata[2 ][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*3 -4) : msg_rdata_o <= {iohpmevt_rdata[3 ][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*4 -4) : msg_rdata_o <= {iohpmevt_rdata[4 ][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*5 -4) : msg_rdata_o <= {iohpmevt_rdata[5 ][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*6 -4) : msg_rdata_o <= {iohpmevt_rdata[6 ][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*7 -4) : msg_rdata_o <= {iohpmevt_rdata[7 ][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*8 -4) : msg_rdata_o <= {iohpmevt_rdata[8 ][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*9 -4) : msg_rdata_o <= {iohpmevt_rdata[9 ][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*10-4) : msg_rdata_o <= {iohpmevt_rdata[10][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*11-4) : msg_rdata_o <= {iohpmevt_rdata[11][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*12-4) : msg_rdata_o <= {iohpmevt_rdata[12][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*13-4) : msg_rdata_o <= {iohpmevt_rdata[13][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*14-4) : msg_rdata_o <= {iohpmevt_rdata[14][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*15-4) : msg_rdata_o <= {iohpmevt_rdata[15][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*16-4) : msg_rdata_o <= {iohpmevt_rdata[16][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*17-4) : msg_rdata_o <= {iohpmevt_rdata[17][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*18-4) : msg_rdata_o <= {iohpmevt_rdata[18][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*19-4) : msg_rdata_o <= {iohpmevt_rdata[19][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*20-4) : msg_rdata_o <= {iohpmevt_rdata[20][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*21-4) : msg_rdata_o <= {iohpmevt_rdata[21][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*22-4) : msg_rdata_o <= {iohpmevt_rdata[22][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*23-4) : msg_rdata_o <= {iohpmevt_rdata[23][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*24-4) : msg_rdata_o <= {iohpmevt_rdata[24][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*25-4) : msg_rdata_o <= {iohpmevt_rdata[25][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*26-4) : msg_rdata_o <= {iohpmevt_rdata[26][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*27-4) : msg_rdata_o <= {iohpmevt_rdata[27][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*28-4) : msg_rdata_o <= {iohpmevt_rdata[28][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*29-4) : msg_rdata_o <= {iohpmevt_rdata[29][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*30-4) : msg_rdata_o <= {iohpmevt_rdata[30][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                IOMMU_IOHPMEVTBASE+(8*31-4) : msg_rdata_o <= {iohpmevt_rdata[31][63:32],            19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                ACD_FAULT_CTRL              : msg_rdata_o <= {acd_fault_ctrl        ,               19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                ACD_INT_EN                  : msg_rdata_o <= {acd_int_en            ,               19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                ACD_INT_ST                  : msg_rdata_o <= {acd_int_st            ,               19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                default                     : msg_rdata_o <= {32'd0                 ,               19'd0, msg_addr[9:2], 1'd0, MSGCODE_CFG_ACK};
                endcase
            end
            else if(csr_wr) begin
                msg_rvalid_o <= 1'b1;
                msg_rdata_o  <= {32'd0, 19'd0, msg_addr[9:2], 1'd1, MSGCODE_CFG_ACK};
            end
            else begin
                msg_rvalid_o <= 1'b0;
            end
        end
    end

    assign msg_rlast_o  = msg_rvalid_o;
//}}}

//=== Ready
    assign msg_ready_o = msg_rvalid_o ? msg_rready_i : 1'b1;
//}}}
endmodule



