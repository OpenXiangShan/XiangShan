module iommu_acd_hpm_top (
//{{{ IO
    input  logic                                    clk,
    input  logic                                    rstn,
    // CSR                                          
    input  logic                                    iocountinh_i         [1:31],
    input  logic [14:0]                             iohpmevt_eventid_i   [1:31],
    input  logic                                    iohpmevt_dmask_i     [1:31],
    input  logic [19:0]                             iohpmevt_pid_pscid_i [1:31],
    input  logic [23:0]                             iohpmevt_did_gscid_i [1:31],
    input  logic                                    iohpmevt_pv_pscv_i   [1:31],
    input  logic                                    iohpmevt_dv_gscv_i   [1:31],
    input  logic                                    iohpmevt_idt_i       [1:31],
    input  logic                                    iommu_ipsr_pmip_clr_i      ,
    input  logic [63:0]                             iohpmctr_counter_i   [1:31],
    output logic                                    iohpmevt_of_o        [1:31],
    output logic [63:0]                             iohpmctr_counter_o   [1:31],
    // event                                        
    input  iommu_acd_pkg::RISCV_HPMEVT_TYPE         riscv_hpmevt_intf_i[1:0],
    //                                              
    input  logic                                    spare_in
//}}}                                               
);
//{{{ Declare

//}}}

//{{{ Main Code

//}}}

//{{{ RISC-V HPM inst
genvar i;
generate
    for(i=1; i<32; i++) begin : hpm_cnt_gen
        iommu_acd_riscv_hpm_cnt#(
        /*parameter  */ .EVTIN_NUM              (2                          )  // = 0
        ) U_riscv_hpm_cnt(                                                                                          
        /*input  logic                                  */  .clk                        (clk                        ),
        /*input  logic                                  */  .rstn                       (rstn                       ),
        /*input  logic                                  */  .inhibit_i                  (iocountinh_i            [i]),
        /*input  logic [14:0]                           */  .eventid_i                  (iohpmevt_eventid_i      [i]),
        /*input  logic                                  */  .dmask_i                    (iohpmevt_dmask_i        [i]),
        /*input  logic [19:0]                           */  .pid_pscid_i                (iohpmevt_pid_pscid_i    [i]),
        /*input  logic [23:0]                           */  .did_gscid_i                (iohpmevt_did_gscid_i    [i]),
        /*input  logic                                  */  .pv_pscv_i                  (iohpmevt_pv_pscv_i      [i]),
        /*input  logic                                  */  .dv_gscv_i                  (iohpmevt_dv_gscv_i      [i]),
        /*input  logic                                  */  .idt_i                      (iohpmevt_idt_i          [i]),
        /*input  logic                                  */  .ipsr_pmip_clr_i            (iommu_ipsr_pmip_clr_i      ),
        /*input  logic [63:0]                           */  .iohpmctr_counter_i         (iohpmctr_counter_i      [i]),
        /*output logic                                  */  .of_o                       (iohpmevt_of_o           [i]),
        /*output logic [63:0]                           */  .counter_o                  (iohpmctr_counter_o      [i]),
        /*input  iommu_acd_pkg::RISCV_HPMEVT_TYPE       */  .riscv_hpmevt_intf_i        (riscv_hpmevt_intf_i        ), //[EVTIN_NUM-1:0]
        /*input  logic                                  */  .spare_in                   (1'b0                       )
        );
    end
endgenerate
//}}}
endmodule
