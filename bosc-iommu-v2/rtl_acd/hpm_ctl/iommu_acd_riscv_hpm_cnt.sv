module iommu_acd_riscv_hpm_cnt #(
    parameter   EVTIN_NUM               = 2
)(
//{{{ IO
    input  logic                                    clk,
    input  logic                                    rstn,
    // CSR                                          
    input  logic                                    inhibit_i,
    input  logic [14:0]                             eventid_i,
    input  logic                                    dmask_i,
    input  logic [19:0]                             pid_pscid_i,
    input  logic [23:0]                             did_gscid_i,
    input  logic                                    pv_pscv_i,
    input  logic                                    dv_gscv_i,
    input  logic                                    idt_i,
    input  logic                                    ipsr_pmip_clr_i,
    input  logic [63:0]                             iohpmctr_counter_i,
    output logic                                    of_o,
    output logic [63:0]                             counter_o,
    // event                                        
    input  iommu_acd_pkg::RISCV_HPMEVT_TYPE         riscv_hpmevt_intf_i[EVTIN_NUM-1:0],
    //                                              
    input  logic                                    spare_in
//}}}                                               
);                                                  
//{{{ Declare                                       
    logic [19:0]                                    pid_pscid_masked;
    logic [23:0]                                    did_gscid_masked;
    logic [19:0]                                    pid_masked[EVTIN_NUM-1:0];
    logic [23:0]                                    did_masked[EVTIN_NUM-1:0];
    logic [19:0]                                    pscid_masked[EVTIN_NUM-1:0];
    logic [15:0]                                    gscid_masked[EVTIN_NUM-1:0];

    logic [19:0]                                    pid_mask_bits;
    logic [23:0]                                    did_mask_bits;
    logic [EVTIN_NUM-1:0]                           count_enable;
    logic [$clog2(EVTIN_NUM):0]                     incr_value;
    
    logic                                           pid_match[EVTIN_NUM-1:0];
    logic                                           did_match[EVTIN_NUM-1:0];
    logic                                           pscid_match[EVTIN_NUM-1:0];
    logic                                           gscid_match[EVTIN_NUM-1:0];
    logic                                           eventid_match[EVTIN_NUM-1:0];

    logic                                           inhibit;
    logic [63:0]                                    counter;
//}}}

//{{{ Main Code
    always@(*) begin
        pid_mask_bits = 20'hfffff;
        for(int unsigned i=20; i>0; i--) begin
            if(dmask_i & (pid_pscid_i[i-1]==1'b0))
                pid_mask_bits = 20'hfffff << i;
        end
    end
    assign pid_pscid_masked = pid_pscid_i & pid_mask_bits;

    always@(*) begin
        did_mask_bits = 24'hffffff;
        for(int unsigned j=24; j>0; j--) begin
            if(dmask_i & (did_gscid_i[j-1]==1'b0))
                did_mask_bits = 24'hffffff << j;
        end
    end
    assign did_gscid_masked = did_gscid_i & did_mask_bits;

genvar evtn;
generate
    for(evtn=0; evtn<EVTIN_NUM; evtn++) begin : evtin_con_gen
        assign pid_masked[evtn]     = riscv_hpmevt_intf_i[evtn].pid & pid_mask_bits;
        assign did_masked[evtn]     = riscv_hpmevt_intf_i[evtn].did & did_mask_bits;
        assign pscid_masked[evtn]   = riscv_hpmevt_intf_i[evtn].pscid & pid_mask_bits;
        assign gscid_masked[evtn]   = riscv_hpmevt_intf_i[evtn].gscid & did_mask_bits[15:0];
        
        assign pid_match[evtn]  = (idt_i==1'b1)     ? 1'b1 :                                                            // IDTYPE IS PSCID, always match
                                  (pv_pscv_i==1'b0) ? 1'b1 :                                                            // no PSCID match need
                                  (riscv_hpmevt_intf_i[evtn].pv     &   (pid_masked[evtn]==pid_pscid_masked));          // IDTYPE IS PROCESS_ID, compare
        
        assign did_match[evtn]  = (idt_i==1'b1)     ? 1'b1 :                                                            // IDTYPE IS GSCID, always match
                                  (dv_gscv_i==1'b0) ? 1'b1 :                                                            // no GSCID match need
                                  (                                      did_masked[evtn]==did_gscid_masked);           // IDTYPE IS DEVICE_ID, compare
        
        assign pscid_match[evtn]= (idt_i==1'b0)     ? 1'b1 :                                                            // IDTYPE IS PROCESS_ID, always match
                                  (pv_pscv_i==1'b0) ? 1'b1 :                                                            // no PROCESS_ID match need
                                  (riscv_hpmevt_intf_i[evtn].pscv   &   (pscid_masked[evtn]==pid_pscid_masked));        // IDTYPE IS PSCID, compare
        
        assign gscid_match[evtn]= (idt_i==1'b0)     ? 1'b1 :                                                            // IDTYPE IS DEVICE_ID, always match
                                  (dv_gscv_i==1'b0) ? 1'b1 :                                                            // no DEVICE_ID match need
                                  (riscv_hpmevt_intf_i[evtn].gv     &   (gscid_masked[evtn]==did_gscid_masked[15:0]));  // IDTYPE IS GSCID, compare
        
        assign eventid_match[evtn]= (eventid_i==riscv_hpmevt_intf_i[evtn].eventid);
        
        assign count_enable[evtn]= ~inhibit_i &
                                   ~(eventid_i==iommu_acd_pkg::HPMEVENTID_DONOTCOUNT) &
                                   riscv_hpmevt_intf_i[evtn].valid &
                                   eventid_match[evtn] &
                                   pid_match[evtn] &
                                   did_match[evtn] &
                                   pscid_match[evtn] &
                                   gscid_match[evtn] &
                                   1'b1;
    end
endgenerate

    always@(*) begin
        incr_value = 'd0;
        for(int unsigned i=0; i<EVTIN_NUM; i++) begin
            if(count_enable[i]==1'b1) begin
                incr_value = incr_value+'d1;
            end
        end
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            inhibit <= 1'b0;
        else 
            inhibit <= inhibit_i;
    end

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            counter <= 64'd0;
        else if ((inhibit && !inhibit_i) || (ipsr_pmip_clr_i && counter[63]))
            counter <= iohpmctr_counter_i;
        else begin
            if(|count_enable)
                counter <= counter + incr_value;
        end
    end
    assign counter_o = counter;

    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            of_o <= 1'b0;
        else begin
            of_o <= 1'b0;
            if((|count_enable) & counter==64'hffff_ffff_ffff_ffff)
                of_o <= 1'b1;
        end
    end
//}}}
endmodule
