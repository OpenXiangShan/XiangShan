////////////////////////////////////////////////////////////////////////////
// iommu_acd_bus_handler_trans_queue_entry
//      1. tranaction info store and ptw req generation
//      2. AXI sequence ctrl (dependency mechanism
//      3. ptw ack process and dequeue
//
// used inside trans_queue
//
////////////////////////////////////////////////////////////////////////////
module iommu_acd_bus_handler_trans_queue_entry #( //{{{
    parameter   IDX_WIDTH               = 8             ,
    parameter   BUS_INFLY_TOKEN_WIDTH   = 6             ,
    parameter   BUS_ADDR_WIDTH          = 64            ,
    parameter   BUS_ID_WIDTH            = 8             ,
    parameter type          TQE_INFO_TYPE           = iommu_acd_pkg::tqe_info_t,
    parameter   IDX_VECTOR_WIDTH        = 2**IDX_WIDTH  ,
    parameter type          TRANSLATE_REQ_TYPE      = iommu_acd_pkg::TRANSLATE_REQ_TYPE,
    parameter type          TRANSLATE_ACK_TYPE      = iommu_acd_pkg::TRANSLATE_ACK_TYPE 
)(
    input  logic                                    clk                         ,
    input  logic                                    rstn                        ,
    input  logic [IDX_WIDTH-1:0]                    idx                         ,
    //
    input  logic                                    update_i                    ,
    input  logic                                    trans_valid_i               ,
    input  logic [23:0]                             trans_device_id_i           ,
    input  logic                                    trans_process_id_valid_i    ,
    input  logic [19:0]                             trans_process_id_i          ,
    input  logic                                    trans_is_translated_i       ,
    input  TQE_INFO_TYPE                            trans_info_i                ,
    input  logic                                    trans_bc_fail_i             ,
    //
    input  logic [IDX_VECTOR_WIDTH-1:0]             depend_bits_i               ,
    output logic                                    depend_bit_o                ,
    input  logic [IDX_VECTOR_WIDTH-1:0]             valid_i                     ,
    input  logic                                    wdata_ready_i               ,
    //
    output logic                                    valid_o                     ,
    output logic [BUS_INFLY_TOKEN_WIDTH-1:0]        token_o                     ,
    output logic [BUS_ID_WIDTH-1:0]                 axid_o                      ,
    //
    output logic                                    ptw_valid_o                 ,
    input  logic                                    ptw_ready_i                 ,
    output TRANSLATE_REQ_TYPE                       ptw_req_o                   ,
    input  logic                                    ptw_ack_valid_i             ,
    input  TRANSLATE_ACK_TYPE                       ptw_ack_i                   ,
    //
    output logic                                    output_valid_o              ,
    input  logic                                    output_ready_i              ,
    output TQE_INFO_TYPE                            output_cont_o               ,
    output logic                                    output_fault_o              ,
    output logic                                    output_wr_o                 ,
    output logic                                    output_mrif_o               ,
    input  logic                                    mrif_barrier_axid_valid_i   ,
    input  logic [BUS_ID_WIDTH-1:0]                 mrif_barrier_axid_i         ,
    //
    input  logic                                    spare_in
);
//=== Declare === {{{
    localparam S_IDLE                               = 3'b000;
    localparam S_TRANSLATE                          = 3'b001;
    localparam S_WAIT_DEPEND                        = 3'b010;
    localparam S_WAIT_DATA                          = 3'b100;
    localparam S_MEM_ACC                            = 3'b111;
    
    logic [2:0] cs, ns;
    logic [IDX_VECTOR_WIDTH-1:0]                    depend_bits, depend_status;
    TQE_INFO_TYPE                                   transaction_info, fast_transaction_info;
    logic                                           translate_is_fail;

    logic                                           output_valid_int;
//}}}

//=== Main code === {{{
//=== FSM stage1 === {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            cs <= S_IDLE;
        end
        else begin
            cs <= ns;
        end
    end
//}}}

//=== FSM stage2 === {{{
    always@(*) begin
        ns = cs;
        case(cs)
        S_IDLE: begin
            if(update_i) begin
                if(trans_bc_fail_i) begin
                    if(depend_bits_i != 'b0) ns = S_WAIT_DEPEND;
                    else begin
                        if(~wdata_ready_i & trans_info_i.wr) ns = S_WAIT_DATA;
                        else ns = S_MEM_ACC;
                    end
                end
                else ns = S_TRANSLATE;
            end
            else ns = S_IDLE;
        end
        S_TRANSLATE: begin
            if(ptw_ack_valid_i) begin
                if(depend_bits != 'b0) ns = S_WAIT_DEPEND;
                else begin
                    if(~wdata_ready_i && transaction_info.wr) ns = S_WAIT_DATA;      // wdata is not ready and this is a Write transaction
                    else ns = S_MEM_ACC;
                end
            end
            else ns = S_TRANSLATE;
        end
        S_WAIT_DEPEND: begin
            if(depend_status == 'b0) begin
                if(~wdata_ready_i && transaction_info.wr) ns = S_WAIT_DATA;          // wdata is not ready and this is a write transaction
                else ns = S_MEM_ACC;
            end
            else ns = S_WAIT_DEPEND;
        end
        S_WAIT_DATA: begin
            if(wdata_ready_i) ns = S_MEM_ACC;
            else ns = S_WAIT_DATA;
        end
        S_MEM_ACC: begin
            if(output_valid_o & output_ready_i) ns = S_IDLE;
            else ns = S_MEM_ACC;
        end
        default: ns = S_IDLE;
        endcase
    end
//}}}

//=== FSM stage3 ===
//=== valid_o
    assign valid_o = (cs != S_IDLE);

//=== depend_bits
    always@(posedge clk or negedge rstn) begin
        if(~rstn)
            depend_bits     <= 'b0;
        else if((ns != S_IDLE) && (cs == S_IDLE))
            depend_bits     <= depend_bits_i;
        else begin
            for(int unsigned kk=0; kk<IDX_VECTOR_WIDTH; kk++) begin
                if(depend_bits[kk] & (~valid_i[kk]))
                    depend_bits[kk] <= 1'b0;
            end
        end
    end
    //assign depend_status = depend_bits & valid_i;
    assign depend_status = depend_bits;

//=== depend_bit_o
    assign depend_bit_o = trans_valid_i & valid_o & (trans_info_i.wr == transaction_info.wr) & (trans_info_i.axpayld.axid == transaction_info.axpayld.axid);

//=== transaction_info, id, rw record
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            transaction_info    <= 'b0;
        end
        else if((ns != S_IDLE) && (cs == S_IDLE)) begin
            transaction_info    <= trans_info_i;
        end
        else if((ns != S_TRANSLATE) && (cs == S_TRANSLATE)) begin
            if(~ptw_ack_i.mrif) begin
                transaction_info.axpayld.axaddr[BUS_ADDR_WIDTH-1:12] <= ptw_ack_i.pa;
            end
            else begin
                transaction_info.axpayld.axaddr[BUS_ADDR_WIDTH-1:9]  <= {3'b0, ptw_ack_i.pa};
            end
            transaction_info.axpayld.axcache <= ptw_ack_i.pbmt=='d1 ? 4'b0011 :    // Svpbmt.NC => AMBA.NORMAL_NON_CACHEABLE{NO_ALLOCATE, NO_ALLOCATE, MODIFIABLE, BUFFERABLE}
                                                ptw_ack_i.pbmt=='d2 ? 4'b0000 :    // Svpbmt.IO => AMBA.DEVICE_nGnRnE{NO_ALLOCATE, NO_ALLOCATE, NO_MODIFIABLE, NO_BUFFERABLE}
                                                transaction_info.axpayld.axcache;  // Svpbmt no active, keep original value
            transaction_info.axpayld.axdomain<= ptw_ack_i.pbmt=='d1 ? 2'b11 :      // Svpbmt.NC => AMBA.NORMAL_NON_CACHEALBE   {SYSTEM}
                                                ptw_ack_i.pbmt=='d2 ? 2'b11 :      // Svpbmt.IO => AMBA.DEVICE_nGnRnE          {SYSTEM}
                                                transaction_info.axpayld.axdomain; // Svpbmt no active, keep original value
            transaction_info.axpayld.axsnoop <= transaction_info.wr ? {transaction_info.axpayld.axsnoop[3:1], 
                                                                       {transaction_info.axpayld.axsnoop[0]&(transaction_info.axpayld.axsnoop[3]|(^fast_transaction_info.axpayld.axdomain))}
                                                                       } :         // WR, keep awsnoop[0] whith STASH_Transaction, otherwise keep awsnoop[0]==0 when awdomain=={NOSHAREABLE|SYSTEM} 
                                                                                   // and keep original value with awdomain=={INNER_SHAREABLE|OUTER_SHAREABLE}
                                                                      transaction_info.axpayld.axsnoop;  // RD, keep arsnoop[0]
        end
    end
    assign token_o  = transaction_info.token;
    assign axid_o   = transaction_info.axpayld.axid;

    always@(*) begin
        fast_transaction_info = transaction_info;
        if((ns == S_MEM_ACC) && (cs == S_TRANSLATE)) begin
            if(~ptw_ack_i.mrif) begin
                fast_transaction_info.axpayld.axaddr[BUS_ADDR_WIDTH-1:12] = ptw_ack_i.pa;
            end
            else begin
                fast_transaction_info.axpayld.axaddr[BUS_ADDR_WIDTH-1:9] = {3'b0, ptw_ack_i.pa};
            end
            fast_transaction_info.axpayld.axcache  = ptw_ack_i.pbmt=='d1 ? 4'b0011 :    // Svpbmt.NC => AMBA.NORMAL_NON_CACHEABLE{NO_ALLOCATE, NO_ALLOCATE, MODIFIABLE, BUFFERABLE}
                                                     ptw_ack_i.pbmt=='d2 ? 4'b0000 :    // Svpbmt.IO => AMBA.DEVICE_nGnRnE{NO_ALLOCATE, NO_ALLOCATE, NO_MODIFIABLE, NO_BUFFERABLE}
                                                     transaction_info.axpayld.axcache;  // Svpbmt no active, keep original value
            fast_transaction_info.axpayld.axdomain = ptw_ack_i.pbmt=='d1 ? 2'b11 :      // Svpbmt.NC => AMBA.NORMAL_NON_CACHEALBE   {SYSTEM}
                                                     ptw_ack_i.pbmt=='d2 ? 2'b11 :      // Svpbmt.IO => AMBA.DEVICE_nGnRnE          {SYSTEM}
                                                     transaction_info.axpayld.axdomain; // Svpbmt no active, keep original value
            fast_transaction_info.axpayld.axsnoop  = transaction_info.wr ? {transaction_info.axpayld.axsnoop[3:1], 
                                                                            {transaction_info.axpayld.axsnoop[0]&(transaction_info.axpayld.axsnoop[3]|(^fast_transaction_info.axpayld.axdomain))}
                                                                            } :         // WR, keep awsnoop[0] whith STASH_Transaction, otherwise keep awsnoop[0]==0 when awdomain=={NOSHAREABLE|SYSTEM} 
                                                                                        // and keep original value with awdomain=={INNER_SHAREABLE|OUTER_SHAREABLE}
                                                                           transaction_info.axpayld.axsnoop;  // RD, keep arsnoop[0]
        end
        else begin
            fast_transaction_info = transaction_info;
        end
    end
//=== 
    always @(posedge clk or negedge rstn) begin
        if (~rstn)begin
            ptw_valid_o <= 1'b0;
        end
        else begin
            if(ptw_valid_o & ptw_ready_i)
                ptw_valid_o <= 1'b0;
            else if(cs != S_TRANSLATE && ns == S_TRANSLATE)
                ptw_valid_o <= 1'b1;
        end
    end

    always @(posedge clk or negedge rstn) begin
        if (~rstn)begin
            ptw_req_o <= 'b0;
        end
        else begin
            if(cs == S_IDLE && ns == S_TRANSLATE)
                ptw_req_o <= {
                    {1'b0, idx}                     ,
                    trans_info_i.axpayld.axprot[0]  ,   // privilege
                    trans_info_i.axpayld.axprot[2]  ,   // X
                    trans_info_i.wr                 ,   // w or r
                    trans_is_translated_i           ,
                    trans_process_id_valid_i        ,
                    trans_process_id_i              ,
                    trans_device_id_i               ,   // 
                    trans_info_i.axpayld.axaddr[BUS_ADDR_WIDTH-1:12] 
                };
        end
    end

//=== output_fault_o
    assign translate_is_fail = (ptw_ack_i.resp != 'd0);

    always @(posedge clk or negedge rstn) begin
        if (~rstn)
            output_fault_o <= 1'b0;
        else if(output_valid_o && output_ready_i)
            output_fault_o <= 1'b0;
        else if((ns != S_IDLE) && (cs == S_IDLE))
            output_fault_o <= trans_bc_fail_i;
        else if((ns != S_TRANSLATE) && (cs == S_TRANSLATE))
            output_fault_o <= translate_is_fail;
    end

//=== output_cont_o
    always @(posedge clk or negedge rstn) begin
        if (~rstn)
            output_cont_o <= 'd0;
        else if(cs != S_MEM_ACC && ns == S_MEM_ACC)
            output_cont_o <= (cs == S_TRANSLATE) ? fast_transaction_info:
                             (cs == S_IDLE)      ? trans_info_i         :
                                                   transaction_info     ;
    end

//=== output_wr_o
    always @(posedge clk or negedge rstn) begin
        if (~rstn)begin
            output_wr_o <= 'd0;
        end
        //20251205 gpf, need updates earlier for mrif use(used at the timee ptw_ack_valid_i actives)
        else if(cs == S_TRANSLATE && ns == S_TRANSLATE) begin
            output_wr_o <= transaction_info.wr;
        end
        else if(cs != S_MEM_ACC && ns == S_MEM_ACC) begin
            output_wr_o <=  (cs == S_TRANSLATE) ? transaction_info.wr   :
                            (cs == S_IDLE     ) ? trans_info_i.wr       :
                                                  transaction_info.wr   ;
        end
    end

//=== output_valid_o
    always @(posedge clk or negedge rstn) begin
        if (~rstn)begin
            output_valid_int <= 1'b0;
        end
        else begin
            if(output_valid_o & output_ready_i)
                output_valid_int <= 1'b0;
            else if(cs != S_MEM_ACC && ns == S_MEM_ACC)
                output_valid_int <= 1'b1;
        end
    end
    assign output_valid_o = output_valid_int & ~(output_wr_o & mrif_barrier_axid_valid_i & output_cont_o.axpayld.axid==mrif_barrier_axid_i);

//=== output_mrif_o
    always @(posedge clk or negedge rstn) begin
        if (~rstn)
            output_mrif_o <= 1'b0;
        else if(output_valid_o && output_ready_i)
            output_mrif_o <= 1'b0;
        else if((ns != S_TRANSLATE) && (cs == S_TRANSLATE))
            output_mrif_o <= ptw_ack_i.mrif;
    end

//}}}

endmodule //}}}



