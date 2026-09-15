module iommu_acd_mtlb_rd import iommu_acd_pkg::*; #( //{{{
    parameter   TLB_SIZE_TYPE               = 0,    // 00:4K  01:2M  10:1G  11:512G
    parameter type          REQ_TYPE                    = logic,
    parameter type          RAM_W_TYPE                  = logic,
    parameter type          RAM_R_TYPE                  = logic,
    parameter   MAIN_TLB_WAY_IDX_WIDTH      = 2,
    parameter   MAIN_TLB_BANK_IDX_WIDTH     = 2,
    parameter   MAIN_TLB_SET_IDX_WIDTH      = 5,
    parameter   MAIN_TLB_WAY_NUM            = 2**MAIN_TLB_WAY_IDX_WIDTH ,
    parameter   MAIN_TLB_BANK_NUM           = 2**MAIN_TLB_BANK_IDX_WIDTH,
    parameter   MAIN_TLB_SET_NUM            = 2**MAIN_TLB_SET_IDX_WIDTH ,
    parameter   SPARE_PARAM                 = 0
)(
//{{{ IO
    input  logic                                        clk,
    input  logic                                        rstn,
    // REQ in
    input  logic                                        req_valid_i,
    input  REQ_TYPE                                     req_i,
    // REQ pipe out
    output logic                                        req_valid_o,
    output REQ_TYPE                                     req_o,
    // ENTRY status
    input  MAIN_TLB_ENTRY_STATUS_TYPE                   entry_status_i,
    // RAM RD
    output RAM_W_TYPE                                   ram_wr_o,
    //
    input  logic                                        spare_in 
//}}}
);
//=== Declare {{{
    logic [MAIN_TLB_SET_IDX_WIDTH-1:0]                  idx_cal, ram_idx;        // idx for LKP, UPD, and NON-FUZZY-INV
    logic [63:12]                                       idx_va;
    logic [23:0]                                        idx_device_id;
    logic                                               idx_process_id_valid;
    logic [19:0]                                        idx_process_id;
    logic [1:0]                                         idx_size;

    logic [MAIN_TLB_SET_IDX_WIDTH-1:0]                  ram_addr_raw;
    logic [MAIN_TLB_BANK_IDX_WIDTH-1:0]                 bank_add_raw;
//}}}
    

//=== MainCode {{{
//=== IDX gen {{{
    assign idx_va               =   (req_i.typ==2'b01) ? req_i.lkp_req.va       :
                                    (req_i.typ==2'b10) ? req_i.upd_req.va       :
                                    (req_i.typ==2'b11) ? req_i.inv_req.req.addr : 'd0;
    assign idx_device_id        =   (req_i.typ==2'b01) ? req_i.lkp_req.device_id:
                                    (req_i.typ==2'b10) ? req_i.upd_req.device_id:
                                    (req_i.typ==2'b11) ? req_i.inv_req.req.did_gscid: 'd0;
    assign idx_process_id_valid =   (req_i.typ==2'b01) ? req_i.lkp_req.process_id_valid:
                                    (req_i.typ==2'b10) ? req_i.upd_req.process_id_valid:
                                    (req_i.typ==2'b11) ? req_i.inv_req.req.pscv: 'd0;
    assign idx_process_id       =   (req_i.typ==2'b01) ? req_i.lkp_req.process_id:
                                    (req_i.typ==2'b10) ? req_i.upd_req.process_id:
                                    (req_i.typ==2'b11) ? req_i.inv_req.req.pid_pscid: 'd0;
    assign idx_size             =   TLB_SIZE_TYPE;

    iommu_acd_mtlb_idx #(
    /*parameter  */ .IDX_WIDTH              (MAIN_TLB_SET_IDX_WIDTH+MAIN_TLB_BANK_IDX_WIDTH ) // = 7
    )(
    /*input  logic [23:0]                           */  .device_id              (idx_device_id          ),
    /*input  logic                                  */  .process_id_valid       (idx_process_id_valid   ),
    /*input  logic [19:0]                           */  .process_id             (idx_process_id         ),
    /*input  logic [63:12]                          */  .va                     (idx_va                 ),
    /*input  logic [1:0]                            */  .page_size              (idx_size               ),
    /*output logic [IDX_WIDTH-1:0]                  */  .idx                    (idx_cal                )
    );

    assign ram_idx = (req_i.typ!=2'b11) ? idx_cal :
                     (req_i.inv_req.req.itype==2'b11 & req_i.inv_req.req.av==1'b1) ? idx_cal : req_i.inv_req.hidx;

    assign ram_addr_raw = ram_idx[MAIN_TLB_SET_IDX_WIDTH-1:0];
    assign bank_add_raw = ram_idx[MAIN_TLB_BANK_IDX_WIDTH+MAIN_TLB_SET_IDX_WIDTH-1:MAIN_TLB_SET_IDX_WIDTH];

//}}}

//=== RAM READ {{{
genvar b,w;
generate
    for(b=0; b<MAIN_TLB_BANK_NUM; b++) begin : bank_ram_wr_gen
        for(w=0; w<MAIN_TLB_WAY_NUM; w++) begin : way_ram_wr_gen
            if(req_valid_i & entry_status_i.bank_s[b].set_s[ram_addr_raw].ways[w].val) begin  // input req valid and the entry is valid
                ram_wr_o.bank_w[b].way_w[w].cs  <= 1'b0;    // CS enable
                ram_wr_o.bank_w[b].way_w[w].wrn <= 1'b1;    // READ
                ram_wr_o.bank_w[b].way_w[w].addr<= ram_addr_raw;
                ram_wr_o.bank_w[b].way_w[w].wdata<= 'd0;
            end
            else begin
                ram_wr_o.bank_w[b].way_w[w].cs  <= 1'b1;    // CS disable
                ram_wr_o.bank_w[b].way_w[w].wrn <= 1'b1;    // READ
                ram_wr_o.bank_w[b].way_w[w].addr<= ram_addr_raw;
                ram_wr_o.bank_w[b].way_w[w].wdata<= 'd0;
            end
        end
    end
endgenerate

//}}}

//=== REQ PIPE {{{
    always@(posedge clk or negedge rstn) begin
        if(~rstn) begin
            req_valid_o <= 1'b0;
            reg_o       <= 'd0;
        end
        else if(req_valid_i) begin
            req_valid_o <= 1'b1;
            req_o       <= req_i;
        end
        else begin
            req_valid_o <= 1'b0;
        end
    end
//}}}


//}}}

endmodule
