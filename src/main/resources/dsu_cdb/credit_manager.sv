//------------------------------------------------------------------------------
  /*
    Copyright all by the Beijing Institute of Open Source Chip

    Filename    : credit_manager
    File-history: 
       (1) SHAO jinhua created in 20240517: credit handshake signal management logic for data sub-router, 
                                            standing for corresponding input buffer room;
       (2) jin peize do merge 4 channels in 2024/5/20               ;
       (3) pass random regression tests 70W+ rounds up to 2024/5/29 ;
       (4) zhoushize refactoried in 2024/11/08                      ;

  */
//------------------------------------------------------------------------------
`include "common_defines.sv"
`include "router_define.sv"

module credit_manager #(
    parameter CRD_CNT_MAX   = 4 ,
    parameter CRD_CNT_SEL   = 0
) (
    input  wire                   clock       ,
    input  wire                   rstn        ,

    input  wire                   inc         ,
    input  wire                   dec         ,
    
    output reg                    crd_full    ,
    output reg                    crd_avail   ,
    output reg [4          -1 :0] crd_cnt_bin
);

//*** WIRE/REG *****************************************************************
    reg  [CRD_CNT_MAX     -1 :0]  crd_cnt_ns      ;
    reg  [CRD_CNT_MAX     -1 :0]  crd_cnt_q       ;
    wire                          crd_full_ns     ;
    wire                          crd_avail_ns    ;


//*** MAIN BODY ****************************************************************
    assign crd_full_ns  =   crd_cnt_ns[CRD_CNT_MAX-1] ; 
    assign crd_avail_ns =   crd_cnt_ns[0]             ; //not empty

    always @(*) begin : OP_CRD_CNT_NS
        if     (  inc & ~dec ) 
            crd_cnt_ns = {crd_cnt_q[CRD_CNT_MAX-2:0],1'b1}; //shift left
        else if( ~inc &  dec )
            crd_cnt_ns = {1'b0,crd_cnt_q[CRD_CNT_MAX-1:1]}; //shift right
        else
            crd_cnt_ns = crd_cnt_q                        ;
    end

    generate
        if(CRD_CNT_SEL)begin : FULL_CREDIT
            always @(posedge clock or negedge rstn) begin
                if( !rstn ) begin
                    crd_cnt_q <= #`NOC_DELAY {CRD_CNT_MAX{1'b1}}; //for router init full credit 
                end else begin
                    crd_cnt_q <= #`NOC_DELAY crd_cnt_ns             ;
                end
            end
        end else begin : ZERO_CREDIT
            always @(posedge clock or negedge rstn) begin
                if( !rstn ) begin
                    crd_cnt_q <= #`NOC_DELAY {CRD_CNT_MAX{1'b0}}; //for device init zero credit
                end else begin
                    crd_cnt_q <= #`NOC_DELAY crd_cnt_ns             ;
                end
            end
        end
    endgenerate

    always @(posedge clock or negedge rstn) begin : DFF_CRD_FULL
        if( !rstn )
            crd_full <= #`NOC_DELAY 1'b0          ;
        else
            crd_full <= #`NOC_DELAY crd_full_ns   ;
    end

    always @(posedge clock or negedge rstn) begin : DFF_CRD_AVAIL
        if( !rstn ) begin
            crd_avail <= #`NOC_DELAY 1'b0         ;
        end
        else begin
            crd_avail <= #`NOC_DELAY crd_avail_ns ;
        end
    end

    always @(*) begin
        crd_cnt_bin = 4'b0;
        for (integer i=0; i<CRD_CNT_MAX; i=i+1) begin
            if(crd_cnt_q[i]==1'b1)begin
                crd_cnt_bin = i + 1;
            end
        end
    end

//*** DEBUG ********************************************************************


endmodule
