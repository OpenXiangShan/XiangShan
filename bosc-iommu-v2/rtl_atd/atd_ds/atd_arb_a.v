//*********************************************//
//Project: 	rv_iommu_bosc
//FileName: 	atd_arb_a.v
//Version: 	1.0
//Description:address arbiter
//Author: 	
//*********************************************//

module atd_arb_a
#( 
   parameter pPORT_N=32
  ,parameter pARID_W=5
  ,parameter pMID_W =5
  ,parameter pDATA_W = 21
  ,parameter pFID_W = pARID_W+pMID_W
)(

input							clk,
input							pin_rstn,

//aribter input interface
output [pPORT_N-1:0]			buffer_grant,
input  [pPORT_N-1:0]			buffer_req,
input  [pPORT_N*pDATA_W-1:0]	buffer_araddr,
input  [pPORT_N*pARID_W-1:0]	buffer_arid,

//aribter output interface
input                           sram_grant,
output [pDATA_W-1:0]			cb_req_addr,
output [pFID_W-1:0]             cb_req_arid,
output							cb_req
);


wire rstn;
assign rstn = pin_rstn;
//atd_sync2_rstn u_sync2_rstn(
//.clk    (clk),
//.rst_n  (pin_rstn),
//.so     (rstn)
//);

////----------------------------------------------------
//`ifdef CB_RR_LOOP
////----------------------------------------------------

//wire and reg define
//----------------------------------------------------
reg [pDATA_W-1:0]				arb_araddr;
reg [pARID_W-1:0]				arb_arid  ; 
reg [pPORT_N-1:0]				bus_grant,bus_grant_d;
reg [pMID_W-1:0]				arb_mid;

//arbiter input and output logic
//----------------------------------------------------

wire							buffer_wen;
wire [pPORT_N*pDATA_W-1:0]		bus_araddr     = buffer_araddr;
wire [pPORT_N*pARID_W-1:0]		bus_arid        = buffer_arid;
wire [pPORT_N-1:0]				req_in          = buffer_req;
//assign                       buffer_grant    = bus_grant_d;
assign							buffer_grant    = bus_grant&{pPORT_N{buffer_wen}}; 
wire							grant_from_input= sram_grant;                        
wire [pPORT_N-1:0]				req_mask        = ~bus_grant_d;

reg [pPORT_N-1:0]            rr_req;
reg [pPORT_N-1:0]            loop_mask;
wire [pPORT_N-1:0]           loop_mask_pre;
//wire [pPORT_N-1:0]           high_prio_new_req= req_in & req_mask & loop_mask_pre;
//wire [pPORT_N-1:0]           low_prio_new_req = req_in & req_mask & ~loop_mask_pre;
//wire [pPORT_N-1:0]           high_prio_new_req= req_in & req_mask & loop_mask;
//wire [pPORT_N-1:0]           low_prio_new_req = req_in & req_mask & ~loop_mask;
wire [pPORT_N-1:0]           high_prio_new_req= req_in & loop_mask;
wire [pPORT_N-1:0]           low_prio_new_req = req_in & ~loop_mask;
wire                         high_new_req_valid = (|high_prio_new_req);
wire                         low_new_req_valid  = (|low_prio_new_req);

wire                         rr_valid        = (|rr_req);
wire [pPORT_N-1:0]           bus_req         = ({pPORT_N{rr_valid}}& rr_req)|
                                               ({pPORT_N{~rr_valid & high_new_req_valid}}& high_prio_new_req)|
                                               ({pPORT_N{~rr_valid & ~high_new_req_valid & low_new_req_valid}}& low_prio_new_req);

wire                         req_valid       = (|bus_req);
reg                          rr_valid_d;
always @(posedge clk or negedge rstn) begin
    if (!rstn)
        rr_valid_d   <= 'b0;
    else
        rr_valid_d   <= rr_valid;
end

wire all_req_done = ~rr_valid & rr_valid_d;

wire loop_done = ~loop_mask[pPORT_N-2] & loop_mask[pPORT_N-1];

assign loop_mask_pre = (all_req_done)?(loop_done?{pPORT_N{1'b1}}:loop_mask<<1):loop_mask;

always @(posedge clk or negedge rstn) begin
    if (!rstn)
        loop_mask <= {pPORT_N{1'b1}};
    else
        loop_mask <= loop_mask_pre;
end

//only for case: both high and low are all valid

//case1 is corner case,it is total two req :for example
//new req   :1001
//loop mask :1100  
wire both_new_req  = ~rr_valid & high_new_req_valid & low_new_req_valid & buffer_wen;

wire high_req_done = ~(|(bus_req&(~bus_grant))) & buffer_wen;

wire low_prio_req_load_en_c1 =  both_new_req & high_req_done;

//case2
//case2 have more req than two
reg low_prio_req_case2;
reg [pPORT_N-1:0] low_prio_new_req_r;

wire low_prio_req_load_en_c2 = low_prio_req_case2 & high_req_done;

always @(posedge clk or negedge rstn) begin
    if (!rstn) begin
        low_prio_req_case2 <= 'b0;
        low_prio_new_req_r <= 'b0;
    end else if (low_prio_req_load_en_c1 | low_prio_req_load_en_c2) begin
        low_prio_req_case2 <= 1'b0;
        low_prio_new_req_r <= 'b0;
    end else if ( both_new_req ) begin
        low_prio_req_case2 <= 1'b1;
        low_prio_new_req_r <= low_prio_new_req;
    end
end


//push arb result into fifo
//-----------------------------------------------------------
localparam pBUF_DW = pFID_W+pDATA_W;
wire                          buffer_empty,buffer_full;
wire [pBUF_DW-1:0]            buffer_dout;
wire [pBUF_DW-1:0]            buffer_din     = {arb_mid,arb_arid,arb_araddr}; 
wire                          buffer_ren     = ~buffer_empty & grant_from_input;
assign                        buffer_wen     = ~buffer_full & req_valid;
assign                        cb_req         = ~buffer_empty;
//assign                        {cb_req_mid,cb_req_arid,cb_req_addr} = buffer_dout;
assign                        {cb_req_arid,cb_req_addr} = buffer_dout;
//arbiter main logic
//-----------------------------------------------------------
generate
	if( pPORT_N==3 ) begin : ARB_3TO1
        always @(*) begin
                begin
                    arb_araddr  = 'b0;
                    arb_arid    = 'b0;
                    arb_mid     = 'b0;
					bus_grant   = 3'b000;	
                end
                begin
					if (bus_req[0])   
						arb_araddr = bus_araddr[1*pDATA_W-1:0*pDATA_W];
					else if (bus_req[1]) 
						arb_araddr = bus_araddr[2*pDATA_W-1:1*pDATA_W];
					else if (bus_req[2]) 
						arb_araddr = bus_araddr[3*pDATA_W-1:2*pDATA_W];
					else
						arb_araddr  = 'b0;
		
					if (bus_req[0])
						arb_arid = bus_arid[1*pARID_W-1:0*pARID_W];
					else if (bus_req[1]) 
						arb_arid = bus_arid[2*pARID_W-1:1*pARID_W];
					else if (bus_req[2]) 
						arb_arid= bus_arid[3*pARID_W-1:2*pARID_W];
					else
						arb_arid = 'b0;
		
					if (bus_req[0])
						arb_mid = 'd0;
					else if (bus_req[1]) 
						arb_mid = 'd1;
					else if (bus_req[2]) 
						arb_mid=  'd2;
					else
						arb_mid = 'd0;
		
					if (bus_req[0])
						bus_grant =  3'b001;
					else if (bus_req[1])
						bus_grant =  3'b010;
					else if (bus_req[2]) 
						bus_grant =  3'b100;	
					else
						bus_grant = 3'b000;																
				end
        end
	end
	else if( pPORT_N==4 ) begin : ARB_4TO1
        always @(*) begin
                begin
                    arb_araddr  = 'b0;
                    arb_arid    = 'b0;
                    arb_mid     = 'b0;
					bus_grant   = 4'b0000;	
                end
                begin
					if (bus_req[0])   
						arb_araddr = bus_araddr[1*pDATA_W-1:0*pDATA_W];
					else if (bus_req[1]) 
						arb_araddr = bus_araddr[2*pDATA_W-1:1*pDATA_W];
					else if (bus_req[2]) 
						arb_araddr = bus_araddr[3*pDATA_W-1:2*pDATA_W];
					else if (bus_req[3]) 
						arb_araddr = bus_araddr[4*pDATA_W-1:3*pDATA_W];	
					else
						arb_araddr  = 'b0;
		
					if (bus_req[0])
						arb_arid = bus_arid[1*pARID_W-1:0*pARID_W];
					else if (bus_req[1]) 
						arb_arid = bus_arid[2*pARID_W-1:1*pARID_W];
					else if (bus_req[2]) 
						arb_arid = bus_arid[3*pARID_W-1:2*pARID_W];
					else if (bus_req[3]) 
						arb_arid = bus_arid[4*pARID_W-1:3*pARID_W];
					else
						arb_arid = 'b0;
		
					if (bus_req[0])
						arb_mid = 'd0;
					else if (bus_req[1]) 
						arb_mid = 'd1;
					else if (bus_req[2]) 
						arb_mid=  'd2;
					else if (bus_req[3]) 
						arb_mid=  'd3;
					else
						arb_mid = 'd0;
		
					if (bus_req[0])
						bus_grant =  4'b0001;
					else if (bus_req[1])
						bus_grant =  4'b0010;
					else if (bus_req[2]) 
						bus_grant =  4'b0100;
					else if (bus_req[3]) 
						bus_grant =  4'b1000;
					else
						bus_grant = 4'b0000;																
				end
        end
	end
endgenerate

always @(posedge clk or negedge rstn) begin
    if (!rstn)
        bus_grant_d   <= 'b0;
    else if (buffer_wen) 
        bus_grant_d   <= bus_grant;
    else
        bus_grant_d   <= 'b0;
end

always @(posedge clk or negedge rstn) begin
    if (!rstn)
        rr_req   <= 'b0;
    else if (buffer_wen)
        rr_req   <= (bus_req&(~bus_grant)) | 
                        ({pPORT_N{low_prio_req_load_en_c1}}&low_prio_new_req) | 
                        ({pPORT_N{low_prio_req_load_en_c2}}&low_prio_new_req);
                        //({pPORT_N{low_prio_req_load_en_c2}}&low_prio_new_req_r);
end


//--------------------------------
// arb to fifo
//--------------------------------
// parameter USE_RS:
// 0 - USE FIFO
// 1 - USE Regsiter Slice
 
// When USE_RS=1
// parameter TMO:
// 0 - pass through mode
// 1 - forward timing mode
// 2 - full timing mode
// 3 - backward timing mode
 atd_sync_fifo_wrap
       #(
         .USE_RS    (   1   )
        ,.FIFO_TMO  (   1   ) 
        ,.FIFO_WID  (pBUF_DW)
        ,.FIFO_DEPTH_WID(1)       //depth 2
       )
 u_fifo_wrap     (
          .clk      (clk),
          .rstn     (rstn),
          .wen      (buffer_wen ),
          .ren      (buffer_ren ),
          .din      (buffer_din ),
          .dout     (buffer_dout),
          .full     (buffer_full),
          .empty    (buffer_empty)
  );
 


  
endmodule

