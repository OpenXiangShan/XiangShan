
//------------------------------------------------------------------------------
/*
    Copyright all by the Beijing Institute of Open Source CHIip

    Filename    : common_defines
    File-history: 
       (1) qubaosen create file at 11/26;
       (2) chenyingxue add new macros at 11/27;
       (3) Junyan Xie add CHI-E.b support on 2025/04/03;

  */

`ifdef  DSU_SUPERBENCH_CONHERENCE
    `include "dsu_verification_defines.sv"
`endif

`ifdef  DSU_TOP_CHI_B_MODE_ON
    `include "dsu_chib_defines.sv"
`else
    `include "dsu_chie_defines.sv"
`endif

`define   DSU_LOGICAL_ID_WIDTH     6
`define   DSU_DEV_TYPE_WIDTH       4

`define   C2XM_AXDATA_WIDTH    256
`define   C2XM_PA_WIDTH        48
//CACHELINE WIDTH AND RANGE
`define DSU_CACHELINE_WIDTH                    512  
`define DSU_CACHELINE_RANGE                    (`DSU_CACHELINE_WIDTH-1):0  

//TLR 
`define DSU_TLR_FLIT_WIDTH     `DSU_PSACTRL_WIDTH

//AXI macro
// Read Address Channel
`define DSU_AX4_ARID_WIDTH     11
`define DSU_AX4_ARADDR_WIDTH   48
`define DSU_AX4_ARLEN_WIDTH    8
`define DSU_AX4_ARSIZE_WIDTH   3
`define DSU_AX4_ARBURST_WIDTH  2
`define DSU_AX4_ARLOCK_WIDTH   1
`define DSU_AX4_ARCACHE_WIDTH  4
`define DSU_AX4_ARPROT_WIDTH   3
`define DSU_AX4_ARQOS_WIDTH    4
`define DSU_AX4_ARREGION_WIDTH -1             // no def
`define DSU_AX4_ARUSER_WIDTH   4             // no def
`define DSU_AX4_ARVALID_WIDTH  1
`define DSU_AX4_ARREADY_WIDTH  1
`define DSU_AX4_ARSNOOP_WIDTH  4              //IN C2XM BUT NOT IN AXI
`define DSU_AX4_ARDOMAIN_WIDTH 2              //IN C2XM BUT NOT IN AXI

`define DSU_AX4_ARID_RANGE           (`DSU_AX4_ARID_WIDTH - 1):0
`define DSU_AX4_ARADDR_RANGE         (`DSU_AX4_ARADDR_WIDTH - 1):0
`define DSU_AX4_ARLEN_RANGE          (`DSU_AX4_ARLEN_WIDTH - 1):0
`define DSU_AX4_ARSIZE_RANGE         (`DSU_AX4_ARSIZE_WIDTH - 1):0
`define DSU_AX4_ARBURST_RANGE        (`DSU_AX4_ARBURST_WIDTH - 1):0
`define DSU_AX4_ARLOCK_RANGE         (`DSU_AX4_ARLOCK_WIDTH - 1):0
`define DSU_AX4_ARCACHE_RANGE        (`DSU_AX4_ARCACHE_WIDTH - 1):0
`define DSU_AX4_ARPROT_RANGE         (`DSU_AX4_ARPROT_WIDTH - 1):0
`define DSU_AX4_ARQOS_RANGE          (`DSU_AX4_ARQOS_WIDTH - 1):0
`define DSU_AX4_ARREGION_RANGE       (`DSU_AX4_ARREGION_WIDTH - 1):0
`define DSU_AX4_ARUSER_RANGE         (`DSU_AX4_ARUSER_WIDTH - 1):0
`define DSU_AX4_ARVALID_RANGE        (`DSU_AX4_ARVALID_WIDTH - 1):0
`define DSU_AX4_ARREADY_RANGE        (`DSU_AX4_ARREADY_WIDTH - 1):0
`define DSU_AX4_ARSNOOP_RANGE        (`DSU_AX4_ARSNOOP_WIDTH - 1):0
`define DSU_AX4_ARDOMAIN_RANGE       (`DSU_AX4_ARDOMAIN_WIDTH - 1):0


// Write Address Channel
`define DSU_AX4_AWID_WIDTH             `DSU_AX4_ARID_WIDTH
`define DSU_AX4_AWADDR_WIDTH           `DSU_AX4_ARADDR_WIDTH
`define DSU_AX4_AWLEN_WIDTH            `DSU_AX4_ARLEN_WIDTH
`define DSU_AX4_AWSIZE_WIDTH           `DSU_AX4_ARSIZE_WIDTH
`define DSU_AX4_AWBURST_WIDTH          `DSU_AX4_ARBURST_WIDTH
`define DSU_AX4_AWLOCK_WIDTH           `DSU_AX4_ARLOCK_WIDTH
`define DSU_AX4_AWCACHE_WIDTH          `DSU_AX4_ARCACHE_WIDTH
`define DSU_AX4_AWPROT_WIDTH           `DSU_AX4_ARPROT_WIDTH
`define DSU_AX4_AWQOS_WIDTH            `DSU_AX4_ARQOS_WIDTH
`define DSU_AX4_AWREGION_WIDTH         `DSU_AX4_ARREGION_WIDTH             //no def
`define DSU_AX4_AWUSER_WIDTH           `DSU_AX4_ARUSER_WIDTH               //no def
`define DSU_AX4_AWVALID_WIDTH          `DSU_AX4_ARVALID_WIDTH
`define DSU_AX4_AWREADY_WIDTH          `DSU_AX4_ARREADY_WIDTH


`define DSU_AX4_AWID_RANGE           (`DSU_AX4_AWID_WIDTH - 1):0
`define DSU_AX4_AWADDR_RANGE         (`DSU_AX4_AWADDR_WIDTH - 1):0
`define DSU_AX4_AWLEN_RANGE          (`DSU_AX4_AWLEN_WIDTH - 1):0
`define DSU_AX4_AWSIZE_RANGE         (`DSU_AX4_AWSIZE_WIDTH - 1):0
`define DSU_AX4_AWBURST_RANGE        (`DSU_AX4_AWBURST_WIDTH - 1):0
`define DSU_AX4_AWLOCK_RANGE         (`DSU_AX4_AWLOCK_WIDTH - 1):0
`define DSU_AX4_AWCACHE_RANGE        (`DSU_AX4_AWCACHE_WIDTH - 1):0
`define DSU_AX4_AWPROT_RANGE         (`DSU_AX4_AWPROT_WIDTH - 1):0
`define DSU_AX4_AWQOS_RANGE          (`DSU_AX4_AWQOS_WIDTH - 1):0
`define DSU_AX4_AWREGION_RANGE       (`DSU_AX4_AWREGION_WIDTH - 1):0
`define DSU_AX4_AWUSER_RANGE         (`DSU_AX4_AWUSER_WIDTH - 1):0
`define DSU_AX4_AWVALID_RANGE        (`DSU_AX4_AWVALID_WIDTH - 1):0
`define DSU_AX4_AWREADY_RANGE        (`DSU_AX4_AWREADY_WIDTH - 1):0

// Write Data Channel
`define DSU_AX4_WID_WIDTH      11             // no def
`define DSU_AX4_WDATA_WIDTH    256
`define DSU_AX4_WSTRB_WIDTH    32
`define DSU_AX4_WLAST_WIDTH    1
`define DSU_AX4_WUSER_WIDTH    4             // no def
`define DSU_AX4_WVALID_WIDTH   1
`define DSU_AX4_WREADY_WIDTH   1
`define DSU_AX4_WPOISON_WIDTH 4              //IN C2XM BUT NOT IN AXI
`define DSU_AX4_WDATACHK_WIDTH 32            //IN C2XM BUT NOT IN AXI

`define DSU_AX4_WID_RANGE            (`DSU_AX4_WID_WIDTH - 1):0
`define DSU_AX4_WDATA_RANGE          (`DSU_AX4_WDATA_WIDTH - 1):0
`define DSU_AX4_WSTRB_RANGE          (`DSU_AX4_WSTRB_WIDTH - 1):0
`define DSU_AX4_WLAST_RANGE          (`DSU_AX4_WLAST_WIDTH - 1):0
`define DSU_AX4_WUSER_RANGE          (`DSU_AX4_WUSER_WIDTH - 1):0
`define DSU_AX4_WVALID_RANGE         (`DSU_AX4_WVALID_WIDTH - 1):0
`define DSU_AX4_WREADY_RANGE         (`DSU_AX4_WREADY_WIDTH - 1):0
`define DSU_AX4_WPOISON_RANGE        (`DSU_AX4_WPOISON_WIDTH - 1):0
`define DSU_AX4_WDATACHK_RANGE       (`DSU_AX4_WDATACHK_WIDTH - 1):0

// Write Response Channel
`define DSU_AX4_BID_WIDTH      11
`define DSU_AX4_BRESP_WIDTH    2
`define DSU_AX4_BUSER_WIDTH    4             // no def
`define DSU_AX4_BVALID_WIDTH   1
`define DSU_AX4_BREADY_WIDTH   1

`define DSU_AX4_BID_RANGE            (`DSU_AX4_BID_WIDTH - 1):0
`define DSU_AX4_BRESP_RANGE          (`DSU_AX4_BRESP_WIDTH - 1):0
`define DSU_AX4_BUSER_RANGE          (`DSU_AX4_BUSER_WIDTH - 1):0
`define DSU_AX4_BVALID_RANGE         (`DSU_AX4_BVALID_WIDTH - 1):0
`define DSU_AX4_BREADY_RANGE         (`DSU_AX4_BREADY_WIDTH - 1):0


// Read Data Channel
`define DSU_AX4_RID_WIDTH      11
`define DSU_AX4_RDATA_WIDTH    256
`define DSU_AX4_RRESP_WIDTH    2
`define DSU_AX4_RLAST_WIDTH    1
`define DSU_AX4_RUSER_WIDTH    4             // no def
`define DSU_AX4_RVALID_WIDTH   1
`define DSU_AX4_RREADY_WIDTH   1
`define DSU_AX4_RTRACE_WIDTH   1              //IN C2XM BUT NOT IN AXI
`define DSU_AX4_RPOISON_WIDTH  4              //IN C2XM BUT NOT IN AXI
`define DSU_AX4_RDATACHK_WIDTH 32             //IN C2XM BUT NOT IN AXI

`define DSU_AX4_RID_RANGE            (`DSU_AX4_RID_WIDTH - 1):0
`define DSU_AX4_RDATA_RANGE          (`DSU_AX4_RDATA_WIDTH - 1):0
`define DSU_AX4_RRESP_RANGE          (`DSU_AX4_RRESP_WIDTH - 1):0
`define DSU_AX4_RLAST_RANGE          (`DSU_AX4_RLAST_WIDTH - 1):0
`define DSU_AX4_RUSER_RANGE          (`DSU_AX4_RUSER_WIDTH - 1):0
`define DSU_AX4_RVALID_RANGE         (`DSU_AX4_RVALID_WIDTH - 1):0
`define DSU_AX4_RREADY_RANGE         (`DSU_AX4_RREADY_WIDTH - 1):0
`define DSU_AX4_RTRACE_RANGE         (`DSU_AX4_RTRACE_WIDTH - 1):0
`define DSU_AX4_RPOISON_RANGE        (`DSU_AX4_RPOISON_WIDTH - 1):0
`define DSU_AX4_RDATACHK_RANGE       (`DSU_AX4_RDATACHK_WIDTH - 1):0

// System Signals
`define DSU_AX4_CSYSREQ_WIDTH  -1              // no def
`define DSU_AX4_CSYSACK_WIDTH  -1              // no def
`define DSU_AX4_CACTIVE_WIDTH  -1              // no def

`define DSU_AX4_CSYSREQ_RANGE        (`DSU_AX4_CSYSREQ_WIDTH - 1):0
`define DSU_AX4_CSYSACK_RANGE        (`DSU_AX4_CSYSACK_WIDTH - 1):0
`define DSU_AX4_CACTIVE_RANGE        (`DSU_AX4_CACTIVE_WIDTH - 1):0



/*RNSAM*/
`define DSU_BASE_ADDR0        48'h1000_0000
`define DSU_BASE_ADDR1        48'h0
`define DSU_BASE_ADDR0_OFFSET 48'h7000_0000
`define DSU_BASE_ADDR1_OFFSET 48'h0

`define DSU_DLB_ADDR          48'h6000_0000
`define DSU_DLB_ADDR_OFFSET   48'h1000_0000

`ifdef DSU_TOP_HNF_NID_0
   `define DSU_HNF0_NID          `DSU_TOP_HNF_NID_0
`else
   `define DSU_HNF0_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_1
   `define DSU_HNF1_NID          `DSU_TOP_HNF_NID_1
`else
   `define DSU_HNF1_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_2
   `define DSU_HNF2_NID          `DSU_TOP_HNF_NID_2
`else
   `define DSU_HNF2_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_3
   `define DSU_HNF3_NID          `DSU_TOP_HNF_NID_3
`else
   `define DSU_HNF3_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_4
   `define DSU_HNF4_NID          `DSU_TOP_HNF_NID_4
`else
   `define DSU_HNF4_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_5
   `define DSU_HNF5_NID          `DSU_TOP_HNF_NID_5
`else
   `define DSU_HNF5_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_6
   `define DSU_HNF6_NID          `DSU_TOP_HNF_NID_6
`else
   `define DSU_HNF6_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_7
   `define DSU_HNF7_NID          `DSU_TOP_HNF_NID_7
`else
   `define DSU_HNF7_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_8
   `define DSU_HNF8_NID          `DSU_TOP_HNF_NID_8
`else
   `define DSU_HNF8_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_9
   `define DSU_HNF9_NID          `DSU_TOP_HNF_NID_9
`else
   `define DSU_HNF9_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_10
   `define DSU_HNF10_NID          `DSU_TOP_HNF_NID_10
`else
   `define DSU_HNF10_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_11
   `define DSU_HNF11_NID          `DSU_TOP_HNF_NID_11
`else
   `define DSU_HNF11_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_12
   `define DSU_HNF12_NID          `DSU_TOP_HNF_NID_12
`else
   `define DSU_HNF12_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_13
   `define DSU_HNF13_NID          `DSU_TOP_HNF_NID_13
`else
   `define DSU_HNF13_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_14
   `define DSU_HNF14_NID          `DSU_TOP_HNF_NID_14
`else
   `define DSU_HNF14_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_15
   `define DSU_HNF15_NID          `DSU_TOP_HNF_NID_15
`else
   `define DSU_HNF15_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_16
   `define DSU_HNF16_NID          `DSU_TOP_HNF_NID_16
`else
   `define DSU_HNF16_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_17
   `define DSU_HNF17_NID          `DSU_TOP_HNF_NID_17
`else
   `define DSU_HNF17_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_18
   `define DSU_HNF18_NID          `DSU_TOP_HNF_NID_18
`else
   `define DSU_HNF18_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_19
   `define DSU_HNF19_NID          `DSU_TOP_HNF_NID_19
`else
   `define DSU_HNF19_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_20
   `define DSU_HNF20_NID          `DSU_TOP_HNF_NID_20
`else
   `define DSU_HNF20_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_21
   `define DSU_HNF21_NID          `DSU_TOP_HNF_NID_21
`else
   `define DSU_HNF21_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_22
   `define DSU_HNF22_NID          `DSU_TOP_HNF_NID_22
`else
   `define DSU_HNF22_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_23
   `define DSU_HNF23_NID          `DSU_TOP_HNF_NID_23
`else
   `define DSU_HNF23_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_24
   `define DSU_HNF24_NID          `DSU_TOP_HNF_NID_24
`else
   `define DSU_HNF24_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_25
   `define DSU_HNF25_NID          `DSU_TOP_HNF_NID_25
`else
   `define DSU_HNF25_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_26
   `define DSU_HNF26_NID          `DSU_TOP_HNF_NID_26
`else
   `define DSU_HNF26_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_27
   `define DSU_HNF27_NID          `DSU_TOP_HNF_NID_27
`else
   `define DSU_HNF27_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_28
   `define DSU_HNF28_NID          `DSU_TOP_HNF_NID_28
`else
   `define DSU_HNF28_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_29
   `define DSU_HNF29_NID          `DSU_TOP_HNF_NID_29
`else
   `define DSU_HNF29_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_30
   `define DSU_HNF30_NID          `DSU_TOP_HNF_NID_30
`else
   `define DSU_HNF30_NID          0
`endif
`ifdef DSU_TOP_HNF_NID_31
   `define DSU_HNF31_NID          `DSU_TOP_HNF_NID_31
`else
   `define DSU_HNF31_NID          0
`endif
`ifdef  DSU_TOP_HNF_NUM
    `define DSU_HNF_NUM           `DSU_TOP_HNF_NUM
`else
    `define DSU_HNF_NUM           5'd0
`endif
`ifdef DSU_TOP_HND_NID
    `define DSU_HND_NID           `DSU_TOP_HND_NID
`else
        `define DSU_HND_NID       0
`endif

`define SYS_CACHE_GRP_START   44'h0
`define SYS_CACHE_GRP_SIZE    44'h800_0000_0000

`define DSU_DEV_TYPE_OTHERS         4'd0
`define DSU_DEV_TYPE_RNFBESAM       4'b1101 //13
`define DSU_DEV_TYPE_SNF            4'd3
`define DSU_DEV_TYPE_RNI            4'b0001 //1
`define DSU_DEV_TYPE_HNI            4'd4
`define DSU_DEV_TYPE_HNF            4'd5
`define DSU_DEV_TYPE_HND            4'd8
`define DSU_DEV_TYPE_PCIE_RNI       4'b0010 //2

`define DSU_DEV_TYPE_HA_RNF         4'b1111 //15
`define DSU_DEV_TYPE_HA_RNI         4'b0110 //6
`define DSU_DEV_TYPE_HA_PCIE_RNI    4'b0111 //7

`define DSU_DEV_TYPE_ROUTER         4'd9


//C2XM
`define DSU_CHI_RSP_FLIT_LOG2_WIDTH           HNF_NUM_LOG2

`define DSU_C2XM_CHI_RSP_FLIT_WIDTH \
    (`DSU_CHI_RSP_FLIT_QOS_WIDTH + `DSU_CHI_RSP_FLIT_LOG2_WIDTH + `DSU_CHI_RSP_FLIT_SRCID_WIDTH + \
     `DSU_CHI_RSP_FLIT_TXNID_WIDTH + `DSU_CHI_RSP_FLIT_OPCODE_WIDTH + `DSU_CHI_RSP_FLIT_RESPERR_WIDTH + \
     `DSU_CHI_RSP_FLIT_RESP_WIDTH + `DSU_CHI_RSP_FLIT_FWDSTATE_WIDTH + `DSU_CHI_RSP_FLIT_DBID_WIDTH +  + `DSU_CHI_RSP_FLIT_CBUSY_WIDTH + \
     `DSU_CHI_RSP_FLIT_PCRDTYPE_WIDTH +  `DSU_CHI_RSP_FLIT_TRACETAG_WIDTH + `DSU_CHI_RSP_FLIT_DEVEVENT_WIDTH + `DSU_CHI_RSP_FLIT_TAGOP_WIDTH)



//C2XM RSPDEFINE
`define C2XM_RSP_FLIT_QOS_RIGHT   0
`define C2XM_RSP_FLIT_QOS_LEFT     (`C2XM_RSP_FLIT_QOS_RIGHT + `DSU_CHI_RSP_FLIT_QOS_WIDTH - 1)
// RSPTGTID
`define C2XM_RSP_FLIT_TGTID_RIGHT     (`C2XM_RSP_FLIT_QOS_LEFT + 1)
`define C2XM_RSP_FLIT_TGTID_LEFT      (`C2XM_RSP_FLIT_TGTID_RIGHT + HNF_NUM_LOG2 - 1)


// RSPSRCID
`define C2XM_RSP_FLIT_SRCID_RIGHT  (`C2XM_RSP_FLIT_TGTID_LEFT + 1)
`define C2XM_RSP_FLIT_SRCID_LEFT   (`C2XM_RSP_FLIT_SRCID_RIGHT + `DSU_CHI_RSP_FLIT_SRCID_WIDTH - 1)

// RSPTXNID
`define C2XM_RSP_FLIT_TXNID_RIGHT (`C2XM_RSP_FLIT_SRCID_LEFT + 1)
`define C2XM_RSP_FLIT_TXNID_LEFT   (`C2XM_RSP_FLIT_TXNID_RIGHT + `DSU_CHI_RSP_FLIT_TXNID_WIDTH - 1)

// RSPOPCODE,
`define C2XM_RSP_FLIT_OPCODE_RIGHT (`C2XM_RSP_FLIT_TXNID_LEFT + 1)
`define C2XM_RSP_FLIT_OPCODE_LEFT  (`C2XM_RSP_FLIT_OPCODE_RIGHT + `DSU_CHI_RSP_FLIT_OPCODE_WIDTH - 1)

// RSPRESPERR
`define C2XM_RSP_FLIT_RESPERR_RIGHT (`C2XM_RSP_FLIT_OPCODE_LEFT + 1)
`define C2XM_RSP_FLIT_RESPERR_LEFT  (`C2XM_RSP_FLIT_RESPERR_RIGHT + `DSU_CHI_RSP_FLIT_RESPERR_WIDTH - 1)

// RSPRESP
`define C2XM_RSP_FLIT_RESP_RIGHT   (`C2XM_RSP_FLIT_RESPERR_LEFT + 1)
`define C2XM_RSP_FLIT_RESP_LEFT    (`C2XM_RSP_FLIT_RESP_RIGHT + `DSU_CHI_RSP_FLIT_RESP_WIDTH - 1)

`define C2XM_RSP_FLIT_RESPSNP_RIGHT   (`C2XM_RSP_FLIT_RESPERR_LEFT + 1)
`define C2XM_RSP_FLIT_RESPSNP_LEFT    (`C2XM_RSP_FLIT_RESPSNP_RIGHT + `DSU_CHI_RSP_FLIT_RESPSNP_WIDTH - 1)  //39:38

`define C2XM_RSP_FLIT_RESPPASSDIRTY_RIGHT   (`C2XM_RSP_FLIT_RESPSNP_LEFT + 1)
`define C2XM_RSP_FLIT_RESPPASSDIRTY_LEFT    (`C2XM_RSP_FLIT_RESPPASSDIRTY_RIGHT + `DSU_CHI_RSP_FLIT_RESPPASSDIRTY_WIDTH - 1)  //40:40

// RSPFWDSTATE
`define C2XM_RSP_FLIT_FWDSTATE_RIGHT (`C2XM_RSP_FLIT_RESP_LEFT + 1)
`define C2XM_RSP_FLIT_FWDSTATE_LEFT  (`C2XM_RSP_FLIT_FWDSTATE_RIGHT + `DSU_CHI_RSP_FLIT_FWDSTATE_WIDTH - 1)

// RSPCBUSY
`define C2XM_RSP_FLIT_CBUSY_RIGHT (`C2XM_RSP_FLIT_FWDSTATE_LEFT + 1)
`define C2XM_RSP_FLIT_CBUSY_LEFT  (`C2XM_RSP_FLIT_CBUSY_RIGHT + `DSU_CHI_RSP_FLIT_CBUSY_WIDTH - 1)

// RSPDBID
`define C2XM_RSP_FLIT_DBID_RIGHT  (`C2XM_RSP_FLIT_CBUSY_LEFT + 1)
`define C2XM_RSP_FLIT_DBID_LEFT   (`C2XM_RSP_FLIT_DBID_RIGHT + `DSU_CHI_RSP_FLIT_DBID_WIDTH - 1)

// RSPPCRDTYPE
`define C2XM_RSP_FLIT_PCRDTYPE_RIGHT   (`C2XM_RSP_FLIT_DBID_LEFT + 1)
`define C2XM_RSP_FLIT_PCRDTYPE_LEFT    (`C2XM_RSP_FLIT_PCRDTYPE_RIGHT + `DSU_CHI_RSP_FLIT_PCRDTYPE_WIDTH - 1)


// RSPTAGOP
`define C2XM_RSP_FLIT_TAGOP_RIGHT   (`C2XM_RSP_FLIT_PCRDTYPE_LEFT + 1)
`define C2XM_RSP_FLIT_TAGOP_LEFT    (`C2XM_RSP_FLIT_TAGOP_RIGHT + `DSU_CHI_RSP_FLIT_TAGOP_WIDTH - 1)

// RSPTRACETAG
`define C2XM_RSP_FLIT_TRACETAG_RIGHT (`C2XM_RSP_FLIT_TAGOP_LEFT + 1)
`define C2XM_RSP_FLIT_TRACETAG_LEFT  (`C2XM_RSP_FLIT_TRACETAG_RIGHT + `DSU_CHI_RSP_FLIT_TRACETAG_WIDTH - 1)

// RSPDEVEVENT
`define C2XM_RSP_FLIT_DEVEVENT_RIGHT (`C2XM_RSP_FLIT_TRACETAG_LEFT + 1)
`define C2XM_RSP_FLIT_DEVEVENT_LEFT  (`C2XM_RSP_FLIT_DEVEVENT_RIGHT + `DSU_CHI_RSP_FLIT_DEVEVENT_WIDTH - 1)




// RSPDBIDCHIB
//parameter C2XM_RSP_FLIT_DBID_RIGHT   =(C2XM_RSP_FLIT_FWDSTATE_LEFT + 1)
//parameter C2XM_RSP_FLIT_DBID_LEFT   = (C2XM_RSP_FLIT_DBID_RIGHT + `DSU_CHI_RSP_FLIT_DBID_WIDTH - 1)

// RSPPCRDTYPECHIB
//parameter C2XM_RSP_FLIT_PCRDTYPE_RIGHT= (C2XM_RSP_FLIT_DBID_LEFT + 1)
//parameter C2XM_RSP_FLIT_PCRDTYPE_LEFT = (C2XM_RSP_FLIT_PCRDTYPE_RIGHT + `DSU_CHI_RSP_FLIT_PCRDTYPE_WIDTH - 1)

// RSPTRACETAGCHIB
//parameter C2XM_RSP_FLIT_TRACETAG_RIGHT =(C2XM_RSP_FLIT_PCRDTYPE_LEFT + 1)
//parameter C2XM_RSP_FLIT_TRACETAG_LEFT  =(C2XM_RSP_FLIT_TRACETAG_RIGHT + `DSU_CHI_RSP_FLIT_TRACETAG_WIDTH - 1)


//AXCTLBUS     
`define DSU_C2XM_CHI_AX4_AXCTLBUS_WIDTH \
    (`DSU_CHI_REQ_FLIT_ADDR_WIDTH + `DSU_CHI_REQ_FLIT_RSVD_WIDTH + \
     `DSU_AX4_ARLEN_WIDTH + `DSU_AX4_ARSIZE_WIDTH + \
     `DSU_AX4_ARBURST_WIDTH + `DSU_CHI_REQ_FLIT_QOS_WIDTH + \
     `DSU_CHI_REQ_FLIT_TRACETAG_WIDTH + `DSU_AX4_ARCACHE_WIDTH + \
     `DSU_AX4_ARPROT_WIDTH)

//AWFIFO 

`define DSU_C2XM_AWFIFO_WIDTH     `DSU_C2XM_CHI_AX4_AXCTLBUS_WIDTH + `DSU_AX4_AWID_WIDTH

`define DSU_C2XM_AWFIFO_ADDR_RIGHT   0
`define DSU_C2XM_AWFIFO_ADDR_LEFT    `DSU_AX4_ARADDR_WIDTH - 1
`define DSU_C2XM_AWFIFO_ADDR_RANGE  `DSU_C2XM_AWFIFO_ADDR_LEFT:`DSU_C2XM_AWFIFO_ADDR_RIGHT

`define DSU_C2XM_AWFIFO_AWUSER_RIGHT  `DSU_C2XM_AWFIFO_ADDR_LEFT + 1
`define DSU_C2XM_AWFIFO_AWUSER_LEFT   `DSU_C2XM_AWFIFO_AWUSER_RIGHT + `DSU_CHI_REQ_FLIT_RSVD_WIDTH - 1
`define DSU_C2XM_AWFIFO_AWUSER_RANGE  `DSU_C2XM_AWFIFO_AWUSER_LEFT:`DSU_C2XM_AWFIFO_AWUSER_RIGHT

`define DSU_C2XM_AWFIFO_AWLEN_RIGHT   `DSU_C2XM_AWFIFO_AWUSER_LEFT + 1
`define DSU_C2XM_AWFIFO_AWLEN_LEFT    `DSU_C2XM_AWFIFO_AWLEN_RIGHT +  `DSU_AX4_ARLEN_WIDTH - 1
`define DSU_C2XM_AWFIFO_AWLEN_RANGE   `DSU_C2XM_AWFIFO_AWLEN_LEFT:`DSU_C2XM_AWFIFO_AWLEN_RIGHT

`define DSU_C2XM_AWFIFO_AWSIZE_RIGHT  `DSU_C2XM_AWFIFO_AWLEN_LEFT  + 1
`define DSU_C2XM_AWFIFO_AWSIZE_LEFT   `DSU_C2XM_AWFIFO_AWSIZE_RIGHT + `DSU_AX4_ARSIZE_WIDTH - 1
`define DSU_C2XM_AWFIFO_AWSIZE_RANGE  `DSU_C2XM_AWFIFO_AWSIZE_LEFT:`DSU_C2XM_AWFIFO_AWSIZE_RIGHT

`define DSU_C2XM_AWFIFO_AWBURST_RIGHT  `DSU_C2XM_AWFIFO_AWSIZE_LEFT + 1
`define DSU_C2XM_AWFIFO_AWBURST_LEFT   `DSU_C2XM_AWFIFO_AWBURST_RIGHT + `DSU_AX4_ARBURST_WIDTH - 1
`define DSU_C2XM_AWFIFO_AWBURST_RANGE  `DSU_C2XM_AWFIFO_AWBURST_LEFT:`DSU_C2XM_AWFIFO_AWBURST_RIGHT

`define DSU_C2XM_AWFIFO_AWQOS_RIGHT   `DSU_C2XM_AWFIFO_AWBURST_LEFT + 1 
`define DSU_C2XM_AWFIFO_AWQOS_LEFT    `DSU_C2XM_AWFIFO_AWQOS_RIGHT +  `DSU_CHI_REQ_FLIT_QOS_WIDTH - 1
`define DSU_C2XM_AWFIFO_AWQOS_RANGE   `DSU_C2XM_AWFIFO_AWQOS_LEFT:`DSU_C2XM_AWFIFO_AWQOS_RIGHT

`define DSU_C2XM_AWFIFO_TT_RIGHT       `DSU_C2XM_AWFIFO_AWQOS_LEFT +  1
`define DSU_C2XM_AWFIFO_TT_LEFT        `DSU_C2XM_AWFIFO_TT_RIGHT +  `DSU_CHI_REQ_FLIT_TRACETAG_WIDTH - 1
`define DSU_C2XM_AWFIFO_TT_RANGE       `DSU_C2XM_AWFIFO_TT_LEFT:`DSU_C2XM_AWFIFO_TT_RIGHT

`define DSU_C2XM_AWFIFO_CACHE_RIGHT    `DSU_C2XM_AWFIFO_TT_LEFT +  1
`define DSU_C2XM_AWFIFO_CACHE_LEFT     `DSU_C2XM_AWFIFO_CACHE_RIGHT +  `DSU_AX4_ARCACHE_WIDTH - 1
`define DSU_C2XM_AWFIFO_CACHE_RANGE    `DSU_C2XM_AWFIFO_CACHE_LEFT:`DSU_C2XM_AWFIFO_CACHE_RIGHT

`define DSU_C2XM_AWFIFO_PROT_RIGHT     `DSU_C2XM_AWFIFO_CACHE_LEFT +  1
`define DSU_C2XM_AWFIFO_PROT_LEFT      `DSU_C2XM_AWFIFO_PROT_RIGHT + `DSU_AX4_ARPROT_WIDTH - 1
`define DSU_C2XM_AWFIFO_PROT_RANGE     `DSU_C2XM_AWFIFO_PROT_LEFT:`DSU_C2XM_AWFIFO_PROT_RIGHT

`define DSU_C2XM_AWFIFO_AWID_RIGHT       `DSU_C2XM_AWFIFO_PROT_LEFT + 1
`define DSU_C2XM_AWFIFO_AWID_LEFT        `DSU_C2XM_AWFIFO_AWID_RIGHT + `DSU_AX4_AWID_WIDTH - 1
`define DSU_C2XM_AWFIFO_AWID_RANGE       `DSU_C2XM_AWFIFO_AWID_LEFT:`DSU_C2XM_AWFIFO_AWID_RIGHT


//RetryAck/ReadReceipt FIFO Data
  
`define DSU_C2XM_RTYACK_NEEDRACK_WIDTH   1

`define DSU_C2XM_RTYACK_RDRCPT_FIFO_WIDTH \
    (`DSU_CHI_REQ_FLIT_QOS_WIDTH + HNF_NUM_LOG2 + \
     `DSU_CHI_REQ_FLIT_TXNID_WIDTH + `DSU_CHI_REQ_FLIT_TRACETAG_WIDTH + `DSU_C2XM_RTYACK_NEEDRACK_WIDTH)

`define DSU_C2XM_RTYACK_TXNID_RIGHT (`DSU_CHI_REQ_FLIT_TGTID_LEFT + 1)
`define DSU_C2XM_RTYACK_TXNID_LEFT  (`DSU_C2XM_RTYACK_TXNID_RIGHT + `DSU_CHI_REQ_FLIT_TXNID_WIDTH - 1)

`define DSU_C2XM_RTYACK_TRACETAG_RIGHT (`DSU_C2XM_RTYACK_TXNID_LEFT + 1)
`define DSU_C2XM_RTYACK_TRACETAG_LEFT  (`DSU_C2XM_RTYACK_TRACETAG_RIGHT + `DSU_CHI_REQ_FLIT_TRACETAG_WIDTH - 1)

`define DSU_C2XM_RTYACK_NEEDRACK_RIGHT   (`DSU_C2XM_RTYACK_TRACETAG_LEFT + 1)
`define DSU_C2XM_RTYACK_NEEDRACK_LEFT    (`DSU_C2XM_RTYACK_NEEDRACK_RIGHT + `DSU_C2XM_RTYACK_NEEDRACK_WIDTH - 1)

`define DSU_C2XM_RTYACK_TXNID_RANGE       `DSU_C2XM_RTYACK_TXNID_LEFT:`DSU_C2XM_RTYACK_TXNID_RIGHT
`define DSU_C2XM_RTYACK_NEEDRACK_RANGE    `DSU_C2XM_RTYACK_NEEDRACK_LEFT:`DSU_C2XM_RTYACK_NEEDRACK_RIGHT
`define DSU_C2XM_RTYACK_TRACETAG_RANGE    `DSU_C2XM_RTYACK_TRACETAG_LEFT:`DSU_C2XM_RTYACK_TRACETAG_RIGHT


`define  DSU_C2XM_QUE_NUM           32
`define  DSU_C2XM_QUE_NUM_WIDTH     5         
`define  DSU_C2XM_WBUF_DEPTH        8         
`define  DSU_C2XM_WBUF_DEPTH_WIDTH  3         

//ARFIFO
`define DSU_C2XM_ARFIFO_WIDTH \
     (`DSU_AX4_ARADDR_WIDTH + `DSU_AX4_ARSIZE_WIDTH + `DSU_AX4_ARBURST_WIDTH + \
	`DSU_AX4_ARCACHE_WIDTH + `DSU_AX4_ARPROT_WIDTH + `DSU_AX4_ARLEN_WIDTH + \
	`DSU_AX4_ARLOCK_WIDTH + `DSU_AX4_ARQOS_WIDTH + `DSU_AX4_ARID_WIDTH)

//RFIFO
`define DSU_C2XM_RFIFO_WIDTH \
      (`DSU_AX4_RID_WIDTH + `DSU_AX4_RLAST_WIDTH + `DSU_AX4_RUSER_WIDTH + \
	`DSU_AX4_RRESP_WIDTH + `DSU_AX4_RTRACE_WIDTH + `DSU_AX4_RPOISON_WIDTH + \
	`DSU_AX4_RDATA_WIDTH + `DSU_AX4_RDATACHK_WIDTH)

//WFIFO
`define DSU_C2XM_WFIFO_WIDTH \
    (`DSU_AX4_RDATA_WIDTH/8 + `DSU_AX4_RDATA_WIDTH/64 + 1 + `DSU_AX4_RDATA_WIDTH/8 + `DSU_AX4_RDATA_WIDTH)

//BFIFO
`define DSU_C2XM_BFIFO_WIDTH \
    (`DSU_AX4_BRESP_WIDTH + `DSU_AX4_BID_WIDTH)

//C2XM data flit
`define  DSU_C2XM_DAT_FLIT_WIDTH \
    (`DSU_CHI_DAT_FLIT_QOS_WIDTH + `DSU_CHI_DAT_FLIT_TGTID_WIDTH + `DSU_CHI_DAT_FLIT_SRCID_WIDTH + \
     `DSU_CHI_DAT_FLIT_TXNID_WIDTH + `DSU_CHI_DAT_FLIT_HNID_WIDTH + `DSU_CHI_DAT_FLIT_OPCODE_WIDTH + \
     `DSU_CHI_DAT_FLIT_RESPERR_WIDTH + `DSU_CHI_DAT_FLIT_RESP_WIDTH + `DSU_CHI_DAT_FLIT_FWDSTATE_WIDTH + \
     `DSU_CHI_DAT_FLIT_DBID_WIDTH + `DSU_CHI_DAT_FLIT_CCID_WIDTH + `DSU_CHI_DAT_FLIT_DATAID_WIDTH + \
     `DSU_CHI_DAT_FLIT_TRACETAG_WIDTH + `DSU_CHI_DAT_FLIT_RSVD_WIDTH + `DSU_CHI_DAT_FLIT_BE_WIDTH + \
     `DSU_CHI_DAT_FLIT_DATA_WIDTH + `DSU_CHI_DAT_FLIT_POISON_WIDTH + `DSU_CHI_DAT_FLIT_CHUNKV_WIDTH + `DSU_CHI_DAT_FLIT_DEVEVENT_WIDTH)


`define  DSU_C2XM_CHUNK_WIDTH          128
`define  DSU_C2XM_REQ_LSRCID           7
`define  DSU_C2XM_REQ_LSRCID_RETRY     7
`define  DSU_C2XM_REQ_CCID               2
`define  DSU_C2XM_RQUE_SIZE             64
`define  DSU_C2XM_DEQ_SIZE              32

//log2
`define FUNC_LOG2(x)            ( ((x) <= ('d1<<'d01)) ? 'd01   \
                                : ((x) <= ('d1<<'d02)) ? 'd02   \
                                : ((x) <= ('d1<<'d03)) ? 'd03   \
                                : ((x) <= ('d1<<'d04)) ? 'd04   \
                                : ((x) <= ('d1<<'d05)) ? 'd05   \
                                : ((x) <= ('d1<<'d06)) ? 'd06   \
                                : ((x) <= ('d1<<'d07)) ? 'd07   \
                                : ((x) <= ('d1<<'d08)) ? 'd08   \
                                : ((x) <= ('d1<<'d09)) ? 'd09   \
                                : ((x) <= ('d1<<'d10)) ? 'd10   \
                                : ((x) <= ('d1<<'d11)) ? 'd11   \
                                : ((x) <= ('d1<<'d12)) ? 'd12   \
                                : ((x) <= ('d1<<'d13)) ? 'd13   \
                                : ((x) <= ('d1<<'d14)) ? 'd14   \
                                : ((x) <= ('d1<<'d15)) ? 'd15   \
                                : ((x) <= ('d1<<'d16)) ? 'd16   \
                                : ((x) <= ('d1<<'d17)) ? 'd17   \
                                : ((x) <= ('d1<<'d18)) ? 'd18   \
                                : ((x) <= ('d1<<'d19)) ? 'd19   \
                                : ((x) <= ('d1<<'d20)) ? 'd20   \
                                : ((x) <= ('d1<<'d21)) ? 'd21   \
                                : ((x) <= ('d1<<'d22)) ? 'd22   \
                                : ((x) <= ('d1<<'d23)) ? 'd23   \
                                : ((x) <= ('d1<<'d24)) ? 'd24   \
                                : ((x) <= ('d1<<'d25)) ? 'd25   \
                                : ((x) <= ('d1<<'d26)) ? 'd26   \
                                : ((x) <= ('d1<<'d27)) ? 'd27   \
                                : ((x) <= ('d1<<'d28)) ? 'd28   \
                                : ((x) <= ('d1<<'d39)) ? 'd29   \
                                : ((x) <= ('d1<<'d30)) ? 'd30   \
                                : ((x) <= ('d1<<'d31)) ? 'd31   \
                                : 'd0                           )

//DLB share
`define  DSU_DLB_NEXT_WIDTH            1

`define  DSU_DLB_FLIT_WIDTH \
         (`DSU_DLB_NEXT_WIDTH + `DSU_DLB_MESH_BRDCST_WIDTH + `DSU_DLB_MESH_XID_WIDTH + `DSU_DLB_MESH_YID_WIDTH +  \
          `DSU_DLB_MESH_DEVICEID_WIDTH + `DSU_DLB_MESH_PORTID_WIDTH + `DSU_DLB_MESH_TODEV_WIDTH + `DSU_DLB_MESH_OPCODE_WIDTH)

`define  DSU_DLB_NEXT_RIGHT            (`DSU_DLB_NEXT_LEFT - `DSU_DLB_NEXT_WIDTH + 1)
`define  DSU_DLB_NEXT_LEFT             (`DSU_DLB_FLIT_WIDTH-1)

`define  DSU_DLB_NEXT_RANGE            `DSU_DLB_NEXT_LEFT : `DSU_DLB_NEXT_RIGHT

`ifdef DSU_TOP_DLB_ENC_DEVID
    `define DSU_DLB_ENC_DEVID           `DSU_TOP_DLB_ENC_DEVID
`else                                  //if router (1 not router, 0 router itself)   port ID (P0/P1/P2/P3)     device ID
    `define DSU_DLB_ENC_DEVID         {1'b1,                                         2'd0,                     2'd0}
`endif

`ifdef DSU_TOP_DLB_DEC_LIST         //indicate existed dlb_decoder
    `define DSU_DLB_DEC_LIST           `DSU_TOP_DLB_DEC_LIST
`else                           //              P3    P2   P1   P0                                                                       
    `define DSU_DLB_DEC_LIST   /*MXP 2_1*/ {16'b0000_0000_0000_0000,\
                               /*MXP 2_0*/  16'b0000_0000_0000_0011,\
                               /*MXP 1_1*/  16'b0000_0000_0000_0100,\
                               /*MXP 1_0*/  16'b0000_0000_0000_0000,\
                               /*MXP 0_1*/  16'b0000_0000_1100_0000,\
                               /*MXP 0_0*/  16'b0000_0000_0000_0000}     
`endif                          //             d3210_3210_3210_3210


`define DSU_DLB_BASEADDR_WIDTH         (`DSU_DLB_MESH_TGTID_WIDTH + `DSU_DLB_ADDR_REG_ADDR_WIDTH)





//`define DSU_MXP_PORTID_WIDTH_1   //def this macro if a router can only have up to 2 ports

`ifdef DSU_MXP_PORTID_WIDTH_1
    `define DSU_DLB_DEC_LOCAL_LIST_WIDTH      8
`else
    `define DSU_DLB_DEC_LOCAL_LIST_WIDTH      16
`endif

`ifdef DSU_TOP_MAX_X_NUM                                       
    `define DSU_MAX_X_NUM           `DSU_TOP_MAX_X_NUM
`else                                                                                          
    `define DSU_MAX_X_NUM            3
`endif                                                       

`ifdef DSU_TOP_MAX_Y_NUM                                       
    `define DSU_MAX_Y_NUM           `DSU_TOP_MAX_Y_NUM
`else                                                                                          
    `define DSU_MAX_Y_NUM            2
`endif   

`ifdef DSU_ARCH_CROSS
    `define DSU_ROUTER_NUM          11
`else                                                                                          
    `define DSU_ROUTER_NUM          `DSU_MAX_X_NUM*`DSU_MAX_Y_NUM
`endif   

//DLB mesh     
//`define  DSU_DLB_MESH_LOCAL_WIDTH      1           
`define  DSU_DLB_MESH_OPCODE_WIDTH     3            
`define  DSU_DLB_MESH_DEVICEID_WIDTH   2
`define  DSU_DLB_MESH_PORTID_WIDTH     2
`define  DSU_DLB_MESH_TODEV_WIDTH      1      
`define  DSU_DLB_MESH_YID_WIDTH        3         
`define  DSU_DLB_MESH_XID_WIDTH        4
`define  DSU_DLB_MESH_BRDCST_WIDTH     1 
`define  DSU_DLB_MESH_DEVID_WIDTH      (`DSU_DLB_MESH_DEVICEID_WIDTH + `DSU_DLB_MESH_PORTID_WIDTH + `DSU_DLB_MESH_TODEV_WIDTH)
`define  DSU_DLB_MESH_TGTID_WIDTH      (`DSU_DLB_MESH_DEVICEID_WIDTH + `DSU_DLB_MESH_PORTID_WIDTH + `DSU_DLB_MESH_TODEV_WIDTH + `DSU_DLB_MESH_YID_WIDTH + `DSU_DLB_MESH_XID_WIDTH)

//`define  DSU_DLB_MESH_LOCAL_RIGHT      0
//`define  DSU_DLB_MESH_LOCAL_LEFT       (`DSU_DLB_MESH_LOCAL_RIGHT+`DSU_DLB_MESH_LOCAL_WIDTH-1)

//`define  DSU_DLB_MESH_OPCODE_RIGHT     (`DSU_DLB_MESH_LOCAL_LEFT+1)
`define  DSU_DLB_MESH_OPCODE_RIGHT     0
`define  DSU_DLB_MESH_OPCODE_LEFT      (`DSU_DLB_MESH_OPCODE_RIGHT+`DSU_DLB_MESH_OPCODE_WIDTH-1)

`define  DSU_DLB_MESH_DEVICEID_RIGHT   (`DSU_DLB_MESH_OPCODE_LEFT+1)
`define  DSU_DLB_MESH_DEVICEID_LEFT    (`DSU_DLB_MESH_DEVICEID_RIGHT+`DSU_DLB_MESH_DEVICEID_WIDTH-1)

`define  DSU_DLB_MESH_PORTID_RIGHT     (`DSU_DLB_MESH_DEVICEID_LEFT+1)
`define  DSU_DLB_MESH_PORTID_LEFT      (`DSU_DLB_MESH_PORTID_RIGHT+`DSU_DLB_MESH_PORTID_WIDTH-1)

`define  DSU_DLB_MESH_TODEV_RIGHT      (`DSU_DLB_MESH_PORTID_LEFT+1)
`define  DSU_DLB_MESH_TODEV_LEFT       (`DSU_DLB_MESH_TODEV_RIGHT+`DSU_DLB_MESH_TODEV_WIDTH-1)

`define  DSU_DLB_MESH_YID_RIGHT        (`DSU_DLB_MESH_TODEV_LEFT+1)
`define  DSU_DLB_MESH_YID_LEFT         (`DSU_DLB_MESH_YID_RIGHT+`DSU_DLB_MESH_YID_WIDTH-1)

`define  DSU_DLB_MESH_XID_RIGHT        (`DSU_DLB_MESH_YID_LEFT+1)
`define  DSU_DLB_MESH_XID_LEFT         (`DSU_DLB_MESH_XID_RIGHT+`DSU_DLB_MESH_XID_WIDTH-1)

`define  DSU_DLB_MESH_BRDCST_RIGHT       (`DSU_DLB_MESH_XID_LEFT+1)
`define  DSU_DLB_MESH_BRDCST_LEFT        (`DSU_DLB_MESH_BRDCST_RIGHT+`DSU_DLB_MESH_BRDCST_WIDTH-1)
  
//`define  DSU_DLB_MESH_LOCAL_RANGE      `DSU_DLB_MESH_LOCAL_LEFT:`DSU_DLB_MESH_LOCAL_RIGHT         
`define  DSU_DLB_MESH_OPCODE_RANGE     `DSU_DLB_MESH_OPCODE_LEFT:`DSU_DLB_MESH_OPCODE_RIGHT          
`define  DSU_DLB_MESH_DEVICEID_RANGE   `DSU_DLB_MESH_DEVICEID_LEFT:`DSU_DLB_MESH_DEVICEID_RIGHT 
`define  DSU_DLB_MESH_PORTID_RANGE     `DSU_DLB_MESH_PORTID_LEFT:`DSU_DLB_MESH_PORTID_RIGHT
`define  DSU_DLB_MESH_TODEV_RANGE      `DSU_DLB_MESH_TODEV_LEFT:`DSU_DLB_MESH_TODEV_RIGHT
`define  DSU_DLB_MESH_YID_RANGE        `DSU_DLB_MESH_YID_LEFT:`DSU_DLB_MESH_YID_RIGHT       
`define  DSU_DLB_MESH_XID_RANGE        `DSU_DLB_MESH_XID_LEFT:`DSU_DLB_MESH_XID_RIGHT
`define  DSU_DLB_MESH_TGTID_RANGE      `DSU_DLB_MESH_XID_LEFT:`DSU_DLB_MESH_DEVICEID_RIGHT  
`define  DSU_DLB_MESH_BRDCST_RANGE     `DSU_DLB_MESH_BRDCST_LEFT:`DSU_DLB_MESH_BRDCST_RIGHT

//DLB addr     
`define  DSU_DLB_ADDR_REG_ADDR_WIDTH   14    
`define  DSU_DLB_ADDR_RSVD_WIDTH       2

`define  DSU_DLB_ADDR_REG_ADDR_RIGHT   0 
`define  DSU_DLB_ADDR_REG_ADDR_LEFT    (`DSU_DLB_ADDR_REG_ADDR_RIGHT + `DSU_DLB_ADDR_REG_ADDR_WIDTH-1)

`define  DSU_DLB_ADDR_RSVD_RIGHT       (`DSU_DLB_ADDR_REG_ADDR_LEFT+1)
`define  DSU_DLB_ADDR_RSVD_LEFT        (`DSU_DLB_ADDR_RSVD_RIGHT + `DSU_DLB_ADDR_RSVD_WIDTH-1)

`define  DSU_DLB_ADDR_RSVD_RANGE       `DSU_DLB_ADDR_RSVD_LEFT:`DSU_DLB_ADDR_RSVD_RIGHT 
`define  DSU_DLB_ADDR_REG_ADDR_RANGE   `DSU_DLB_ADDR_REG_ADDR_LEFT:`DSU_DLB_ADDR_REG_ADDR_RIGHT     

//DLB data
`define  DSU_DLB_DATA_LOW_WIDTH        16
`define  DSU_DLB_DATA_HIGH_WIDTH       16
`define  DSU_DLB_DATA_BUS_WIDTH        (`DSU_DLB_DATA_LOW_WIDTH+`DSU_DLB_DATA_HIGH_WIDTH)

`define  DSU_DLB_DATA_LOW_RIGHT        0
`define  DSU_DLB_DATA_LOW_LEFT         (`DSU_DLB_DATA_LOW_RIGHT+`DSU_DLB_DATA_LOW_WIDTH-1)        

`define  DSU_DLB_DATA_LOW_RANGE        `DSU_DLB_DATA_LOW_LEFT:`DSU_DLB_DATA_LOW_RIGHT

`define  DSU_DLB_DATA_HIGH_RIGHT       0
`define  DSU_DLB_DATA_HIGH_LEFT        (`DSU_DLB_DATA_HIGH_RIGHT+`DSU_DLB_DATA_HIGH_WIDTH-1)        

`define  DSU_DLB_DATA_HIGH_RANGE       `DSU_DLB_DATA_HIGH_LEFT:`DSU_DLB_DATA_HIGH_RIGHT

//DLB intr
`define  DSU_DLB_INTR_ERRINFO_WIDTH    6
`define  DSU_DLB_INTR_DEVTYPE_WIDTH    `DSU_DEV_TYPE_WIDTH
`define  DSU_DLB_INTR_LGID_WIDTH       `DSU_LOGICAL_ID_WIDTH

`define  DSU_DLB_INTR_ERRINFO_RIGHT    0
`define  DSU_DLB_INTR_ERRINFO_LEFT     (`DSU_DLB_INTR_ERRINFO_RIGHT+`DSU_DLB_INTR_ERRINFO_WIDTH-1)
`define  DSU_DLB_INTR_ERRINFO_RANGE    `DSU_DLB_INTR_ERRINFO_LEFT:`DSU_DLB_INTR_ERRINFO_RIGHT

`define  DSU_DLB_INTR_DEVTYPE_RIGHT    (`DSU_DLB_INTR_ERRINFO_LEFT+1)
`define  DSU_DLB_INTR_DEVTYPE_LEFT     (`DSU_DLB_INTR_DEVTYPE_RIGHT+`DSU_DLB_INTR_DEVTYPE_WIDTH-1)
`define  DSU_DLB_INTR_DEVTYPE_RANGE    `DSU_DLB_INTR_DEVTYPE_LEFT:`DSU_DLB_INTR_DEVTYPE_RIGHT

`define  DSU_DLB_INTR_LGID_RIGHT       (`DSU_DLB_INTR_DEVTYPE_LEFT+1)
`define  DSU_DLB_INTR_LGID_LEFT        (`DSU_DLB_INTR_LGID_RIGHT+`DSU_DLB_INTR_LGID_WIDTH-1)
`define  DSU_DLB_INTR_LGID_RANGE       `DSU_DLB_INTR_LGID_LEFT:`DSU_DLB_INTR_LGID_RIGHT

//DRB
`define  DSU_DRB_ADDR_WIDTH            `DSU_DLB_ADDR_REG_ADDR_WIDTH
`define  DSU_DRB_DATA_WIDTH            `DSU_DLB_DATA_BUS_WIDTH

//PSA
`define  DSU_PSACTRL_CMD_WIDTH         4
`define  DSU_PSACTRL_VLD_WIDTH         1
`define  DSU_PSACTRL_WIDTH             (`DSU_PSACTRL_CMD_WIDTH + `DSU_PSACTRL_VLD_WIDTH)

`define  DSU_PSACTRL_CMD_RIGHT         0
`define  DSU_PSACTRL_CMD_LEFT          (`DSU_PSACTRL_CMD_RIGHT+`DSU_PSACTRL_CMD_WIDTH-1)
`define  DSU_PSACTRL_CMD_RANGE         `DSU_DLB_INTR_ERRINFO_LEFT:`DSU_PSACTRL_CMD_RIGHT

`define  DSU_PSACTRL_VLD_RIGHT         (`DSU_PSACTRL_CMD_LEFT+1)          
`define  DSU_PSACTRL_VLD_LEFT          (`DSU_PSACTRL_VLD_RIGHT+`DSU_PSACTRL_VLD_WIDTH-1)
`define  DSU_PSACTRL_VLD_RANGE         `DSU_PSACTRL_VLD_LEFT:`DSU_PSACTRL_VLD_RIGHT

`define  DSU_PSACTRL_CMD_IDLE          4'b0000  
`define  DSU_PSACTRL_CMD_SS            4'b0001 
`define  DSU_PSACTRL_CMD_FLUSH         4'b0010 
`define  DSU_PSACTRL_CMD_EN            4'b0011 
`define  DSU_PSACTRL_CMD_DISABLE       4'b0100

`define  DSU_DUAL_LANE_REQ_EN   0
`define  DSU_DUAL_LANE_RSP_EN   1
`define  DSU_DUAL_LANE_SNP_EN   0
`define  DSU_DUAL_LANE_DAT_EN   1
