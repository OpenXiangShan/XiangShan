module addr_mapper (
   `axi_slave_if(s_axi, 64, 8),
   `axi_master_if(m_axi, 64, 8)
);

    assign  m_axi_awaddr = {4'd0, 2'b01, s_axi_awaddr[29:0]};
    assign  m_axi_araddr = {4'd0, 2'b01, s_axi_araddr[29:0]};
    assign  m_axi_arburst = s_axi_arburst;
    assign  m_axi_arcache = s_axi_arcache;
    assign  m_axi_arid    = s_axi_arid   ;
//    assign  m_axi_aruser  = s_axi_aruser ;
    assign  m_axi_arlen   = s_axi_arlen  ;
    assign  m_axi_arlock  = s_axi_arlock ;
    assign  m_axi_arprot  = s_axi_arprot ;
    assign  s_axi_arready = m_axi_arready;
    assign  m_axi_arsize  = s_axi_arsize ;
    assign  m_axi_arvalid = s_axi_arvalid;
    assign  m_axi_awburst = s_axi_awburst;
    assign  m_axi_awcache = s_axi_awcache;
    assign  m_axi_awid    = s_axi_awid   ;
//    assign  m_axi_awuser  = s_axi_awuser ;
    assign  m_axi_awlen   = s_axi_awlen  ;
    assign  m_axi_awlock  = s_axi_awlock ;
    assign  m_axi_awprot  = s_axi_awprot ;
    assign  s_axi_awready = m_axi_awready;
    assign  m_axi_awsize  = s_axi_awsize ;
    assign  m_axi_awvalid = s_axi_awvalid;
    assign  s_axi_bid     = m_axi_bid    ;
    assign  m_axi_bready  = s_axi_bready ;
    assign  s_axi_bresp   = m_axi_bresp  ;
    assign  s_axi_bvalid  = m_axi_bvalid ;
    assign  s_axi_rdata   = m_axi_rdata  ;
    assign  s_axi_rid     = m_axi_rid    ;
    assign  s_axi_rlast   = m_axi_rlast  ;
    assign  m_axi_rready  = s_axi_rready ;
    assign  s_axi_rresp   = m_axi_rresp  ;
    assign  s_axi_rvalid  = m_axi_rvalid ;
    assign  m_axi_wdata   = s_axi_wdata  ;
    assign  m_axi_wlast   = s_axi_wlast  ;
    assign  s_axi_wready  = m_axi_wready ;
    assign  m_axi_wstrb   = s_axi_wstrb  ;
    assign  m_axi_wvalid  = s_axi_wvalid ;
    assign  m_axi_arqos   = s_axi_arqos  ;
    assign  m_axi_awqos   = s_axi_awqos  ;

endmodule
