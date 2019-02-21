`define axi_field(in_out, width, prefix, field) \
  in_out [width - 1: 0] prefix``_``field

`define axi_wire_field(width, prefix, field) \
  wire [width - 1: 0] prefix``_``field

`define axi_connect_field(io_prefix, wire_prefix, field) \
	.io_prefix``_``field(wire_prefix``_``field)



`define axilite_if(dir1, dir2, prefix, data_width) \
	`axi_field(dir1, 1, prefix, awready), \
	`axi_field(dir2, 1, prefix, awvalid), \
	`axi_field(dir2, 40, prefix, awaddr), \
	`axi_field(dir2, 3, prefix, awprot), \
	`axi_field(dir1, 1, prefix, wready), \
	`axi_field(dir2, 1, prefix, wvalid), \
	`axi_field(dir2, data_width, prefix, wdata), \
	`axi_field(dir2, (data_width / 8), prefix, wstrb), \
	`axi_field(dir2, 1, prefix, bready), \
	`axi_field(dir1, 1, prefix, bvalid), \
	`axi_field(dir1, 2, prefix, bresp), \
	`axi_field(dir1, 1, prefix, arready), \
	`axi_field(dir2, 1, prefix, arvalid), \
	`axi_field(dir2, 40, prefix, araddr), \
	`axi_field(dir2, 3, prefix, arprot), \
	`axi_field(dir2, 1, prefix, rready), \
	`axi_field(dir1, 1, prefix, rvalid), \
	`axi_field(dir1, 2, prefix, rresp), \
	`axi_field(dir1, data_width, prefix, rdata)

`define axilite_master_if(prefix) \
	`axilite_if(input, output, prefix, 32)

`define axilite_slave_if(prefix) \
	`axilite_if(output, input, prefix, 32)


`define axilite_wire_internal(prefix, data_width) \
	`axi_wire_field(1, prefix, awready); \
	`axi_wire_field(1, prefix, awvalid); \
	`axi_wire_field(40, prefix, awaddr); \
	`axi_wire_field(3, prefix, awprot); \
	`axi_wire_field(1, prefix, wready); \
	`axi_wire_field(1, prefix, wvalid); \
	`axi_wire_field(data_width, prefix, wdata); \
	`axi_wire_field((data_width / 8), prefix, wstrb); \
	`axi_wire_field(1, prefix, bready); \
	`axi_wire_field(1, prefix, bvalid); \
	`axi_wire_field(2, prefix, bresp); \
	`axi_wire_field(1, prefix, arready); \
	`axi_wire_field(1, prefix, arvalid); \
	`axi_wire_field(40, prefix, araddr); \
	`axi_wire_field(3, prefix, arprot); \
	`axi_wire_field(1, prefix, rready); \
	`axi_wire_field(1, prefix, rvalid); \
	`axi_wire_field(2, prefix, rresp); \
	`axi_wire_field(data_width, prefix, rdata)

`define axilite_wire(prefix) \
    `axilite_wire_internal(prefix, 32)

`define axilite_connect_if(io_prefix, wire_prefix) \
	`axi_connect_field(io_prefix, wire_prefix, awready), \
	`axi_connect_field(io_prefix, wire_prefix, awvalid), \
	`axi_connect_field(io_prefix, wire_prefix, awaddr), \
	`axi_connect_field(io_prefix, wire_prefix, awprot), \
	`axi_connect_field(io_prefix, wire_prefix, wready), \
	`axi_connect_field(io_prefix, wire_prefix, wvalid), \
	`axi_connect_field(io_prefix, wire_prefix, wdata), \
	`axi_connect_field(io_prefix, wire_prefix, wstrb), \
	`axi_connect_field(io_prefix, wire_prefix, bready), \
	`axi_connect_field(io_prefix, wire_prefix, bvalid), \
	`axi_connect_field(io_prefix, wire_prefix, bresp), \
	`axi_connect_field(io_prefix, wire_prefix, arready), \
	`axi_connect_field(io_prefix, wire_prefix, arvalid), \
	`axi_connect_field(io_prefix, wire_prefix, araddr), \
	`axi_connect_field(io_prefix, wire_prefix, arprot), \
	`axi_connect_field(io_prefix, wire_prefix, rready), \
	`axi_connect_field(io_prefix, wire_prefix, rvalid), \
	`axi_connect_field(io_prefix, wire_prefix, rresp), \
	`axi_connect_field(io_prefix, wire_prefix, rdata)


`define axi_if(dir1, dir2, prefix, data_width, id_width) \
  (* X_INTERFACE_PARAMETER = `"ID_WIDTH id_width`" *) \
	`axilite_if(dir1, dir2, prefix, data_width), \
	`axi_field(dir2, 8, prefix, awlen), \
	`axi_field(dir2, 3, prefix, awsize), \
	`axi_field(dir2, 2, prefix, awburst), \
	`axi_field(dir2, 1, prefix, awlock), \
	`axi_field(dir2, 4, prefix, awcache), \
	`axi_field(dir2, 4, prefix, awqos), \
	`axi_field(dir2, id_width, prefix, awid), \
	/* `axi_field(dir2, 6, prefix, awuser), */ \
	`axi_field(dir2, 1, prefix, wlast), \
	`axi_field(dir1, id_width, prefix, bid), \
	`axi_field(dir2, 8, prefix, arlen), \
	`axi_field(dir2, 3, prefix, arsize), \
	`axi_field(dir2, 2, prefix, arburst), \
	`axi_field(dir2, 1, prefix, arlock), \
	`axi_field(dir2, 4, prefix, arcache), \
	`axi_field(dir2, 4, prefix, arqos), \
	`axi_field(dir2, id_width, prefix, arid), \
	/* `axi_field(dir2, 6, prefix, aruser), */ \
	`axi_field(dir1, 1, prefix, rlast), \
	`axi_field(dir1, id_width, prefix, rid)

`define axi_master_if(prefix, data_width, id_width) \
	`axi_if(input, output, prefix, data_width, id_width)

`define axi_slave_if(prefix, data_width, id_width) \
	`axi_if(output, input, prefix, data_width, id_width)

`define axi_wire(prefix, data_width, id_width) \
	`axilite_wire_internal(prefix, data_width); \
	`axi_wire_field(8, prefix, awlen); \
	`axi_wire_field(3, prefix, awsize); \
	`axi_wire_field(2, prefix, awburst); \
	`axi_wire_field(1, prefix, awlock); \
	`axi_wire_field(4, prefix, awcache); \
	`axi_wire_field(4, prefix, awqos); \
	`axi_wire_field(id_width, prefix, awid); \
	/* `axi_wire_field(6, prefix, awuser); */ \
	`axi_wire_field(1, prefix, wlast); \
	`axi_wire_field(id_width, prefix, bid); \
	`axi_wire_field(8, prefix, arlen); \
	`axi_wire_field(3, prefix, arsize); \
	`axi_wire_field(2, prefix, arburst); \
	`axi_wire_field(1, prefix, arlock); \
	`axi_wire_field(4, prefix, arcache); \
	`axi_wire_field(4, prefix, arqos); \
	`axi_wire_field(id_width, prefix, arid); \
	/* `axi_wire_field(6, prefix, aruser); */ \
	`axi_wire_field(1, prefix, rlast); \
	`axi_wire_field(id_width, prefix, rid)

`define axi_connect_if_no_id(io_prefix, wire_prefix) \
	`axilite_connect_if(io_prefix, wire_prefix), \
	`axi_connect_field(io_prefix, wire_prefix, awlen), \
	`axi_connect_field(io_prefix, wire_prefix, awsize), \
	`axi_connect_field(io_prefix, wire_prefix, awburst), \
	`axi_connect_field(io_prefix, wire_prefix, awlock), \
	`axi_connect_field(io_prefix, wire_prefix, awcache), \
	`axi_connect_field(io_prefix, wire_prefix, awqos), \
	/* `axi_connect_field(io_prefix, wire_prefix, awuser), */ \
	`axi_connect_field(io_prefix, wire_prefix, wlast), \
	`axi_connect_field(io_prefix, wire_prefix, arlen), \
	`axi_connect_field(io_prefix, wire_prefix, arsize), \
	`axi_connect_field(io_prefix, wire_prefix, arburst), \
	`axi_connect_field(io_prefix, wire_prefix, arlock), \
	`axi_connect_field(io_prefix, wire_prefix, arcache), \
	`axi_connect_field(io_prefix, wire_prefix, arqos), \
	/* `axi_connect_field(io_prefix, wire_prefix, aruser), */ \
	`axi_connect_field(io_prefix, wire_prefix, rlast)

`define axi_connect_if(io_prefix, wire_prefix) \
	`axi_connect_if_no_id(io_prefix, wire_prefix), \
	`axi_connect_field(io_prefix, wire_prefix, awid), \
	`axi_connect_field(io_prefix, wire_prefix, bid), \
	`axi_connect_field(io_prefix, wire_prefix, arid), \
	`axi_connect_field(io_prefix, wire_prefix, rid)
