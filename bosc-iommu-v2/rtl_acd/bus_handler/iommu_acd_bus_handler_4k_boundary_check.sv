module iommu_acd_bus_handler_4k_boundary_check #(
    parameter BUS_ADDR_WIDTH        = 64,
    parameter BUS_SIZE_WIDTH        = 3
)(
    input  logic                        valid_i,
    input  logic [BUS_ADDR_WIDTH-1:0]   addr_i,
    input  logic [1:0]                  burst_i,
    input  logic [7:0]                  length_i,
    input  logic [BUS_SIZE_WIDTH-1:0]   size_i,
    output logic                        violation_o
); 

// remove bc check, leave it done by downstream NIC. 20250226, gpf
    assign violation_o = 1'b0;
//    logic [8:0] length;
//    assign length = {1'b0, length_i};
//    
//        always@(*) begin
//            violation_o = 1'b0;
//            if (valid_i) begin
//                case (burst_i)
//                    // BURST_FIXED: The final address is Start Addr + 8 (ex: ARADDR + 8)
//                    2'b00: begin
//                        if (((addr_i & 12'hfff) + (1'b1 << size_i)) < (1'b1 << 12)) begin
//                            violation_o = 1'b0;
//                        end
//                        else begin
//                            violation_o = 1'b1;
//                        end
//                    end
//                    // BURST_WRAP: The final address is the Wrap Boundary (Lower address) + size of the transfer
//                    2'b10: begin
//                        // wrap_boundary = (start_address/(number_bytes*length)) * (number_bytes*length)
//                        // address_n = wrap_boundary + (number_bytes * length)
//                        logic [BUS_ADDR_WIDTH-1:0] wrap_boundary;
//                        // by spec, N of transfers must be {2, 4, 8, 16}
//                        // So, ARLEN must be {1, 3, 7, 15}
//                        logic [2:0] log2_len;
//                        case (length)
//                            9'd1:   log2_len = 3'b001;
//                            9'd3:   log2_len = 3'b010;
//                            9'd7:   log2_len = 3'b011;
//                            9'd15:  log2_len = 3'b100;
//                            default:log2_len = 3'b111;  // invalid
//                        endcase
//                        // The lowest address within a wrapping burst
//                        // Wrap_Boundary = (INT(Start_Address / (Burst_Length x Number_Bytes))) x (Burst_Length x Number_Bytes)
//                        wrap_boundary = (addr_i >> (log2_len + size_i)) << (log2_len + size_i);
//                        // Check if the highest address crosses a 4 kiB boundary (Highest Addr - Lower Addr >= 4kiB)
//                        // Highest addr_i = Wrap_Boundary + (Burst_Length x Number_Bytes)
//                        if (!(&log2_len) && 
//                             (((wrap_boundary & 12'hfff) + ((length + 1) << size_i)) <= (1'b1 << 12))) begin
//                            violation_o = 1'b0;     // Allow transaction
//                        end
//                        // Boundary violation
//                        else begin
//                            violation_o = 1'b1;
//                        end
//                    end
//                    // BURST_INCR: The final address is Start Addr + Burst_Length x Number_Bytes
//                    2'b01: begin
//                        // check if burst is within 4K range
//                        if (((addr_i & 12'hfff) + ((length + 1) << size_i)) <= (1'b1 << 12)) begin
//                            violation_o = 1'b0;     // Allow transaction
//                        end
//                        // Boundary violation
//                        else begin
//                            violation_o = 1'b1;
//                        end
//                    end
//                    default:violation_o = 1'b0;
//                endcase
//            end
//        end
endmodule
