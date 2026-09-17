/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
*
* OpenIOMMU is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

module iommu_multi_bit_1_check #( //{{{
    parameter WIDTH = 8
)(
    input  logic [WIDTH-1:0]    din,
    output logic                ok
);
    logic [WIDTH-1:0] check;

genvar pp;
generate
    assign check[0] = 1'b0;
    for(pp=1; pp<WIDTH; pp++) begin : check_gen
        assign check[pp] = din[pp] & (din[(pp-1):0] != 'd0);
    end
endgenerate

    assign ok = |check;
endmodule
//}}}




