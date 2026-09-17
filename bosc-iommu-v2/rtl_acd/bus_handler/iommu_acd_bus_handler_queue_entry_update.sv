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

////////////////////////////////////////////////////////////////////////////
// iommu_acd_bus_handler_queue_entry_update
// check if all bits in valid_i below tag_i is 1 and tag_i's bit is 0
// for example
//     tag_i == 3
//     if(valid_i[2:0] == 3'b111 && valid_i[3] == 1'b0) update_o = 1;
//     else                                             update_o = 0;
////////////////////////////////////////////////////////////////////////////
module iommu_acd_bus_handler_queue_entry_update #( //{{{
    parameter IDX_WIDTH             = 3,
    parameter DEPTH                 = 2**IDX_WIDTH
)(
    input  logic [DEPTH-1:0]        valid_i,
    input  logic                    update_i,
    input  logic [IDX_WIDTH-1:0]    tag_i,
    output logic                    update_o
);

    logic [DEPTH-1:0] tmp_vec;
    always@(*) begin
        for(int unsigned i = 0; i < DEPTH; i++) begin
            if(i < tag_i)
                tmp_vec[i] = valid_i[i];
            else
                tmp_vec[i] = 1'b1;
        end
    end

    assign update_o = (&tmp_vec) & (~valid_i[tag_i]) & update_i;

endmodule
//}}}
