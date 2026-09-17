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

module atd_sync2_rstn(
input clk,
input rst_n,
output so
);
reg si_d1, si_d2;
always@(posedge clk or negedge rst_n)
    if (!rst_n) begin
        si_d1 <= 1'b0;
        si_d2 <= 1'b0;
    end else begin
        si_d1 <= 1'b1;
        si_d2 <= si_d1;
    end
assign so = si_d2;
endmodule
