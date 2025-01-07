/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.mem.Bundles._

class StdImp(override val wrapper: MemUnit)(implicit p: Parameters, params: MemUnitParams)
  extends MemUnitImp(wrapper)
{
  s0_out.ready := false.B
  io.toBackend.writeback.zip(s0_wbPort).map {
    case (wb, chosen) =>
      wb.valid      := s0_out.valid && chosen
      wb.bits       := DontCare
      wb.bits.uop   := s0_out.bits.uop
      wb.bits.data  := s0_out.bits.src(0)
      wb.bits.mmio  := false.B
      wb.bits.vaddr := 0.U
      wb.bits.paddr := 0.U

      when (chosen) {
        s0_out.ready := wb.ready
      }
  }
}
