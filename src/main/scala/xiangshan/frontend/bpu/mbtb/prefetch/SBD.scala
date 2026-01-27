package xiangshan.frontend.bpu.mbtb.prefetch

// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.XSDebug
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.mbtb.prefetch.PrefetchBtbBundle
import xiangshan.frontend.bpu.mbtb.prefetch.PrefetchBtbModule
import xiangshan.frontend.ifu.PreDecodeHelper

class SBD(implicit p: Parameters) extends PrefetchBtbModule with SBDHelper {
  class PreDecodeIO(implicit p: Parameters) extends PrefetchBtbBundle {
    class PreDecodeReq(implicit p: Parameters) extends PrefetchBtbBundle {
      val data:       Vec[UInt] = Vec(FetchBlockInstNum, UInt(32.W))
      val isRvc:      Vec[Bool] = Vec(FetchBlockInstNum, Bool())
      val instrValid: Vec[Bool] = Vec(FetchBlockInstNum, Bool())
    }
    class PreDecodeResp(implicit p: Parameters) extends PrefetchBtbBundle {
      val pd:         Vec[PreDecodeInfo] = Vec(FetchBlockInstNum, new PreDecodeInfo)
      val instr:      Vec[UInt]          = Vec(FetchBlockInstNum, UInt(32.W))
      val jumpOffset: Vec[PrunedAddr]    = Vec(FetchBlockInstNum, PrunedAddr(VAddrBits))
    }

    val req:  Valid[PreDecodeReq] = Flipped(ValidIO(new PreDecodeReq))
    val resp: PreDecodeResp       = Output(new PreDecodeResp)
  }
  val io: PreDecodeIO = IO(new PreDecodeIO)

  private val data     = io.req.bits.data
  private val rawInsts = VecInit((0 until FetchBlockInstNum).map(i => data(i)))

  for (i <- 0 until FetchBlockInstNum) {
    val inst = WireInit(rawInsts(i))

    val jalOffset = getJalOffset(inst, io.req.bits.isRvc(i))
    val brOffset  = getBrOffset(inst, io.req.bits.isRvc(i))

    io.resp.pd(i).valid := io.req.bits.instrValid(i)
    io.resp.pd(i).isRVC := io.req.bits.isRvc(i)

    // for diff purpose only
    io.resp.pd(i).brAttribute := BranchAttribute.decode(inst, io.req.valid)

    io.resp.instr(i)      := inst
    io.resp.jumpOffset(i) := Mux(io.resp.pd(i).isBr, brOffset, jalOffset)
  }

  for (i <- 0 until FetchBlockInstNum) {
    XSDebug(
      true.B,
      p"instr ${Hexadecimal(io.resp.instr(i))}, " +
        p"isRVC ${Binary(io.resp.pd(i).isRVC)}, " +
        p"branchType ${Binary(io.resp.pd(i).brAttribute.branchType)}, " +
        p"rasAction ${Binary(io.resp.pd(i).brAttribute.rasAction)}, "
    )
  }
}
