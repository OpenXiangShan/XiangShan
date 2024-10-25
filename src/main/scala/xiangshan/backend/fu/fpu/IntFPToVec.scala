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

// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package xiangshan.backend.fu.fpu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{SignExt, ZeroExt}
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.IF2VectorType

class IntFPToVec(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  private val isFliH = in.ctrl.fuOpType(7) &&  in.ctrl.fuOpType(6) && !in.ctrl.fuOpType(5)
  private val isFliS = in.ctrl.fuOpType(7) && !in.ctrl.fuOpType(6) && !in.ctrl.fuOpType(5)
  private val isFliD = in.ctrl.fuOpType(7) && !in.ctrl.fuOpType(6) &&  in.ctrl.fuOpType(5)
  private val isFli = isFliH || isFliS || isFliD

  private val FliData = Wire(UInt(XLEN.W))

  private val FliHTable = Module(new FliHTable)
  private val FliSTable = Module(new FliSTable)
  private val FliDTable = Module(new FliDTable)

  FliHTable.src := in.ctrl.fuOpType(4, 0)
  FliSTable.src := in.ctrl.fuOpType(4, 0)
  FliDTable.src := in.ctrl.fuOpType(4, 0)

  FliData := Mux1H(
    Seq(
      isFliH,
      isFliS,
      isFliD
    ),
    Seq(
      Cat(~0.U(48.W), FliHTable.out),
      Cat(~0.U(32.W), FliSTable.out, 0.U(16.W)),
      Cat(FliDTable.out, 0.U(48.W)))
  )

  // vsew is the lowest 2 bits of fuOpType
  private val isImm = Mux(isFli, 0.U, IF2VectorType.isImm(in.ctrl.fuOpType(4, 2))).asBool
  // when needDup is true, the scalar data is duplicated in vector register
  private val needDup = Mux(isFli, 0.U, IF2VectorType.needDup(in.ctrl.fuOpType(4, 2))).asBool
  // when isFmv is true, the high bits of the scalar data is 1
  private val isFmv = Mux(isFli, 0.U, IF2VectorType.isFmv(in.ctrl.fuOpType(4, 2))).asBool

  private val isFp = Mux(isFli, 0.U, IF2VectorType.isFp(in.ctrl.fuOpType(4, 2))).asBool

  // imm use src(1), scalar use src(0)
  private val scalaData = Mux(isFli, FliData, Mux(isImm, in.data.src(1), in.data.src(0)))
  // vsew is the lowest 2 bits of fuOpType
  private val vsew = in.ctrl.fuOpType(1, 0)
  private val dataWidth = cfg.destDataBits

  private val outNAN = Seq(
    Cat(0.U, Fill(3, 1.U), 1.U, 0.U(3.W)),
    Cat(0.U, Fill(5, 1.U), 1.U, 0.U(9.W)),
    Cat(0.U, Fill(8, 1.U), 1.U, 0.U(22.W))
  )
  private val isFpCanonicalNAN = Seq(
    !scalaData.head(56).andR,
    !scalaData.head(48).andR,
    !scalaData.head(32).andR
  )

  private val fpData = Mux1H(Seq(
    (vsew === VSew.e8)  -> Cat(Fill(56, 1.U), scalaData( 7, 0)),
    (vsew === VSew.e16) -> Cat(Fill(48, 1.U), scalaData(15, 0)),
    (vsew === VSew.e32) -> Cat(Fill(32, 1.U), scalaData(31, 0)),
    (vsew === VSew.e64) -> scalaData
  ))

  private val vecE8Data  = Wire(Vec(dataWidth /  8, UInt( 8.W)))
  private val vecE16Data = Wire(Vec(dataWidth / 16, UInt(16.W)))
  private val vecE32Data = Wire(Vec(dataWidth / 32, UInt(32.W)))
  private val vecE64Data = Wire(Vec(dataWidth / 64, UInt(64.W)))

  vecE8Data   := VecInit(Seq.fill(dataWidth /  8)(Mux(isFpCanonicalNAN(0) & isFp, outNAN(0), scalaData( 7, 0))))
  vecE16Data  := VecInit(Seq.fill(dataWidth / 16)(Mux(isFpCanonicalNAN(1) & isFp, outNAN(1), scalaData(15, 0))))
  vecE32Data  := VecInit(Seq.fill(dataWidth / 32)(Mux(isFpCanonicalNAN(2) & isFp, outNAN(2), scalaData(31, 0))))
  vecE64Data  := VecInit(Seq.fill(dataWidth / 64)(scalaData(63, 0)))
  connect0LatencyCtrlSingal
  out.res.data := Mux(needDup, Mux1H(Seq(
    (vsew === VSew.e8)  -> vecE8Data.asUInt,
    (vsew === VSew.e16) -> vecE16Data.asUInt,
    (vsew === VSew.e32) -> vecE32Data.asUInt,
    (vsew === VSew.e64) -> vecE64Data.asUInt,
  )), Mux(isFmv, fpData, scalaData))
}
