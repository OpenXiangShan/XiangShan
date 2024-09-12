/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend.regfile

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.issue._
import xiangshan.backend.datapath.DataConfig._
import difftest._
import utility.RegNextN

class DiffRegState(implicit p: Parameters, params: BackendParams) extends XSModule {
  private val intSchdParams = params.schdParams(IntScheduler())
  private val fpSchdParams = params.schdParams(FpScheduler())
  private val vfSchdParams = params.schdParams(VfScheduler())

  private val v0RfSplitNum = VLEN / XLEN
  private val vfRfSplitNum = VLEN / XLEN

  val io = IO(new Bundle {
    val hartId = Input(UInt(8.W))

    val fromIntWb = MixedVec(params.genIntWriteBackBundle)
    val fromFpWb  = MixedVec(params.genFpWriteBackBundle)
    val fromVfWb  = MixedVec(params.genVfWriteBackBundle)
    val fromV0Wb  = MixedVec(params.genV0WriteBackBundle)
    val fromVlWb  = MixedVec(params.genVlWriteBackBundle)

    val intDiffTable = Input(Vec(IntLogicRegs, UInt(PhyRegIdxWidth.W)))
    val fpDiffTable  = Input(Vec(FpLogicRegs,  UInt(PhyRegIdxWidth.W)))
    val vecDiffTable = Input(Vec(VecLogicRegs, UInt(PhyRegIdxWidth.W)))
    val v0DiffTable  = Input(Vec(V0LogicRegs,  UInt(PhyRegIdxWidth.W)))
    val vlDiffTable  = Input(Vec(VlLogicRegs,  UInt(PhyRegIdxWidth.W)))

    val vecCSRState = Input(new Bundle {
      val vstart = UInt(64.W)
      val vxsat  = UInt(64.W)
      val vxrm   = UInt(64.W)
      val vcsr   = UInt(64.W)
      val vtype  = UInt(64.W)
      val vlenb  = UInt(64.W)
    })
  })

  private val intDiffRead: (Vec[UInt], Vec[UInt]) = (Wire(Vec(32, UInt(intSchdParams.pregIdxWidth.W))), Wire(Vec(32, UInt(XLEN.W))))
  private val fpDiffRead:  (Vec[UInt], Vec[UInt]) = (Wire(Vec(32, UInt(fpSchdParams.pregIdxWidth.W))), Wire(Vec(32, UInt(XLEN.W))))
  private val vfDiffRead:  (Vec[UInt], Vec[UInt]) = (Wire(Vec(31, UInt(vfSchdParams.pregIdxWidth.W))), Wire(Vec(31, UInt(VLEN.W))))
  private val v0DiffRead:  (Vec[UInt], Vec[UInt]) = (Wire(Vec(1, UInt(log2Up(V0PhyRegs).W))), Wire(Vec(1, UInt(V0Data().dataWidth.W))))
  private val vlDiffRead:  (Vec[UInt], Vec[UInt]) = (Wire(Vec(1, UInt(log2Up(VlPhyRegs).W))), Wire(Vec(1, UInt(VlData().dataWidth.W))))

  private val fpDiffReadData:  Vec[UInt] = Wire(Vec(32, UInt(XLEN.W)))
  private val vecDiffReadData: Vec[UInt] = Wire(Vec(64, UInt(64.W))) // v0 = Cat(Vec(1), Vec(0))
  private val vlDiffReadData:      UInt  = Wire(UInt(VlData().dataWidth.W))

  intDiffRead._1 := io.intDiffTable.take(32)
  fpDiffRead._1  := io.fpDiffTable.take(32)
  vfDiffRead._1  := io.vecDiffTable.slice(1, 32)
  v0DiffRead._1  := io.v0DiffTable.take(1)
  vlDiffRead._1  := io.vlDiffTable.take(1)

  fpDiffReadData  := fpDiffRead._2.slice(0, 32).map(_(63, 0)) // fp only used [63, 0]
  vecDiffReadData := v0DiffRead._2.slice(0, 1).map(x => Seq(x(63, 0), x(127, 64))).flatten ++ 
                     vfDiffRead._2.slice(0, 31).map(x => Seq(x(63, 0), x(127, 64))).flatten
  vlDiffReadData  := vlDiffRead._2(0)

  val intRegFile = IntRegFile("DebugIntRegFile", intSchdParams.numPregs, Seq(), Wire(Vec(0, UInt(0.W))),
    io.fromIntWb.map(_.wen), io.fromIntWb.map(_.addr), io.fromIntWb.map(_.data), bankNum = 1,
    debugReadAddr = Some(intDiffRead._1), debugReadData = Some(intDiffRead._2))
  val fpRegFile  = FpRegFile("DebugFpRegFile", fpSchdParams.numPregs, Seq(), Wire(Vec(0, UInt(0.W))),
    io.fromFpWb.map(_.wen), io.fromFpWb.map(_.addr), io.fromFpWb.map(_.data), bankNum = 1,
    debugReadAddr = Some(fpDiffRead._1), debugReadData = Some(fpDiffRead._2))
  val vfRegFile  = VfRegFile("DebugVfRegFile", vfSchdParams.numPregs, vfRfSplitNum, Seq(), Wire(Vec(0, UInt(0.W))),
    Seq.fill(vfRfSplitNum)(io.fromVfWb.map(_.wen)), io.fromVfWb.map(_.addr), io.fromVfWb.map(_.data),
    debugReadAddr = Some(vfDiffRead._1), debugReadData = Some(vfDiffRead._2))
  val v0RegFile  = VfRegFile("DebugV0RegFile", V0PhyRegs, v0RfSplitNum, Seq(), Wire(Vec(0, UInt(0.W))),
    Seq.fill(v0RfSplitNum)(io.fromV0Wb.map(_.wen)), io.fromV0Wb.map(_.addr), io.fromV0Wb.map(_.data),
    debugReadAddr = Some(v0DiffRead._1), debugReadData = Some(v0DiffRead._2))
  val vlRegFile  = FpRegFile("DebugVlRegFile", VlPhyRegs, Seq(), Wire(Vec(0, UInt(0.W))),
    io.fromVlWb.map(_.wen), io.fromVlWb.map(_.addr), io.fromVlWb.map(_.data), bankNum = 1, isVlRegfile = true,
    debugReadAddr = Some(vlDiffRead._1), debugReadData = Some(vlDiffRead._2))

  private val difftestArchIntRegState = DifftestModule(new DiffArchIntRegState, delay = 2)
  difftestArchIntRegState.coreid := io.hartId
  difftestArchIntRegState.value := intDiffRead._2

  private val difftestArchFpRegState = DifftestModule(new DiffArchFpRegState, delay = 2)
  difftestArchFpRegState.coreid := io.hartId
  difftestArchFpRegState.value := fpDiffReadData

  private val difftestArchVecRegState = DifftestModule(new DiffArchVecRegState, delay = 2)
  difftestArchVecRegState.coreid := io.hartId
  difftestArchVecRegState.value := vecDiffReadData

  private val diffVecCSRState = DifftestModule(new DiffVecCSRState)
  diffVecCSRState.coreid := io.hartId
  diffVecCSRState.vstart := io.vecCSRState.vstart
  diffVecCSRState.vxsat := io.vecCSRState.vxsat
  diffVecCSRState.vxrm := io.vecCSRState.vxrm
  diffVecCSRState.vcsr := io.vecCSRState.vcsr
  diffVecCSRState.vl := RegNextN(vlDiffReadData, 2)
  diffVecCSRState.vtype := io.vecCSRState.vtype
  diffVecCSRState.vlenb := io.vecCSRState.vlenb
}
