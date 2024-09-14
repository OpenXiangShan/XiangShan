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

package xiangshan.backend.rename.predictor

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.frontend.{FtqPtr, FtqToCtrlIO, Ftq_RF_Components}

class StridePredictorPCMem()(implicit p: Parameters) extends XSModule with StridePredictorParams {

  private val numMemRead = RenameWidth + CommitUpdateSize

  val io = IO(new StridePredictorPcMemIO())

  private def hasRen: Boolean = true
  private val pcMem = Module(new SyncDataModuleTemplate(new SP_Ftq_RF_Components, FtqSize, numMemRead, 1, hasRen = hasRen))
  private val pcVec : Vec[UInt] = Wire(Vec(numMemRead, UInt(ValidPcWidth.W)))

  val fromFtqData = Wire(new SP_Ftq_RF_Components)
  fromFtqData.connectFromNormalComponents(io.fromFrontendFtq.pc_mem_wdata)
  pcMem.io.wen.head := GatedValidRegNext(io.fromFrontendFtq.pc_mem_wen)
  pcMem.io.waddr.head := RegEnable(io.fromFrontendFtq.pc_mem_waddr, io.fromFrontendFtq.pc_mem_wen)
  pcMem.io.wdata.head := RegEnable(fromFtqData, io.fromFrontendFtq.pc_mem_wen)

  for (i <- 0 until numMemRead) {
    val ren    = io.toStridePredictor(i).ren
    val ftqPtr = io.toStridePredictor(i).ftqPtr
    val offset = io.toStridePredictor(i).ftqOffset

    pcMem.io.ren.get(i) := ren
    pcMem.io.raddr(i) := ftqPtr.value
    pcVec(i) := pcMem.io.rdata(i).getPc(RegEnable(offset, ren))
  }

  io.toStridePredictor.zip(pcVec).foreach{ case (toSP, pc) =>
    toSP.pc := pc
  }
}

class SP_Ftq_RF_Components(implicit p: Parameters) extends XSBundle with StridePredictorParams {
  val startAddr = UInt(ValidPcWidth.W)
  val nextLineAddr = UInt(ValidPcWidth.W)
  val isNextMask = Vec(PredictWidth, Bool())

  def getPc(offset: UInt) = {
    def getHigher(pc: UInt) = pc(ValidPcWidth-1, log2Ceil(PredictWidth)+instOffsetBits+1)
    def getOffset(pc: UInt) = pc(log2Ceil(PredictWidth)+instOffsetBits, instOffsetBits)
    Cat(getHigher(Mux(isNextMask(offset) && startAddr(log2Ceil(PredictWidth)+instOffsetBits), nextLineAddr, startAddr)),
        getOffset(startAddr)+offset, 0.U(instOffsetBits.W))
  }

  def connectFromNormalComponents(in: Ftq_RF_Components) = {
    this.startAddr := in.startAddr
    this.nextLineAddr := in.nextLineAddr
    this.isNextMask := in.isNextMask
  }
}

class SPPcMemReadPort()(implicit p: Parameters) extends XSBundle with StridePredictorParams {
  // input
  val ren       = Input(Bool())
  val ftqPtr    = Input(new FtqPtr)
  val ftqOffset = Input(UInt(log2Up(PredictWidth).W))
  // output
  val pc        = Output(UInt(ValidPcWidth.W))
}

class StridePredictorPcMemIO()(implicit p: Parameters) extends XSBundle with StridePredictorParams {
  // from frontend
  val fromFrontendFtq = Flipped(new FtqToCtrlIO)
  // to stride predictor
  val toStridePredictor = Vec(RenameWidth + CommitUpdateSize, new SPPcMemReadPort)
}
