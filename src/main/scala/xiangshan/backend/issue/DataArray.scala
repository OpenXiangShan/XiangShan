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

package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.decode.{ImmUnion, Imm_U}
import xiangshan.backend.exu.ExuConfig

class DataArrayReadIO(numEntries: Int, numSrc: Int, dataBits: Int)(implicit p: Parameters) extends XSBundle {
  val addr = Input(UInt(numEntries.W))
  val data = Vec(numSrc, Output(UInt(dataBits.W)))

  override def cloneType: DataArrayReadIO.this.type =
    new DataArrayReadIO(numEntries, numSrc, dataBits).asInstanceOf[this.type]
}

class DataArrayWriteIO(numEntries: Int, numSrc: Int, dataBits: Int)(implicit p: Parameters) extends XSBundle {
  val enable = Input(Bool())
  val mask   = Vec(numSrc, Input(Bool()))
  val addr   = Input(UInt(numEntries.W))
  val data   = Vec(numSrc, Input(UInt(dataBits.W)))

  override def cloneType: DataArrayWriteIO.this.type =
    new DataArrayWriteIO(numEntries, numSrc, dataBits).asInstanceOf[this.type]
}

class DataArrayMultiWriteIO(numEntries: Int, numSrc: Int, dataBits: Int)(implicit p: Parameters) extends XSBundle {
  val enable = Input(Bool())
  val addr   = Vec(numSrc, Input(UInt(numEntries.W)))
  val data   = Input(UInt(dataBits.W))

  override def cloneType: DataArrayMultiWriteIO.this.type =
    new DataArrayMultiWriteIO(numEntries, numSrc, dataBits).asInstanceOf[this.type]
}

class DataArrayIO(params: RSParams)(implicit p: Parameters) extends XSBundle {
  val read = Vec(params.numDeq + 1, new DataArrayReadIO(params.numEntries, params.numSrc, params.dataBits))
  val write = Vec(params.numEnq, new DataArrayWriteIO(params.numEntries, params.numSrc, params.dataBits))
  val multiWrite = Vec(params.numWakeup, new DataArrayMultiWriteIO(params.numEntries, params.numSrc, params.dataBits))
  val delayedWrite = if (params.delayedRf) Vec(params.numEnq, Flipped(ValidIO(UInt(params.dataBits.W)))) else null
  val partialWrite = if (params.hasMidState) Vec(params.numDeq, new DataArrayWriteIO(params.numEntries, params.numSrc - 1, params.dataBits)) else null

  override def cloneType: DataArrayIO.this.type =
    new DataArrayIO(params).asInstanceOf[this.type]
}

class DataArray(params: RSParams)(implicit p: Parameters) extends XSModule {
  val io = IO(new DataArrayIO(params))

  for (i <- 0 until params.numSrc) {
    val delayedWen = if (i == 1 && params.delayedRf) io.delayedWrite.map(_.valid) else Seq()
    val delayedWaddr = if (i == 1 && params.delayedRf) RegNext(VecInit(io.write.map(_.addr))) else Seq()
    val delayedWdata = if (i == 1 && params.delayedRf) io.delayedWrite.map(_.bits) else Seq()

    val partialWen = if (i < 2 && params.hasMidState) io.partialWrite.map(_.enable) else Seq()
    val partialWaddr = if (i < 2 && params.hasMidState) io.partialWrite.map(_.addr) else Seq()
    val partialWdata = if (i < 2 && params.hasMidState) io.partialWrite.map(_.data(i)) else Seq()

    val wen = io.write.map(w => w.enable && w.mask(i)) ++ io.multiWrite.map(_.enable) ++ delayedWen ++ partialWen
    val waddr = io.write.map(_.addr) ++ io.multiWrite.map(_.addr(i)) ++ delayedWaddr ++ partialWaddr
    val wdata = io.write.map(_.data(i)) ++ io.multiWrite.map(_.data) ++ delayedWdata ++ partialWdata

    val dataModule = Module(new SyncRawDataModuleTemplate(UInt(params.dataBits.W), params.numEntries, io.read.length, wen.length))
    dataModule.io.rvec := VecInit(io.read.map(_.addr))
    io.read.map(_.data(i)).zip(dataModule.io.rdata).foreach{ case (d, r) => d := r }
    dataModule.io.wen := wen
    dataModule.io.wvec := waddr
    dataModule.io.wdata := wdata
    for (i <- 0 until params.numEntries) {
      val w = VecInit(wen.indices.map(j => dataModule.io.wen(j) && dataModule.io.wvec(j)(i)))
      assert(RegNext(PopCount(w) <= 1.U))
      when(PopCount(w) > 1.U) {
        XSDebug("ERROR: RS DataArray write overlap!\n")
      }
    }
  }

}

class ImmExtractor(numSrc: Int, dataBits: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val uop = Input(new MicroOp)
    val data_in = Vec(numSrc, Input(UInt(dataBits.W)))
    val data_out = Vec(numSrc, Output(UInt(dataBits.W)))
  })
  io.data_out := io.data_in
}

class JumpImmExtractor(implicit p: Parameters) extends ImmExtractor(2, 64) {
  val jump_pc = IO(Input(UInt(VAddrBits.W)))
  val jalr_target = IO(Input(UInt(VAddrBits.W)))

  when (SrcType.isPc(io.uop.ctrl.srcType(0))) {
    io.data_out(0) := SignExt(jump_pc, XLEN)
  }
  // when src1 is reg (like sfence's asid) do not let data_out(1) be the jarl_target
  when (!SrcType.isReg(io.uop.ctrl.srcType(1))) {
    io.data_out(1) := jalr_target
  }
}

class AluImmExtractor(implicit p: Parameters) extends ImmExtractor(2, 64) {
  when (SrcType.isImm(io.uop.ctrl.srcType(1))) {
    val imm32 = Mux(io.uop.ctrl.selImm === SelImm.IMM_U,
      ImmUnion.U.toImm32(io.uop.ctrl.imm),
      ImmUnion.I.toImm32(io.uop.ctrl.imm)
    )
    io.data_out(1) := SignExt(imm32, XLEN)
  }
}

class MduImmExtractor(implicit p: Parameters) extends ImmExtractor(2, 64) {
  when (SrcType.isImm(io.uop.ctrl.srcType(1))) {
    val imm32 = ImmUnion.I.toImm32(io.uop.ctrl.imm)
    io.data_out(1) := SignExt(imm32, XLEN)
  }
}

object ImmExtractor {
  def apply(params: RSParams, uop: MicroOp, data_in: Vec[UInt], pc: Option[UInt], target: Option[UInt])
           (implicit p: Parameters): Vec[UInt] = {
    val immExt = (params.isJump, params.isAlu, params.isMul) match {
      case (true, false, false) => {
        val ext = Module(new JumpImmExtractor)
        ext.jump_pc := pc.get
        ext.jalr_target := target.get
        ext
      }
      case (false, true, false) => Module(new AluImmExtractor)
      case (false, false, true) => Module(new MduImmExtractor)
      case _ => Module(new ImmExtractor(params.numSrc, params.dataBits))
    }
    immExt.io.uop := uop
    immExt.io.data_in := data_in
    immExt.io.data_out
  }
}
