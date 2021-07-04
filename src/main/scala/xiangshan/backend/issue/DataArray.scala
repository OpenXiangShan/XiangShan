/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

class DataArrayIO(config: RSConfig)(implicit p: Parameters) extends XSBundle {
  val read = Vec(config.numDeq, new DataArrayReadIO(config.numEntries, config.numSrc, config.dataBits))
  val write = Vec(config.numEnq, new DataArrayWriteIO(config.numEntries, config.numSrc, config.dataBits))
  val multiWrite = Vec(config.numValueBroadCast, new DataArrayMultiWriteIO(config.numEntries, config.numSrc, config.dataBits))
  val delayedWrite = if (config.delayedRf) Vec(config.numEnq, Flipped(ValidIO(UInt(config.dataBits.W)))) else null

  override def cloneType: DataArrayIO.this.type =
    new DataArrayIO(config).asInstanceOf[this.type]
}

class DataArray(config: RSConfig)(implicit p: Parameters) extends XSModule {
  val io = IO(new DataArrayIO(config))

  for (i <- 0 until config.numSrc) {
    val delayedWen = if (i == 1 && config.delayedRf) io.delayedWrite.map(_.valid) else Seq()
    val delayedWaddr = if (i == 1 && config.delayedRf) RegNext(VecInit(io.write.map(_.addr))) else Seq()
    val delayedWdata = if (i == 1 && config.delayedRf) io.delayedWrite.map(_.bits) else Seq()

    val wen = io.write.map(w => w.enable && w.mask(i)) ++ io.multiWrite.map(_.enable) ++ delayedWen
    val waddr = io.write.map(_.addr) ++ io.multiWrite.map(_.addr(i)) ++ delayedWaddr
    val wdata = io.write.map(_.data(i)) ++ io.multiWrite.map(_.data) ++ delayedWdata

    val dataModule = Module(new AsyncRawDataModuleTemplate(UInt(config.dataBits.W), config.numEntries, io.read.length, wen.length))
    dataModule.io.rvec := VecInit(io.read.map(_.addr))
    io.read.map(_.data(i)).zip(dataModule.io.rdata).map{ case (d, r) => d := r }
    dataModule.io.wen := wen
    dataModule.io.wvec := waddr
    dataModule.io.wdata := wdata
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
  io.data_out(1) := jalr_target
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

object ImmExtractor {
  def apply(config: RSConfig, exuCfg: ExuConfig, uop: MicroOp, data_in: Vec[UInt], pc: UInt, target: UInt)(implicit p: Parameters): Vec[UInt] = {
    val immExt = exuCfg match {
      case JumpExeUnitCfg => {
        val ext = Module(new JumpImmExtractor)
        ext.jump_pc := pc
        ext.jalr_target := target
        ext
      }
      case AluExeUnitCfg => Module(new AluImmExtractor)
      case _ => Module(new ImmExtractor(config.numSrc, config.dataBits))
    }
    immExt.io.uop := uop
    immExt.io.data_in := data_in
    immExt.io.data_out
  }
}
