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

package xiangshan.backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSPerfAccumulate
import xiangshan._
import xiangshan.backend.fu.fpu._

trait HasFuLatency {
  val latencyVal: Option[Int]
}

case class CertainLatency(value: Int) extends HasFuLatency {
  override val latencyVal: Option[Int] = Some(value)
}

case class UncertainLatency() extends HasFuLatency {
  override val latencyVal: Option[Int] = None
}


case class FuConfig
(
  name: String,
  fuGen: Parameters => FunctionUnit,
  fuSel: MicroOp => Bool,
  fuType: UInt,
  numIntSrc: Int,
  numFpSrc: Int,
  numVecSrc: Int = 0,
  writeIntRf: Boolean,
  writeFpRf: Boolean,
  writeVecRf: Boolean = false,
  writeFflags: Boolean = false,
  hasRedirect: Boolean = false,
  latency: HasFuLatency = CertainLatency(0),
  fastUopOut: Boolean = false,
  fastImplemented: Boolean = false,
  hasInputBuffer: (Boolean, Int, Boolean) = (false, 0, false),
  exceptionOut: Seq[Int] = Seq(),
  hasLoadError: Boolean = false,
  flushPipe: Boolean = false,
  replayInst: Boolean = false,
  trigger: Boolean = false
) {
  def srcCnt: Int = math.max(math.max(numIntSrc, numFpSrc), numVecSrc)
  def isVectorFU: Boolean = (numVecSrc > 0) && writeVecRf

  require(numFpSrc == 0 || numVecSrc == 0, "numVecSrc+numFpSrc is not handled now. It's forbidden until someone add support for numVecSrc+numFpSrc")

  override def toString: String = {
    s"${name}: SrcNum(${numIntSrc}|${numFpSrc}|${numVecSrc}) " +
    s"Write(" +
    (if(writeIntRf) "int|" else "") +
    (if(writeFpRf) "fp|" else "") +
    (if(writeVecRf) "vec|" else "") +
    (if(writeFflags) "fflags" else "") +
    (if(!writeIntRf && !writeFpRf && !writeVecRf && !writeFflags) "none" else "") + ") " +
    (if(hasRedirect) "hasRedirect " else "") +
    (if(latency.latencyVal.getOrElse(99) != 99) "latency " + latency.latencyVal.get+" " else "") +
    (if(fastUopOut) "hasFastUopOut " else "") +
    s"inputBuffer (${hasInputBuffer._1},${hasInputBuffer._2},${hasInputBuffer._3}) "
  }
}


class FuOutput(val len: Int)(implicit p: Parameters) extends XSBundle {
  val data = UInt(len.W)
  val uop = new MicroOp
}

class FunctionUnitInput(val len: Int)(implicit p: Parameters) extends XSBundle {
  val src = Vec(3, UInt(len.W))
  val uop = new MicroOp
}

class FunctionUnitIO(val len: Int)(implicit p: Parameters) extends XSBundle {
  val in = Flipped(DecoupledIO(new FunctionUnitInput(len)))

  val out = DecoupledIO(new FuOutput(len))

  val redirectIn = Flipped(ValidIO(new Redirect))
}

abstract class FunctionUnit(len: Int = 64)(implicit p: Parameters) extends XSModule {

  val io = IO(new FunctionUnitIO(len))

  XSPerfAccumulate("in_valid", io.in.valid)
  XSPerfAccumulate("in_fire", io.in.fire)
  XSPerfAccumulate("out_valid", io.out.valid)
  XSPerfAccumulate("out_fire", io.out.fire)

}

abstract class FUWithRedirect(len: Int = 64)(implicit p: Parameters) extends FunctionUnit(len: Int) with HasRedirectOut

trait HasPipelineReg {
  this: FunctionUnit =>

  def latency: Int

  require(latency > 0)

  val validVec = io.in.valid +: Array.fill(latency)(RegInit(false.B))
  val rdyVec = (Array.fill(latency - 1)(Wire(Bool())) :+ io.out.ready) :+ WireInit(true.B)
  val uopVec = io.in.bits.uop +: Array.fill(latency)(Reg(new MicroOp))


  // if flush(0), valid 0 will not given, so set flushVec(0) to false.B
  val flushVec = validVec.zip(uopVec).map(x => x._1 && x._2.robIdx.needFlush(io.redirectIn))

  for (i <- 0 until latency - 1) {
    rdyVec(i) := !validVec(i + 1) || rdyVec(i + 1)
  }

  for (i <- 1 to latency) {
    when(rdyVec(i - 1) && validVec(i - 1) && !flushVec(i - 1)){
      validVec(i) := validVec(i - 1)
      uopVec(i) := uopVec(i - 1)
    }.elsewhen(flushVec(i) || rdyVec(i)){
      validVec(i) := false.B
    }
  }

  io.in.ready := rdyVec(0)
  io.out.valid := validVec.takeRight(2).head
  io.out.bits.uop := uopVec.takeRight(2).head

  def regEnable(i: Int): Bool = validVec(i - 1) && rdyVec(i - 1) && !flushVec(i - 1)

  def PipelineReg[TT <: Data](i: Int)(next: TT) = RegEnable(
    next,
    regEnable(i)
  )

  def S1Reg[TT <: Data](next: TT): TT = PipelineReg[TT](1)(next)

  def S2Reg[TT <: Data](next: TT): TT = PipelineReg[TT](2)(next)

  def S3Reg[TT <: Data](next: TT): TT = PipelineReg[TT](3)(next)

  def S4Reg[TT <: Data](next: TT): TT = PipelineReg[TT](4)(next)

  def S5Reg[TT <: Data](next: TT): TT = PipelineReg[TT](5)(next)
}
