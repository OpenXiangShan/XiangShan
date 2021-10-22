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

package xiangshan.backend.exu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utils.{XSPerfAccumulate, XSPerfHistogram}
import xiangshan._

class ExuWbArbiter(n: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val in = Vec(n, Flipped(DecoupledIO(new ExuOutput)))
    val out = DecoupledIO(new ExuOutput)
  })

  class ExuCtrl extends Bundle{
    val uop = new MicroOp
    val fflags = UInt(5.W)
    val redirectValid = Bool()
    val redirect = new Redirect
    val debug = new DebugBundle
  }
  val ctrl_arb = Module(new Arbiter(new ExuCtrl, n))
  val data_arb = Module(new Arbiter(UInt((XLEN+1).W), n))

  ctrl_arb.io.out.ready := io.out.ready
  data_arb.io.out.ready := io.out.ready

  for(((in, ctrl), data) <- io.in.zip(ctrl_arb.io.in).zip(data_arb.io.in)){
    ctrl.valid := in.valid
    for((name, d) <- ctrl.bits.elements) {
      d := in.bits.elements(name)
    }
    data.valid := in.valid
    data.bits := in.bits.data
    in.ready := ctrl.ready
    assert(ctrl.ready === data.ready)
  }
  assert(ctrl_arb.io.chosen === data_arb.io.chosen)

  io.out.bits.data := data_arb.io.out.bits
  for((name, d) <- ctrl_arb.io.out.bits.elements){
    io.out.bits.elements(name) := d
  }
  io.out.valid := ctrl_arb.io.out.valid
  assert(ctrl_arb.io.out.valid === data_arb.io.out.valid)
}

class WbArbiter(cfgs: Seq[ExuConfig], numOut: Int, isFp: Boolean)(implicit p: Parameters) extends LazyModule {
  val priorities = cfgs.map(c => if(isFp) c.wbFpPriority else c.wbIntPriority)

  // NOTE:
  // 0 for direct connect (exclusive);
  // 1 for shared connect but non-blocked;
  // other for shared and may blocked
  val exclusivePorts = priorities.zipWithIndex.filter(_._1 == 0).map(_._2)
  val sharedPorts = priorities.zipWithIndex.filter(_._1 == 1).map(_._2)
  val otherPorts = priorities.zipWithIndex.filter(_._1 > 1).map(_._2)

  val numInPorts = cfgs.length
  val numOutPorts = exclusivePorts.length + sharedPorts.length
  require(numOutPorts <= numOut)
  if (numOutPorts < numOut) {
    println(s"Warning: only $numOutPorts of $numOut ports are used!")
  }

  def splitN(in: Seq[Int], n: Int): Seq[Seq[Int]] = {
    if (n == 0) {
      Seq()
    }
    else if (n == 1) {
      Seq(in)
    } else {
      if (in.size < n) {
        Seq(in) ++ Seq.fill(n - 1)(Seq())
      } else {
        val m = in.size / n
        in.take(m) +: splitN(in.drop(m), n - 1)
      }
    }
  }

  val otherConnections = splitN(otherPorts, sharedPorts.length)
  val sharedConnections = sharedPorts.zip(otherConnections).map{ case (s, o) => s +: o }
  val allConnections: Seq[Seq[Int]] = exclusivePorts.map(Seq(_)) ++ sharedConnections
  val hasFastUopOutVec = allConnections.map(_.map(cfgs(_).hasFastUopOut))
  val hasFastUopOut: Seq[Boolean] = hasFastUopOutVec.map(_.reduce(_ || _))
  hasFastUopOutVec.zip(hasFastUopOut).foreach{ case (vec, fast) =>
    if (fast && vec.contains(false)) {
      println("Warning: some exu does not have fastUopOut. It has extra one-cycle latency.")
    }
  }

  val sb = new StringBuffer(s"\n${if(isFp) "fp" else "int"} wb arbiter:\n")
  for ((port, i) <- exclusivePorts.zipWithIndex) {
    val hasFastUopOutS = if (hasFastUopOut(i)) s" (hasFastUopOut)" else ""
    sb.append(s"[ ${cfgs(port).name} ] -> out$hasFastUopOutS #$i\n")
  }
  for ((port, i) <- sharedPorts.zipWithIndex) {
    sb.append(s"[ ${cfgs(port).name} ")
    val useArb = otherConnections(i).nonEmpty
    for (req <- otherConnections(i)) {
      sb.append(s"${cfgs(req).name} ")
    }
    val hasFastUopOutS = if (hasFastUopOut(i + exclusivePorts.length)) s" (hasFastUopOut)" else ""
    sb.append(s"] -> ${if(useArb) "arb ->" else ""} out$hasFastUopOutS #${exclusivePorts.size + i}\n")
  }
  println(sb)

  lazy val module = new WbArbiterImp(this)
}

class WbArbiterImp(outer: WbArbiter)(implicit p: Parameters) extends LazyModuleImp(outer) {

  val io = IO(new Bundle() {
    val in = Vec(outer.numInPorts, Flipped(DecoupledIO(new ExuOutput)))
    val out = Vec(outer.numOutPorts, ValidIO(new ExuOutput))
  })

  val exclusiveIn = outer.exclusivePorts.map(io.in(_))
  val sharedIn = outer.sharedPorts.map(io.in(_))

  // exclusive ports are connected directly
  io.out.take(exclusiveIn.size).zip(exclusiveIn).zipWithIndex.foreach{
    case ((out, in), i) =>
      val hasFastUopOut = outer.hasFastUopOut(i)
      out.valid := in.valid
      out.bits := in.bits
      if (hasFastUopOut) {
        // When hasFastUopOut, only uop comes at the same cycle with valid.
        out.valid := RegNext(in.valid)
        out.bits.uop := RegNext(in.bits.uop)
      }
      in.ready := true.B
  }

  // shared ports are connected with an arbiter
  for (i <- sharedIn.indices) {
    val out = io.out(exclusiveIn.size + i)
    val shared = outer.sharedConnections(i).map(io.in(_))
    val hasFastUopOut = outer.hasFastUopOut(i + exclusiveIn.length)
    val arb = Module(new ExuWbArbiter(shared.size))
    arb.io.in <> shared
    out.valid := arb.io.out.valid
    out.bits := arb.io.out.bits
    if (hasFastUopOut) {
      out.valid := RegNext(arb.io.out.valid)
      // When hasFastUopOut, only uop comes at the same cycle with valid.
      // Other bits like data, fflags come at the next cycle after valid,
      // and they need to be selected with the fireVec.
      val fastVec = outer.hasFastUopOutVec(i + exclusiveIn.length)
      val dataVec = VecInit(shared.map(_.bits).zip(fastVec).map{ case (d, f) => if (f) d else RegNext(d) })
      val sel = VecInit(arb.io.in.map(_.fire)).asUInt
      out.bits := Mux1H(RegNext(sel), dataVec)
      // uop comes at the same cycle with valid and only RegNext is needed.
      out.bits.uop := RegNext(arb.io.out.bits.uop)
    }
    arb.io.out.ready := true.B
  }

  for (i <- 0 until outer.numInPorts) {
    XSPerfAccumulate(s"in_valid_$i", io.in(i).valid)
    XSPerfAccumulate(s"in_fire_$i", io.in(i).fire)
  }
  XSPerfHistogram("in_count", PopCount(io.in.map(_.valid)), true.B, 0, outer.numInPorts, 1)
  XSPerfHistogram("out_count", PopCount(io.out.map(_.valid)), true.B, 0, outer.numInPorts, 1)
}

class WbArbiterWrapper(
  exuConfigs: Seq[ExuConfig],
  numIntOut: Int,
  numFpOut: Int
)(implicit p: Parameters) extends LazyModule {
  val numInPorts = exuConfigs.length

  val intConfigs = exuConfigs.filter(_.writeIntRf)
  val intArbiter = LazyModule(new WbArbiter(intConfigs, numIntOut, isFp = false))
  val intWbPorts = intArbiter.allConnections.map(c => c.map(intConfigs(_)))
  val numIntWbPorts = intWbPorts.length
  val intConnections = intArbiter.allConnections

  val fpConfigs = exuConfigs.filter(_.writeFpRf)
  val fpArbiter = LazyModule(new WbArbiter(fpConfigs, numFpOut, isFp = true))
  val fpWbPorts = fpArbiter.allConnections.map(c => c.map(fpConfigs(_)))
  val numFpWbPorts = fpWbPorts.length
  val fpConnections = fpArbiter.allConnections

  val numOutPorts = intArbiter.numOutPorts + fpArbiter.numOutPorts

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle() {
      val in = Vec(numInPorts, Flipped(DecoupledIO(new ExuOutput)))
      val out = Vec(numOutPorts, ValidIO(new ExuOutput))
    })

    // ready is set to true.B as default (to be override later)
    io.in.foreach(_.ready := true.B)

    val intWriteback = io.in.zip(exuConfigs).filter(_._2.writeIntRf)
    intArbiter.module.io.in.zip(intWriteback).foreach { case (arb, (wb, cfg)) =>
      // When the function unit does not write fp regfile, we don't need to check fpWen
      arb.valid := wb.valid && (!cfg.writeFpRf.B || !wb.bits.uop.ctrl.fpWen)
      arb.bits := wb.bits
      when (arb.valid) {
        wb.ready := arb.ready
      }
    }

    val fpWriteback = io.in.zip(exuConfigs).filter(_._2.writeFpRf)
    fpArbiter.module.io.in.zip(fpWriteback).foreach{ case (arb, (wb, cfg)) =>
      // When the function unit does not write fp regfile, we don't need to check fpWen
      arb.valid := wb.valid && (!cfg.writeIntRf.B || wb.bits.uop.ctrl.fpWen)
      arb.bits := wb.bits
      when (arb.valid) {
        wb.ready := arb.ready
      }
    }

    io.out <> intArbiter.module.io.out ++ fpArbiter.module.io.out
  }
}
