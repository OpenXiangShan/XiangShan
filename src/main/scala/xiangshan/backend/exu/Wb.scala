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

class Wb(cfgs: Seq[ExuConfig], numOut: Int, isFp: Boolean)(implicit p: Parameters) extends LazyModule {
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

  val sb = new StringBuffer(s"\n${if(isFp) "fp" else "int"} wb arbiter:\n")
  for ((port, i) <- exclusivePorts.zipWithIndex) {
    sb.append(s"[ ${cfgs(port).name} ] -> out #$i\n")
  }
  for ((port, i) <- sharedPorts.zipWithIndex) {
    sb.append(s"[ ${cfgs(port).name} ")
    val useArb = otherConnections(i).nonEmpty
    for (req <- otherConnections(i)) {
      sb.append(s"${cfgs(req).name} ")
    }
    sb.append(s"] -> ${if(useArb) "arb ->" else ""} out #${exclusivePorts.size + i}\n")
  }
  println(sb)

  lazy val module = new WbImp(this)
}

class WbImp(outer: Wb)(implicit p: Parameters) extends LazyModuleImp(outer) {

  val io = IO(new Bundle() {
    val in = Vec(outer.numInPorts, Flipped(DecoupledIO(new ExuOutput)))
    val out = Vec(outer.numOutPorts, ValidIO(new ExuOutput))
  })

  val exclusiveIn = outer.exclusivePorts.map(io.in(_))
  val sharedIn = outer.sharedPorts.map(io.in(_))

  // exclusive ports are connected directly
  io.out.take(exclusiveIn.size).zip(exclusiveIn).foreach{
    case (o, i) =>
      val arb = Module(new ExuWbArbiter(1))
      arb.io.in.head <> i
      o.bits := arb.io.out.bits
      o.valid := arb.io.out.valid
      arb.io.out.ready := true.B
  }

  // shared ports are connected with an arbiter
  for (i <- sharedIn.indices) {
    val out = io.out(exclusiveIn.size + i)
    val shared = outer.sharedConnections(i).map(io.in(_))
    val arb = Module(new ExuWbArbiter(shared.size))
    arb.io.in <> shared
    out.valid := arb.io.out.valid
    out.bits := arb.io.out.bits
    arb.io.out.ready := true.B
  }
}
