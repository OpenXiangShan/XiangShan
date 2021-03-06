package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._

class ExuWbArbiter(n: Int) extends XSModule {
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

class Wb(cfgs: Seq[ExuConfig], numOut: Int, isFp: Boolean) extends XSModule {

  val priorities = cfgs.map(c => if(isFp) c.wbFpPriority else c.wbIntPriority)

  val io = IO(new Bundle() {
    val in = Vec(cfgs.size, Flipped(DecoupledIO(new ExuOutput)))
    val out = Vec(numOut, ValidIO(new ExuOutput))
  })

  val directConnect = io.in.zip(priorities).filter(x => x._2 == 0).map(_._1)
  val mulReq = io.in.zip(priorities).filter(x => x._2 == 1).map(_._1)
  val otherReq = io.in.zip(priorities).filter(x => x._2 > 1).map(_._1)

  val portUsed = directConnect.size + mulReq.size
  require(portUsed <= numOut)

  io.out.take(directConnect.size).zip(directConnect).foreach{
    case (o, i) =>
      val arb = Module(new ExuWbArbiter(1))
      arb.io.in.head <> i
      o.bits := arb.io.out.bits
      o.valid := arb.io.out.valid
      arb.io.out.ready := true.B
  }

  def splitN[T](in: Seq[T], n: Int): Seq[Option[Seq[T]]] = {
    if(n == 0) return Seq()
    if(n == 1){
      Seq(Some(in))
    } else {
      if(in.size < n ){
        Seq(Some(in)) ++ Seq.fill(n-1)(None)
      } else {
        val m = in.size / n
        Some(in.take(m)) +: splitN(in.drop(m), n-1)
      }
    }
  }

  val arbReq = splitN(
    otherReq,
    mulReq.size
  )

  for(i <- mulReq.indices) {
    val out = io.out(directConnect.size + i)
    val other = arbReq(i).getOrElse(Seq())
    val arb = Module(new ExuWbArbiter(1+other.size))
    arb.io.in <> mulReq(i) +: other
    out.valid := arb.io.out.valid
    out.bits := arb.io.out.bits
    arb.io.out.ready := true.B
  }

  if(portUsed < numOut){
    println(s"Warning: ${numOut - portUsed} ports are not used!")
    io.out.drop(portUsed).foreach(_ <> DontCare)
  }

  val sb = new StringBuffer(s"\n${if(isFp) "fp" else "int"} wb arbiter:\n")
  for((conn, i) <- directConnect.zipWithIndex){
    sb.append(s"[ ${cfgs(io.in.indexOf(conn)).name} ] -> out #$i\n")
  }
  for(i <- mulReq.indices){
    sb.append(s"[ ${cfgs(io.in.indexOf(mulReq(i))).name} ")
    val useArb = arbReq(i).nonEmpty
    for(req <- arbReq(i).getOrElse(Nil)){
      sb.append(s"${cfgs(io.in.indexOf(req)).name} ")
    }
    sb.append(s"] -> ${if(useArb) "arb ->" else ""} out #${directConnect.size + i}\n")
  }
  println(sb)

}