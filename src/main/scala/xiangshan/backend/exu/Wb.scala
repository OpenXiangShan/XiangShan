package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._


class Wb(priorities: Seq[Int], numOut: Int) extends XSModule {
  val io = IO(new Bundle() {
    val in = Vec(priorities.size, Flipped(DecoupledIO(new ExuOutput)))
    val out = Vec(numOut, ValidIO(new ExuOutput))
  })


//  def exuOutToRfReq(exuOut: DecoupledIO[ExuOutput]): DecoupledIO[ExuOutput] = {
//    val req = WireInit(exuOut)
//    req.valid := exuOut.valid && wen(exuOut.bits)
//    exuOut.ready := Mux(req.valid, req.ready, true.B)
//    req
//  }

  val directConnect = io.in.zip(priorities).filter(x => x._2 == 0).map(_._1)
  val mulReq = io.in.zip(priorities).filter(x => x._2 == 1).map(_._1)
  val otherReq = io.in.zip(priorities).filter(x => x._2 > 1).map(_._1)

  val portUsed = directConnect.size + mulReq.size
  require(portUsed <= numOut)

  io.out.take(directConnect.size).zip(directConnect).foreach{
    case (o, i) =>
      o.bits := i.bits
      o.valid := i.valid
      i.ready := true.B
  }

  def splitN[T](in: Seq[T], n: Int): Seq[Option[Seq[T]]] = {
    require(n > 0)
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

  if(mulReq.nonEmpty){
    val arbReq = splitN(
      otherReq,
      mulReq.size
    )
    for(i <- mulReq.indices){
      val other = arbReq(i).getOrElse(Seq())
      val arb = Module(new Arbiter(new ExuOutput, 1+other.size))
      arb.io.in <> mulReq(i) +: other
      val out = io.out(directConnect.size + i)
      out.valid := arb.io.out.valid
      out.bits := arb.io.out.bits
      arb.io.out.ready := true.B
    }
  }

  if(portUsed < numOut){
    println(s"Warning: ${numOut - portUsed} ports are not used!")
    io.out.drop(portUsed).foreach(_ <> DontCare)
  }
}