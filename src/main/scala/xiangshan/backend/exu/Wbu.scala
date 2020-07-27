package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class WriteBackArbMtoN(m: Int, n: Int) extends XSModule {
  val io = IO(new Bundle() {
    val in = Vec(m, Flipped(DecoupledIO(new ExuOutput)))
    val out = Vec(n, ValidIO(new ExuOutput))
  })

  require(m >= n, "m < n! Why use an arbiter???")

  // first n-1 ports, direct connect
  for((i, o) <- io.in.take(n-1).zip(io.out)){
    o.valid := i.valid
    o.bits := i.bits
    i.ready := true.B
  }

  // last m-(n-1) ports, rr arb
  val arb = Module(new RRArbiter[ExuOutput](new ExuOutput, m-n+1))

  for((arbIn, ioIn) <- arb.io.in.zip(io.in.drop(n-1))){
    arbIn <> ioIn
  }

  io.out.last.bits := arb.io.out.bits
  io.out.last.valid := arb.io.out.valid
  arb.io.out.ready := true.B

  for (i <- 0 until n) {
    XSInfo(io.out(i).valid, "out(%d) pc(0x%x) writebacks 0x%x to pdest(%d) ldest(%d)\n", i.U, io.out(i).bits.uop.cf.pc,
      io.out(i).bits.data, io.out(i).bits.uop.pdest, io.out(i).bits.uop.ctrl.ldest)
  }

}

class Wbu(wbIntIdx: Array[Int], wbFpIdx: Array[Int]) extends XSModule{

  val io = IO(new Bundle() {
    val in  = Vec(exuParameters.ExuCnt, Flipped(DecoupledIO(new ExuOutput)))
    val toRoq = Vec(exuParameters.ExuCnt, ValidIO(new ExuOutput))
    val toIntRf = Vec(NRIntWritePorts, ValidIO(new ExuOutput))
    val toFpRf = Vec(NRFpWritePorts, ValidIO(new ExuOutput))
  })

  def exuOutToRfReq
  (exuOutVec: Vec[DecoupledIO[ExuOutput]], fp: Boolean): Seq[(DecoupledIO[ExuOutput], Int)] = {
    val wbIdxSet = if(fp) wbFpIdx else wbIntIdx
    exuOutVec.zipWithIndex.filter(x => wbIdxSet.contains(x._2)).map({
      case(exuOut, idx) =>
        val req = Wire(Decoupled(new ExuOutput))
        req.valid := exuOut.valid && {if(fp) exuOut.bits.uop.ctrl.fpWen else exuOut.bits.uop.ctrl.rfWen}
        req.bits := exuOut.bits
        (req, idx)
    })
  }

  val wbIntReq = exuOutToRfReq(io.in, fp = false)

  val wbFpReq = exuOutToRfReq(io.in, fp = true)

  for(i <- io.in.indices){
    val intReqIdx = wbIntReq.map(_._2).indexOf(i)
    val fpReqIdx = wbFpReq.map(_._2).indexOf(i)

    val wbInt = intReqIdx >= 0
    val wbFp = fpReqIdx >= 0

    val iReq = if(wbInt) wbIntReq(intReqIdx)._1 else null
    val fReq = if(wbFp) wbFpReq(fpReqIdx)._1 else null

    if(wbInt && wbFp){
      io.in(i).ready := Mux(iReq.valid,
        iReq.ready,
        Mux(fReq.valid,
          fReq.ready,
          true.B
        )
      )
      assert(!(iReq.valid && fReq.valid), s"Error: iReq and fReq valid at same time, idx=$i")
    } else if(wbInt){
      io.in(i).ready := Mux(iReq.valid, iReq.ready, true.B)
    } else if(wbFp){
      io.in(i).ready := Mux(fReq.valid, fReq.ready, true.B)
    } else {
      // store
      io.in(i).ready := true.B
    }
  }

  if(wbIntIdx.length < NRIntWritePorts){
    io.toIntRf.take(wbIntIdx.length).zip(wbIntReq.map(_._1)).foreach(x => {
      x._1.bits := x._2.bits
      x._1.valid := x._2.valid
      x._2.ready := true.B
    })
    io.toIntRf.drop(wbIntIdx.length).foreach(_ <> DontCare)
  } else {
    val intArb = Module(new WriteBackArbMtoN(wbIntIdx.length, NRIntWritePorts))
    intArb.io.in <> wbIntReq.map(_._1)
    io.toIntRf <> intArb.io.out
  }

  if(wbFpIdx.length < NRFpWritePorts){
    io.toFpRf.take(wbFpIdx.length).zip(wbFpReq.map(_._1)).foreach(x => {
      x._1.bits := x._2.bits
      x._1.valid := x._2.valid
      x._2.ready := true.B
    })
    io.toFpRf.drop(wbFpIdx.length).foreach(_ <> DontCare)
  } else {
    val fpArb = Module(new WriteBackArbMtoN(wbFpIdx.length, NRFpWritePorts))
    fpArb.io.in <> wbFpReq.map(_._1)
    io.toFpRf <> fpArb.io.out
  }

  io.toRoq.zip(io.in).foreach({
    case(roq, in) =>
      roq.valid := in.fire() && !in.bits.redirectValid
      roq.bits := in.bits
  })

}
