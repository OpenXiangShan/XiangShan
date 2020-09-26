package xiangshan.backend.exu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class Wbu(exuConfigs: Array[ExuConfig]) extends XSModule{

  val io = IO(new Bundle() {
    val in  = Vec(exuParameters.ExuCnt, Flipped(DecoupledIO(new ExuOutput)))
    val toRoq = Vec(exuParameters.ExuCnt, ValidIO(new ExuOutput))
    val toIntRf = Vec(NRIntWritePorts, ValidIO(new ExuOutput))
    val toFpRf = Vec(NRFpWritePorts, ValidIO(new ExuOutput))
  })

  require(io.in.length == exuConfigs.length)


  def exuOutToRfReq
  (exuOut: DecoupledIO[ExuOutput], fp: Boolean): DecoupledIO[ExuOutput] = {
    val req = Wire(Decoupled(new ExuOutput))
    req.valid := exuOut.valid && {
      if(fp) exuOut.bits.uop.ctrl.fpWen else exuOut.bits.uop.ctrl.rfWen
    }
    req.bits := exuOut.bits
    req
  }

  // ((ExuOutput, ExuConfig), index) => ((WbReq, ExuConfig), index)
  val wbInt = io.in.zip(exuConfigs).zipWithIndex.
    filter(_._1._2.writeIntRf).map(x =>
    ((exuOutToRfReq(x._1._1, fp = false), x._1._2), x._2))

  val wbIntReq = wbInt.map(_._1)

  val wbFp = io.in.zip(exuConfigs).zipWithIndex.
    filter(_._1._2.writeFpRf).map(x =>
      ((exuOutToRfReq(x._1._1, fp = true), x._1._2), x._2))

  val wbFpReq = wbFp.map(_._1)

  for(i <- io.in.indices){
    val writeIntReqIdx = wbInt.map(_._2).indexOf(i)
    val writeFpReqIdx = wbFp.map(_._2).indexOf(i)
    val writeIntRf = writeIntReqIdx >= 0
    val writeFpRf = writeFpReqIdx >= 0

    val iReq = if(writeIntRf) wbIntReq(writeIntReqIdx)._1 else null
    val fReq = if(writeFpRf) wbFpReq(writeFpReqIdx)._1 else null

    if(writeIntRf && writeFpRf){
      io.in(i).ready := Mux(iReq.valid,
        iReq.ready,
        Mux(fReq.valid,
          fReq.ready,
          true.B
        )
      )
      assert(!(iReq.valid && fReq.valid), s"Error: iReq and fReq valid at same time, idx=$i")
    } else if(writeIntRf){
      io.in(i).ready := iReq.ready
    } else if(writeFpRf){
      io.in(i).ready := fReq.ready
    } else {
      io.in(i).ready := true.B
    }

    exuConfigs(i) match {
      case Exu.aluExeUnitCfg =>
        io.toRoq(i).valid := io.in(i).fire() && !io.in(i).bits.redirectValid
      case Exu.jmpExeUnitCfg =>
        io.toRoq(i).valid := io.in(i).fire() && !io.in(i).bits.redirectValid
      case _ =>
        io.toRoq(i).valid := io.in(i).fire()
    }
    io.toRoq(i).bits := io.in(i).bits
  }

  def directConnect(rfWrite: Valid[ExuOutput], wbReq: DecoupledIO[ExuOutput]) = {
    rfWrite.bits := wbReq.bits
    rfWrite.valid := wbReq.valid
    wbReq.ready := true.B
  }

  def splitN[T](in: Seq[T], n: Int): Seq[Option[Seq[T]]] = {
    require(n > 0)
    if(in.size < n) Seq(Some(in)) ++ Seq.fill(n-1)(None)
    else {
      val m = in.size/n
      Some(in.take(m)) +: splitN(in.drop(m), n-1)
    }
  }

  if(wbIntReq.size <= NRIntWritePorts){ // write ports are enough
    io.toIntRf.
      take(wbIntReq.size).
      zip(wbIntReq).
      foreach(x => directConnect(x._1, x._2._1))

    if(wbIntReq.size < NRIntWritePorts){
      println(s"Warrning: ${NRIntWritePorts-wbIntReq.size} int write ports are not used!")
      io.toIntRf.drop(wbIntReq.size).foreach(_ <> DontCare)
    }
  } else {
    val directReq = wbIntReq.filter(w => Seq(Exu.ldExeUnitCfg, Exu.aluExeUnitCfg).contains(w._2))
    val mulReq = wbIntReq.filter(w => Seq(Exu.mulExeUnitCfg, Exu.mulDivExeUnitCfg, Exu.mulDivFenceExeUnitCfg).contains(w._2))
    val otherReq = splitN(
      wbIntReq.filterNot(w => Seq(
        Exu.ldExeUnitCfg, Exu.aluExeUnitCfg, Exu.mulDivExeUnitCfg, Exu.mulExeUnitCfg, Exu.mulDivFenceExeUnitCfg
      ).contains(w._2)),
      mulReq.size
    )
    require(directReq.size + mulReq.size <= NRIntWritePorts)
    // alu && load: direct connect
    io.toIntRf.take(directReq.size).zip(directReq).foreach(x => directConnect(x._1, x._2._1))
    for( i <- mulReq.indices){
      val arbiter = Module(new Arbiter(new ExuOutput, 1+otherReq(i).getOrElse(Seq()).size))
      arbiter.io.in <> (mulReq(i) +: otherReq(i).getOrElse(Seq())).map(_._1)
      io.toIntRf.drop(directReq.size)(i) := arbiter.io.out
      arbiter.io.out.ready := true.B
    }
    if(directReq.size + mulReq.size < NRIntWritePorts){
      println(s"Warrning: ${NRIntWritePorts-directReq.size-mulReq.size} int write ports are not used!")
      io.toIntRf.drop(directReq.size + mulReq.size).foreach(_ <> DontCare)
    }
  }

  if(wbFpReq.size <= NRFpWritePorts){
    io.toFpRf.
      take(wbFpReq.size).
      zip(wbFpReq).
      foreach(x => directConnect(x._1, x._2._1))

    if(wbFpReq.size < NRFpWritePorts){
      println(s"Warrning: ${NRFpWritePorts-wbFpReq.size} fp write ports are not used!")
      io.toFpRf.drop(wbFpReq.size).foreach(_ <> DontCare)
    }
  } else {
    val directReq = wbFpReq.filter(w => Seq(Exu.ldExeUnitCfg, Exu.fmacExeUnitCfg).contains(w._2))
    val fmiscReq = wbFpReq.filter(w => Seq(Exu.fmiscExeUnitCfg, Exu.fmiscDivExeUnitCfg).contains(w._2))
    val otherReq = splitN(
      wbFpReq.filterNot(w => Seq(
        Exu.ldExeUnitCfg, Exu.fmacExeUnitCfg, Exu.fmiscExeUnitCfg, Exu.fmiscDivExeUnitCfg
      ).contains(w._2)),
      fmiscReq.size
    )
    require(directReq.size + fmiscReq.size <= NRFpWritePorts)
    io.toFpRf.take(directReq.size).zip(directReq).foreach(x => directConnect(x._1, x._2._1))
    for( i <- fmiscReq.indices){
      val arbiter = Module(new Arbiter(new ExuOutput, 1+otherReq(i).getOrElse(Seq()).size))
      arbiter.io.in <> (fmiscReq(i) +: otherReq(i).getOrElse(Seq())).map(_._1)
      io.toFpRf.drop(directReq.size)(i) := arbiter.io.out
      arbiter.io.out.ready := true.B
    }
    if(directReq.size + fmiscReq.size < NRFpWritePorts){
      println(s"Warrning: ${NRFpWritePorts-directReq.size-fmiscReq.size} fp write ports are not used!")
      io.toFpRf.drop(directReq.size + fmiscReq.size).foreach(_ <> DontCare)
    }
  }
}
