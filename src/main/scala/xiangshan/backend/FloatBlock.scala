package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.regfile.Regfile
import xiangshan.backend.exu._
import xiangshan.backend.issue.ReservationStation
import xiangshan.mem.{HasFpLoadHelper, HasLoadHelper}


class FpBlockToCtrlIO extends XSBundle {
  val wbRegs = Vec(NRFpWritePorts, ValidIO(new ExuOutput))
  val numExist = Vec(exuParameters.FpExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
}

class FloatBlock
(
  intSlowWakeUpIn: Seq[ExuConfig],
  memSlowWakeUpIn: Seq[ExuConfig],
  fastWakeUpOut: Seq[ExuConfig],
  slowWakeUpOut: Seq[ExuConfig],
) extends XSModule with HasExeBlockHelper with HasFpLoadHelper {
  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToFpBlockIO)
    val toCtrlBlock = new FpBlockToCtrlIO
    val toMemBlock = new FpBlockToMemBlockIO

    val intWakeUpFp = Vec(intSlowWakeUpIn.size, Flipped(DecoupledIO(new ExuOutput)))
    val memWakeUpFp = Vec(memSlowWakeUpIn.size, Flipped(DecoupledIO(new ExuOutput)))
    val wakeUpOut = Flipped(new WakeUpBundle(fastWakeUpOut.size, slowWakeUpOut.size))
    val intWakeUpOut = Vec(intSlowWakeUpIn.size, DecoupledIO(new ExuOutput))

    // from csr
    val frm = Input(UInt(3.W))
  })

  val redirect = io.fromCtrlBlock.redirect
  val flush = io.fromCtrlBlock.flush

  val intWakeUpFpReg = Wire(Vec(intSlowWakeUpIn.size, Flipped(DecoupledIO(new ExuOutput))))
  for((w, r) <- io.intWakeUpFp.zip(intWakeUpFpReg)){
    val in = WireInit(w)
    w.ready := in.ready
    in.valid := w.valid && !w.bits.uop.roqIdx.needFlush(redirect, flush)
    PipelineConnect(in, r, r.fire() || r.bits.uop.roqIdx.needFlush(redirect, flush), false.B)
  }
  // to memBlock's store rs
  io.intWakeUpOut <> intWakeUpFpReg.map(x => WireInit(x))

  val intRecoded = intWakeUpFpReg.map(x => {
    val rec = Wire(DecoupledIO(new ExuOutput))
    rec.valid := x.valid && x.bits.uop.ctrl.fpWen
    rec.bits := x.bits
    rec.bits.data := Mux(x.bits.uop.ctrl.fpu.typeTagOut === S,
      recode(x.bits.data(31, 0), S),
      recode(x.bits.data(63, 0), D)
    )
    rec.bits.redirectValid := false.B
    x.ready := rec.ready || !rec.valid
    rec
  })

  val memRecoded = WireInit(io.memWakeUpFp)
  for((rec, reg) <- memRecoded.zip(io.memWakeUpFp)){
    rec.bits.data := fpRdataHelper(reg.bits.uop, reg.bits.data)
    rec.bits.redirectValid := false.B
    reg.ready := true.B
  }
  val wakeUpInRecode = intRecoded ++ memRecoded

  val fpRf = Module(new Regfile(
    numReadPorts = NRFpReadPorts,
    numWirtePorts = NRFpWritePorts,
    hasZero = false,
    len = XLEN + 1
  ))

  val fmacExeUnits = Array.tabulate(exuParameters.FmacCnt)(_ => Module(new FmacExeUnit))
  val fmiscExeUnits = Array.tabulate(exuParameters.FmiscCnt)(_ => Module(new FmiscExeUnit))

  fmacExeUnits.foreach(_.frm := io.frm)
  fmiscExeUnits.foreach(_.frm := io.frm)

  val exeUnits = fmacExeUnits ++ fmiscExeUnits

  def needWakeup(cfg: ExuConfig): Boolean =
    (cfg.readIntRf && cfg.writeIntRf) || (cfg.readFpRf && cfg.writeFpRf)

  def needData(a: ExuConfig, b: ExuConfig): Boolean =
    (a.readIntRf && b.writeIntRf) || (a.readFpRf && b.writeFpRf)

  // val readPortIndex = RegNext(io.fromCtrlBlock.readPortIndex)
  val readPortIndex = Seq(0, 1, 2, 3, 2, 3)
  val reservedStations = exeUnits.map(_.config).zipWithIndex.map({ case (cfg, i) =>
    var certainLatency = -1
    if (cfg.hasCertainLatency) {
      certainLatency = cfg.latency.latencyVal.get
    }

    val readFpRf = cfg.readFpRf
    val wakeUpInRecodeWithCfg = intSlowWakeUpIn.zip(intRecoded) ++ memSlowWakeUpIn.zip(memRecoded)

    val inBlockFastPorts = exeUnits.filter(e => e.config.hasCertainLatency).map(a => (a.config, a.io.out.bits.data))
    val fastPortsCnt = inBlockFastPorts.length

    val inBlockListenPorts = exeUnits.filter(e => e.config.hasUncertainlatency).map(a => (a.config, a.io.out))
    val slowPorts = (inBlockListenPorts ++ wakeUpInRecodeWithCfg).map(a => (a._1, decoupledIOToValidIO(a._2)))
    val slowPortsCnt = slowPorts.length

    println(s"${i}: exu:${cfg.name} fastPortsCnt: ${fastPortsCnt} " +
      s"slowPorts: ${slowPortsCnt} " +
      s"delay:${certainLatency}"
    )

    val rs = Module(new ReservationStation(s"rs_${cfg.name}", cfg, IssQueSize, XLEN + 1,
      inBlockFastPorts.map(_._1),
      slowPorts.map(_._1),
      fixedDelay = certainLatency,
      fastWakeup = certainLatency >= 0,
      feedback = false
    ))

    rs.io.redirect <> redirect // TODO: remove it
    rs.io.flush <> flush // TODO: remove it
    rs.io.numExist <> io.toCtrlBlock.numExist(i)
    rs.io.fromDispatch <> io.fromCtrlBlock.enqIqCtrl(i)

    rs.io.srcRegValue := DontCare
    val src1Value = VecInit((0 until 4).map(i => fpRf.io.readPorts(i * 3).data))
    val src2Value = VecInit((0 until 4).map(i => fpRf.io.readPorts(i * 3 + 1).data))
    val src3Value = VecInit((0 until 4).map(i => fpRf.io.readPorts(i * 3 + 2).data))

    rs.io.srcRegValue(0) := src1Value(readPortIndex(i))
    rs.io.srcRegValue(1) := src2Value(readPortIndex(i))
    if (cfg.fpSrcCnt > 2) rs.io.srcRegValue(2) := src3Value(readPortIndex(i))

    rs.io.fastDatas <> inBlockFastPorts.map(_._2)
    rs.io.slowPorts <> slowPorts.map(_._2)

    exeUnits(i).io.redirect <> redirect
    exeUnits(i).io.flush <> flush
    exeUnits(i).io.fromFp <> rs.io.deq
    // rs.io.memfeedback := DontCare

    rs.suggestName(s"rs_${cfg.name}")

    rs
  })

  for(rs <- reservedStations){
    val inBlockUops = reservedStations.filter(x =>
      x.exuCfg.hasCertainLatency && x.exuCfg.writeFpRf
    ).map(x => {
      val raw = WireInit(x.io.fastUopOut)
      raw.valid := x.io.fastUopOut.valid && raw.bits.ctrl.fpWen
      raw
    })
    rs.io.fastUopsIn <> inBlockUops
  }

  // read fp rf from ctrl block
  fpRf.io.readPorts.zipWithIndex.map{ case (r, i) => r.addr := io.fromCtrlBlock.readRf(i) }
  (0 until exuParameters.StuCnt).foreach(i =>
    io.toMemBlock.readFpRf(i).data := RegNext(ieee(fpRf.io.readPorts(i + 12).data))
  )
  // write fp rf arbiter
  val fpWbArbiter = Module(new Wb(
    exeUnits.map(_.config) ++ intSlowWakeUpIn ++ memSlowWakeUpIn,
    NRFpWritePorts,
    isFp = true
  ))
  fpWbArbiter.io.in.drop(exeUnits.length).zip(wakeUpInRecode).foreach(
    x => x._1 <> fpOutValid(x._2, connectReady = true)
  )

  for((exu, i) <- exeUnits.zipWithIndex){
    val out, outReg = Wire(DecoupledIO(new ExuOutput))
    out.bits := exu.io.out.bits
    out.valid := exu.io.out.valid && !out.bits.uop.roqIdx.needFlush(redirect, flush)
    PipelineConnect(out, outReg,
      outReg.fire() || outReg.bits.uop.roqIdx.needFlush(redirect, flush), false.B
    )
    io.wakeUpOut.slow(i).valid := outReg.valid
    io.wakeUpOut.slow(i).bits := outReg.bits
    io.wakeUpOut.slow(i).bits.redirectValid := false.B
    io.wakeUpOut.slow(i).bits.data := Mux(outReg.bits.uop.ctrl.fpWen,
      ieee(outReg.bits.data),
      outReg.bits.data
    )
    fpWbArbiter.io.in(i).valid := exu.io.out.valid && exu.io.out.bits.uop.ctrl.fpWen && outReg.ready
    fpWbArbiter.io.in(i).bits := exu.io.out.bits
    if(exu.config.writeIntRf){
      outReg.ready := !outReg.valid || (
        io.wakeUpOut.slow(i).ready && outReg.bits.uop.ctrl.rfWen
        ) || outReg.bits.uop.ctrl.fpWen
      // don't consider flush in 'intFire'
      val intFire = exu.io.out.valid && out.ready && out.bits.uop.ctrl.rfWen
      exu.io.out.ready := intFire || fpWbArbiter.io.in(i).fire() || !exu.io.out.valid
    } else {
      outReg.ready := true.B
      exu.io.out.ready := fpWbArbiter.io.in(i).fire() || !exu.io.out.valid
    }
  }

  XSPerf("competition", fpWbArbiter.io.in.map(i => !i.ready && i.valid).foldRight(0.U)(_+_))

  // set busytable and update roq
  io.toCtrlBlock.wbRegs <> fpWbArbiter.io.out

  fpRf.io.writePorts.zip(fpWbArbiter.io.out).foreach{
    case (rf, wb) =>
      rf.wen := wb.valid
      rf.addr := wb.bits.uop.pdest
      rf.data := wb.bits.data
  }

}
