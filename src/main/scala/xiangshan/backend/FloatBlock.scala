package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.regfile.Regfile
import xiangshan.backend.exu._
import xiangshan.backend.issue.ReservationStationNew


class FpBlockToCtrlIO extends XSBundle {
  val wbRegs = Vec(NRFpWritePorts, ValidIO(new ExuOutput))
  val numExist = Vec(exuParameters.FpExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
}

class FloatBlock
(
  fastWakeUpIn: Seq[ExuConfig],
  slowWakeUpIn: Seq[ExuConfig],
  fastFpOut: Seq[ExuConfig],
  slowFpOut: Seq[ExuConfig],
  fastIntOut: Seq[ExuConfig],
  slowIntOut: Seq[ExuConfig]
) extends XSModule with HasExeBlockHelper with NeedImpl {
  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToFpBlockIO)
    val toCtrlBlock = new FpBlockToCtrlIO

    val wakeUpIn = new WakeUpBundle(fastWakeUpIn.size, slowWakeUpIn.size)
    val wakeUpFpOut = Flipped(new WakeUpBundle(fastFpOut.size, slowFpOut.size))
    val wakeUpIntOut = Flipped(new WakeUpBundle(fastIntOut.size, slowIntOut.size))
  })

  val redirect = io.fromCtrlBlock.redirect

  val fpRf = Module(new Regfile(
    numReadPorts = NRFpReadPorts,
    numWirtePorts = NRFpWritePorts,
    hasZero = false,
    len = XLEN + 1
  ))

  val fmacExeUnits = Array.tabulate(exuParameters.FmacCnt)(_ => Module(new FmacExeUnit))
  val fmiscExeUnits = Array.tabulate(exuParameters.FmiscCnt)(_ => Module(new FmiscExeUnit))

  val exeUnits = fmacExeUnits ++ fmiscExeUnits

  def needWakeup(cfg: ExuConfig): Boolean =
    (cfg.readIntRf && cfg.writeIntRf) || (cfg.readFpRf && cfg.writeFpRf)

  def needData(a: ExuConfig, b: ExuConfig): Boolean =
    (a.readIntRf && b.writeIntRf) || (a.readFpRf && b.writeFpRf)

  val reservedStations = exeUnits.map(_.config).zipWithIndex.map({ case (cfg, i) =>
    var certainLatency = -1
    if (cfg.hasCertainLatency) {
      certainLatency = cfg.latency.latencyVal.get
    }

    val readFpRf = cfg.readFpRf

    val inBlockWbData = exeUnits.filter(e => e.config.hasCertainLatency && readFpRf).map(_.io.toFp.bits.data)
    val writeBackData = inBlockWbData ++ io.wakeUpIn.fast.map(_.bits.data)
    val wakeupCnt = writeBackData.length

    val inBlockListenPorts = exeUnits.filter(e => e.config.hasUncertainlatency && readFpRf).map(_.io.toFp)
    val extraListenPorts = inBlockListenPorts ++ io.wakeUpIn.slow
    val extraListenPortsCnt = extraListenPorts.length

    println(s"${i}: exu:${cfg.name} wakeupCnt: ${wakeupCnt} " +
      s"extraListenPorts: ${extraListenPortsCnt} " +
      s"delay:${certainLatency}"
    )

    val rs = Module(new ReservationStationNew(
      cfg, wakeupCnt, extraListenPortsCnt, fixedDelay = certainLatency, feedback = false
    ))

    rs.io.redirect <> redirect
    rs.io.numExist <> io.toCtrlBlock.numExist(i)
    rs.io.enqCtrl <> io.fromCtrlBlock.enqIqCtrl(i)
    rs.io.enqData <> io.fromCtrlBlock.enqIqData(i)

    rs.io.writeBackedData <> writeBackData
    for ((x, y) <- rs.io.extraListenPorts.zip(extraListenPorts)) {
      x.valid := y.fire()
      x.bits := y.bits
    }

    exeUnits(i).io.redirect <> redirect
    exeUnits(i).io.fromInt <> rs.io.deq
    rs.io.tlbFeedback := DontCare

    rs.suggestName(s"rs_${cfg.name}")

    rs
  })

  for(rs <- reservedStations){
    val inBlockUops = reservedStations.filter(x =>
      x.exuCfg.hasCertainLatency && x.exuCfg.writeFpRf
    ).map(x => {
      val raw = WireInit(x.io.selectedUop)
      raw.valid := x.io.selectedUop.valid && raw.bits.ctrl.fpWen
      raw
    })
    rs.io.broadcastedUops <> inBlockUops ++ io.wakeUpIn.fastUops
  }

  io.wakeUpFpOut.fastUops <> reservedStations.filter(
    rs => fpFastFilter(rs.exuCfg)
  ).map(_.io.selectedUop)

  io.wakeUpFpOut.fast <> exeUnits.filter(
    x => fpFastFilter(x.config)
  ).map(_.io.toFp)

  io.wakeUpFpOut.slow <> exeUnits.filter(
    x => fpSlowFilter(x.config)
  ).map(_.io.toFp)

  io.wakeUpIntOut.fastUops <> reservedStations.filter(
    rs => intFastFilter(rs.exuCfg)
  ).map(_.io.selectedUop)

  io.wakeUpIntOut.fast <> exeUnits.filter(
    x => intFastFilter(x.config)
  ).map(_.io.toInt)

  io.wakeUpIntOut.slow <> exeUnits.filter(
    x => intSlowFilter(x.config)
  ).map(_.io.toInt)


  // read int rf from ctrl block
  fpRf.io.readPorts <> io.fromCtrlBlock.readRf
  // write int rf arbiter
  val fpWbArbiter = Module(new Wb(
    (exeUnits.map(_.config) ++ fastWakeUpIn ++ slowWakeUpIn).map(_.wbIntPriority),
    NRFpWritePorts
  ))
  fpWbArbiter.io.in <> exeUnits.map(_.io.toFp) ++ io.wakeUpIn.fast ++ io.wakeUpIn.slow

  // set busytable and update roq
  io.toCtrlBlock.wbRegs <> fpWbArbiter.io.out

  fpRf.io.writePorts.zip(fpWbArbiter.io.out).foreach{
    case (rf, wb) =>
      rf.wen := wb.valid && wb.bits.uop.ctrl.fpWen
      rf.addr := wb.bits.uop.pdest
      rf.data := wb.bits.data
  }

}
