package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.regfile.Regfile
import xiangshan.backend.exu._
import xiangshan.backend.issue.{ReservationStationCtrl, ReservationStationData}


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
) extends XSModule with HasExeBlockHelper {
  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToFpBlockIO)
    val toCtrlBlock = new FpBlockToCtrlIO
    val toMemBlock = new FpBlockToMemBlockIO

    val wakeUpIn = new WakeUpBundle(fastWakeUpIn.size, slowWakeUpIn.size)
    val wakeUpFpOut = Flipped(new WakeUpBundle(fastFpOut.size, slowFpOut.size))
    val wakeUpIntOut = Flipped(new WakeUpBundle(fastIntOut.size, slowIntOut.size))

    // from csr
    val frm = Input(UInt(3.W))
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

  fmacExeUnits.foreach(_.frm := io.frm)
  fmiscExeUnits.foreach(_.frm := io.frm)

  val exeUnits = fmacExeUnits ++ fmiscExeUnits

  def needWakeup(cfg: ExuConfig): Boolean =
    (cfg.readIntRf && cfg.writeIntRf) || (cfg.readFpRf && cfg.writeFpRf)

  def needData(a: ExuConfig, b: ExuConfig): Boolean =
    (a.readIntRf && b.writeIntRf) || (a.readFpRf && b.writeFpRf)

  val readPortIndex = RegNext(io.fromCtrlBlock.readPortIndex)
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

    val rsCtrl = Module(new ReservationStationCtrl(cfg, wakeupCnt, extraListenPortsCnt, fixedDelay = certainLatency, feedback = false))
    val rsData = Module(new ReservationStationData(cfg, wakeupCnt, extraListenPortsCnt, fixedDelay = certainLatency, feedback = false))

    rsCtrl.io.data <> rsData.io.ctrl
    rsCtrl.io.redirect <> redirect // TODO: remove it
    rsCtrl.io.numExist <> io.toCtrlBlock.numExist(i)
    rsCtrl.io.enqCtrl <> io.fromCtrlBlock.enqIqCtrl(i)

    rsData.io.srcRegValue := DontCare
    val src1Value = VecInit((0 until 4).map(i => fpRf.io.readPorts(i * 3).data))
    val src2Value = VecInit((0 until 4).map(i => fpRf.io.readPorts(i * 3 + 1).data))
    val src3Value = VecInit((0 until 4).map(i => fpRf.io.readPorts(i * 3 + 2).data))
    
    rsData.io.srcRegValue(0) := src1Value(readPortIndex(i))
    rsData.io.srcRegValue(1) := src2Value(readPortIndex(i))
    if (cfg.fpSrcCnt > 2) rsData.io.srcRegValue(2) := src3Value(readPortIndex(i))
    rsData.io.redirect <> redirect

    rsData.io.writeBackedData <> writeBackData
    for ((x, y) <- rsData.io.extraListenPorts.zip(extraListenPorts)) {
      x.valid := y.fire()
      x.bits := y.bits
    }

    exeUnits(i).io.redirect <> redirect
    exeUnits(i).io.fromFp <> rsData.io.deq
    rsData.io.feedback := DontCare

    rsCtrl.suggestName(s"rsc_${cfg.name}")
    rsData.suggestName(s"rsd_${cfg.name}")

    rsData
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
  ).map(_.io.selectedUop).map(fpValid)

  io.wakeUpFpOut.fast <> exeUnits.filter(
    x => fpFastFilter(x.config)
  ).map(_.io.toFp)

  io.wakeUpFpOut.slow <> exeUnits.filter(
    x => fpSlowFilter(x.config)
  ).map(_.io.toFp)

  io.wakeUpIntOut.fastUops <> reservedStations.filter(
    rs => intFastFilter(rs.exuCfg)
  ).map(_.io.selectedUop).map(intValid)

  io.wakeUpIntOut.fast <> exeUnits.filter(
    x => intFastFilter(x.config)
  ).map(_.io.toInt)

  io.wakeUpIntOut.slow <> exeUnits.filter(
    x => intSlowFilter(x.config)
  ).map(_.io.toInt)


  // read fp rf from ctrl block
  fpRf.io.readPorts.zipWithIndex.map{ case (r, i) => r.addr := io.fromCtrlBlock.readRf(i) }
  (0 until exuParameters.StuCnt).foreach(i => io.toMemBlock.readFpRf(i).data := fpRf.io.readPorts(i + 12).data)
  // write fp rf arbiter
  val fpWbArbiter = Module(new Wb(
    (exeUnits.map(_.config) ++ fastWakeUpIn ++ slowWakeUpIn),
    NRFpWritePorts,
    isFp = true
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