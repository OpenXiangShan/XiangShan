package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.regfile.Regfile
import xiangshan.backend.exu._
import xiangshan.backend.issue.{ReservationStation}


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
  val flush = io.fromCtrlBlock.flush

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

    val inBlockWbData = exeUnits.filter(e => e.config.hasCertainLatency && readFpRf).map(_.io.toFp.bits.data)
    val writeBackData = inBlockWbData ++ io.wakeUpIn.fast.map(_.bits.data)
    val fastPortsCnt = writeBackData.length

    val inBlockListenPorts = exeUnits.filter(e => e.config.hasUncertainlatency && readFpRf).map(_.io.toFp)
    val slowPorts = inBlockListenPorts ++ io.wakeUpIn.slow
    val slowPortsCnt = slowPorts.length

    println(s"${i}: exu:${cfg.name} fastPortsCnt: ${fastPortsCnt} " +
      s"slowPorts: ${slowPortsCnt} " +
      s"delay:${certainLatency}"
    )

    val rs = Module(new ReservationStation(cfg, XLEN + 1, fastPortsCnt, slowPortsCnt, fixedDelay = certainLatency, fastWakeup = certainLatency >= 0, feedback = false))

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

    rs.io.fastDatas <> writeBackData
    for ((x, y) <- rs.io.slowPorts.zip(slowPorts)) {
      x.valid := y.fire()
      x.bits := y.bits
    }

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
    rs.io.fastUopsIn <> inBlockUops ++  io.wakeUpIn.fastUops
  }

  io.wakeUpFpOut.fastUops <> reservedStations.filter(
    rs => fpFastFilter(rs.exuCfg)
  ).map(_.io.fastUopOut).map(fpValid)

  io.wakeUpFpOut.fast <> exeUnits.filter(
    x => fpFastFilter(x.config)
  ).map(_.io.toFp)

  io.wakeUpFpOut.slow <> exeUnits.filter(
    x => fpSlowFilter(x.config)
  ).map(_.io.toFp)

  io.wakeUpIntOut.fastUops <> reservedStations.filter(
    rs => intFastFilter(rs.exuCfg)
  ).map(_.io.fastUopOut).map(intValid)

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