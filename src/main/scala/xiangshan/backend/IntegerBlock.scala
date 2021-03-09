package xiangshan.backend

import chisel3._
import chisel3.util._
import utils.XSPerf
import xiangshan._
import xiangshan.backend.exu.Exu.{jumpExeUnitCfg, ldExeUnitCfg, stExeUnitCfg}
import xiangshan.backend.exu._
import xiangshan.backend.issue.ReservationStation
import xiangshan.backend.fu.{FenceToSbuffer, CSRFileIO}
import xiangshan.backend.regfile.Regfile

class WakeUpBundle(numFast: Int, numSlow: Int) extends XSBundle {
  val fastUops = Vec(numFast, Flipped(ValidIO(new MicroOp)))
  val fast = Vec(numFast, Flipped(ValidIO(new ExuOutput))) //one cycle later than fastUops
  val slow = Vec(numSlow, Flipped(DecoupledIO(new ExuOutput)))

  override def cloneType = (new WakeUpBundle(numFast, numSlow)).asInstanceOf[this.type]

}

class IntBlockToCtrlIO extends XSBundle {
  // write back regfile signals after arbiter
  // used to update busytable and roq state
  val wbRegs = Vec(NRIntWritePorts, ValidIO(new ExuOutput))
  // write back to brq
  val exuRedirect = Vec(exuParameters.AluCnt + exuParameters.JmpCnt, ValidIO(new ExuOutput))
  val numExist = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
}

trait HasExeBlockHelper {
  def fpUopValid(x: ValidIO[MicroOp]): ValidIO[MicroOp] = {
    val uop = WireInit(x)
    uop.valid := x.valid && x.bits.ctrl.fpWen
    uop
  }
  def fpOutValid(x: ValidIO[ExuOutput]): ValidIO[ExuOutput] = {
    val out = WireInit(x)
    out.valid := x.valid && x.bits.uop.ctrl.fpWen
    out
  }
  def fpOutValid(x: DecoupledIO[ExuOutput], connectReady: Boolean = false): DecoupledIO[ExuOutput] = {
    val out = WireInit(x)
    if(connectReady) x.ready := out.ready
    out.valid := x.valid && x.bits.uop.ctrl.fpWen
    out
  }
  def intUopValid(x: ValidIO[MicroOp]): ValidIO[MicroOp] = {
    val uop = WireInit(x)
    uop.valid := x.valid && x.bits.ctrl.rfWen
    uop
  }
  def intOutValid(x: ValidIO[ExuOutput]): ValidIO[ExuOutput] = {
    val out = WireInit(x)
    out.valid := x.valid && !x.bits.uop.ctrl.fpWen
    out
  }
  def intOutValid(x: DecoupledIO[ExuOutput], connectReady: Boolean = false): DecoupledIO[ExuOutput] = {
    val out = WireInit(x)
    if(connectReady) x.ready := out.ready
    out.valid := x.valid && !x.bits.uop.ctrl.fpWen
    out
  }
  def decoupledIOToValidIO[T <: Data](d: DecoupledIO[T]): Valid[T] = {
    val v = Wire(Valid(d.bits.cloneType))
    v.valid := d.valid
    v.bits := d.bits
    v
  }

  def validIOToDecoupledIO[T <: Data](v: Valid[T]): DecoupledIO[T] = {
    val d = Wire(DecoupledIO(v.bits.cloneType))
    d.valid := v.valid
    d.ready := true.B
    d.bits := v.bits
    d
  }
}

class IntegerBlock
(
  fastWakeUpIn: Seq[ExuConfig],
  slowWakeUpIn: Seq[ExuConfig],
  fastWakeUpOut: Seq[ExuConfig],
  slowWakeUpOut: Seq[ExuConfig]
) extends XSModule with HasExeBlockHelper {
  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToIntBlockIO)
    val toCtrlBlock = new IntBlockToCtrlIO
    val toMemBlock = new IntBlockToMemBlockIO

    val wakeUpIn = new WakeUpBundle(fastWakeUpIn.size, slowWakeUpIn.size)
    val wakeUpOut = Flipped(new WakeUpBundle(fastWakeUpOut.size, slowWakeUpOut.size))

    val csrio = new CSRFileIO
    val fenceio = new Bundle {
      val sfence = Output(new SfenceBundle) // to front,mem
      val fencei = Output(Bool()) // to icache
      val sbuffer = new FenceToSbuffer // to mem
    }
  })
  val difftestIO = IO(new Bundle() {
    val fromCSR = new Bundle() {
      val intrNO = Output(UInt(64.W))
      val cause = Output(UInt(64.W))
      val priviledgeMode = Output(UInt(2.W))
      val mstatus = Output(UInt(64.W))
      val sstatus = Output(UInt(64.W))
      val mepc = Output(UInt(64.W))
      val sepc = Output(UInt(64.W))
      val mtval = Output(UInt(64.W))
      val stval = Output(UInt(64.W))
      val mtvec = Output(UInt(64.W))
      val stvec = Output(UInt(64.W))
      val mcause = Output(UInt(64.W))
      val scause = Output(UInt(64.W))
      val satp = Output(UInt(64.W))
      val mip = Output(UInt(64.W))
      val mie = Output(UInt(64.W))
      val mscratch = Output(UInt(64.W))
      val sscratch = Output(UInt(64.W))
      val mideleg = Output(UInt(64.W))
      val medeleg = Output(UInt(64.W))
    }
  })
  difftestIO <> DontCare

  val redirect = io.fromCtrlBlock.redirect
  val flush = io.fromCtrlBlock.flush

  val intRf = Module(new Regfile(
    numReadPorts = NRIntReadPorts,
    numWirtePorts = NRIntWritePorts,
    hasZero = true,
    len = XLEN
  ))

  val jmpExeUnit = Module(new JumpExeUnit)
  val mduExeUnits = Array.tabulate(exuParameters.MduCnt)(_ => Module(new MulDivExeUnit))
  val aluExeUnits = Array.tabulate(exuParameters.AluCnt)(_ => Module(new AluExeUnit))

  val exeUnits = jmpExeUnit +: (mduExeUnits ++ aluExeUnits)

  def needWakeup(cfg: ExuConfig): Boolean =
    (cfg.readIntRf && cfg.writeIntRf) || (cfg.readFpRf && cfg.writeFpRf)

  def needData(a: ExuConfig, b: ExuConfig): Boolean =
    (a.readIntRf && b.writeIntRf) || (a.readFpRf && b.writeFpRf)

  // val readPortIndex = RegNext(io.fromCtrlBlock.readPortIndex)
  val readPortIndex = Seq(1, 2, 3, 0, 1, 2, 3)
  val reservationStations = exeUnits.map(_.config).zipWithIndex.map({ case (cfg, i) =>
    var certainLatency = -1
    if (cfg.hasCertainLatency) {
      certainLatency = cfg.latency.latencyVal.get
    }

    val readIntRf = cfg.readIntRf

    val inBlockWbData = exeUnits.filter(e => e.config.hasCertainLatency).map(a => (a.config, a.io.out.bits.data))
    val fastDatas = inBlockWbData ++ fastWakeUpIn.zip(io.wakeUpIn.fast.map(_.bits.data))
    val fastPortsCnt = fastDatas.length

    val inBlockListenPorts = exeUnits.filter(e => e.config.hasUncertainlatency).map(a => (a.config, a.io.out))
    val slowPorts = (inBlockListenPorts ++ slowWakeUpIn.zip(io.wakeUpIn.slow)).map(a => (a._1, decoupledIOToValidIO(a._2)))
    val extraListenPortsCnt = slowPorts.length

    val feedback = (cfg == ldExeUnitCfg) || (cfg == stExeUnitCfg)

    println(s"${i}: exu:${cfg.name} fastPortsCnt: ${fastPortsCnt} slowPorts: ${extraListenPortsCnt} delay:${certainLatency} feedback:${feedback}")

    val rs = Module(new ReservationStation(s"rs_${cfg.name}", cfg, XLEN,
      fastDatas.map(_._1),
      slowPorts.map(_._1),
      fixedDelay = certainLatency,
      fastWakeup = certainLatency >= 0,
      feedback = feedback
    ))

    rs.io.redirect <> redirect
    rs.io.flush <> flush // TODO: remove it
    rs.io.numExist <> io.toCtrlBlock.numExist(i)
    rs.io.fromDispatch <> io.fromCtrlBlock.enqIqCtrl(i)

    rs.io.srcRegValue := DontCare
    val src1Value = VecInit((0 until 4).map(i => intRf.io.readPorts(i * 2).data))
    val src2Value = VecInit((0 until 4).map(i => intRf.io.readPorts(i * 2 + 1).data))
    rs.io.srcRegValue(0) := src1Value(readPortIndex(i))
    if (cfg.intSrcCnt > 1) rs.io.srcRegValue(1) := src2Value(readPortIndex(i))
    if (cfg == Exu.jumpExeUnitCfg) {
      rs.io.jumpPc := io.fromCtrlBlock.jumpPc
      rs.io.jalr_target := io.fromCtrlBlock.jalr_target
    }

    rs.io.fastDatas <> fastDatas.map(_._2)
    rs.io.slowPorts <> slowPorts.map(_._2)

    exeUnits(i).io.redirect <> redirect
    exeUnits(i).io.fromInt <> rs.io.deq
    exeUnits(i).io.flush <> flush
    // rs.io.memfeedback := DontCare

    rs.suggestName(s"rs_${cfg.name}")

    rs
  })

  for (rs <- reservationStations) {
    val inBlockUops = reservationStations.filter(x =>
      x.exuCfg.hasCertainLatency && x.exuCfg.writeIntRf
    ).map(x => {
      val raw = WireInit(x.io.fastUopOut)
      raw.valid := x.io.fastUopOut.valid && raw.bits.ctrl.rfWen
      raw
    })
    rs.io.fastUopsIn <> inBlockUops ++ io.wakeUpIn.fastUops
  }

  io.wakeUpOut.fastUops <> reservationStations.filter(
    rs => rs.exuCfg.hasCertainLatency
  ).map(_.io.fastUopOut).map(intUopValid)

  io.wakeUpOut.fast <> exeUnits.filter(
    x => x.config.hasCertainLatency
  ).map(_.io.out).map(decoupledIOToValidIO)

  io.wakeUpOut.slow <> exeUnits.filter(
    x => x.config.hasUncertainlatency
  ).map(x => WireInit(x.io.out))

  // send misprediction to brq
  io.toCtrlBlock.exuRedirect.zip(
    exeUnits.filter(_.config.hasRedirect).map(_.io.out)
  ).foreach {
    case (x, y) =>
      x.valid := y.fire() && y.bits.redirectValid
      x.bits := y.bits
  }

  jmpExeUnit.csrio <> io.csrio
  jmpExeUnit.fenceio <> io.fenceio
  if (!env.FPGAPlatform) {
    jmpExeUnit.difftestIO.fromCSR <> difftestIO.fromCSR
  }

  // read int rf from ctrl block
  intRf.io.readPorts.zipWithIndex.map { case (r, i) => r.addr := io.fromCtrlBlock.readRf(i) }
  (0 until NRMemReadPorts).foreach(i => io.toMemBlock.readIntRf(i).data := intRf.io.readPorts(i + 8).data)
  // write int rf arbiter
  val intWbArbiter = Module(new Wb(
    (exeUnits.map(_.config) ++ fastWakeUpIn ++ slowWakeUpIn),
    NRIntWritePorts,
    isFp = false
  ))
  intWbArbiter.io.in <> exeUnits.map(e => {
    val w = WireInit(e.io.out)
    if(e.config.writeFpRf){
      w.valid := e.io.out.valid && !e.io.out.bits.uop.ctrl.fpWen && io.wakeUpOut.slow(0).ready
    } else {
      w.valid := e.io.out.valid
    }
    w
  }) ++ io.wakeUpIn.slow.map(x => intOutValid(x, connectReady = true))

  XSPerf("competition", intWbArbiter.io.in.map(i => !i.ready && i.valid).foldRight(0.U)(_+_))  
  
  exeUnits.zip(intWbArbiter.io.in).foreach{
    case (exu, wInt) =>
      if(exu.config.writeFpRf){
        val wakeUpOut = io.wakeUpOut.slow(0) // jmpExeUnit
        val writeFpReady = wakeUpOut.fire() && wakeUpOut.bits.uop.ctrl.fpWen
        exu.io.out.ready := wInt.fire() || writeFpReady || !exu.io.out.valid
      } else {
        exu.io.out.ready := wInt.fire() || !exu.io.out.valid
      }
  }

  // set busytable and update roq
  io.toCtrlBlock.wbRegs <> intWbArbiter.io.out

  intRf.io.writePorts.zip(intWbArbiter.io.out).foreach {
    case (rf, wb) =>
      rf.wen := wb.valid && wb.bits.uop.ctrl.rfWen
      rf.addr := wb.bits.uop.pdest
      rf.data := wb.bits.data
  }
}
