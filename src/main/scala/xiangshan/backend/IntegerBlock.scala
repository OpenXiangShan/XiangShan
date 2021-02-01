package xiangshan.backend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.exu.Exu.{ldExeUnitCfg, stExeUnitCfg}
import xiangshan.backend.exu._
import xiangshan.backend.fu.FenceToSbuffer
import xiangshan.backend.issue.{ReservationStation}
import xiangshan.backend.regfile.Regfile
import xiangshan.backend.roq.RoqExceptionInfo

class WakeUpBundle(numFast: Int, numSlow: Int) extends XSBundle {
  val fastUops = Vec(numFast, Flipped(ValidIO(new MicroOp)))
  val fast = Vec(numFast, Flipped(DecoupledIO(new ExuOutput))) //one cycle later than fastUops
  val slow = Vec(numSlow, Flipped(DecoupledIO(new ExuOutput)))

  override def cloneType = (new WakeUpBundle(numFast, numSlow)).asInstanceOf[this.type]

}

class IntBlockToCtrlIO extends XSBundle {
  // write back regfile signals after arbiter
  // used to update busytable and roq state
  val wbRegs = Vec(NRIntWritePorts, ValidIO(new ExuOutput))
  // write back to brq
  val exuRedirect = Vec(exuParameters.AluCnt+exuParameters.JmpCnt, ValidIO(new ExuOutput))
  val numExist = Vec(exuParameters.IntExuCnt, Output(UInt(log2Ceil(IssQueSize).W)))
}

trait HasExeBlockHelper {
  def fpFastFilter(cfg: ExuConfig): Boolean = {
    cfg.hasCertainLatency && cfg.writeFpRf
  }
  def fpSlowFilter(cfg: ExuConfig): Boolean = {
    cfg.hasUncertainlatency && cfg.writeFpRf
  }
  def intFastFilter(cfg: ExuConfig): Boolean = {
    cfg.hasCertainLatency && cfg.writeIntRf
  }
  def intSlowFilter(cfg: ExuConfig): Boolean = {
    cfg.hasUncertainlatency && cfg.writeIntRf
  }
  def fpValid(x: ValidIO[MicroOp]): ValidIO[MicroOp] = {
	val uop = WireInit(x)
	uop.valid := x.valid && x.bits.ctrl.fpWen
	uop
  }
  def intValid(x: ValidIO[MicroOp]): ValidIO[MicroOp] = {
	val uop = WireInit(x)
	uop.valid := x.valid && x.bits.ctrl.rfWen
	uop
  }
}

class IntegerBlock
(
  fastWakeUpIn: Seq[ExuConfig],
  slowWakeUpIn: Seq[ExuConfig],
  fastFpOut: Seq[ExuConfig],
  slowFpOut: Seq[ExuConfig],
  fastIntOut: Seq[ExuConfig],
  slowIntOut: Seq[ExuConfig]
) extends XSModule with HasExeBlockHelper
{
  val io = IO(new Bundle {
    val fromCtrlBlock = Flipped(new CtrlToIntBlockIO)
    val toCtrlBlock = new IntBlockToCtrlIO
    val toMemBlock = new IntBlockToMemBlockIO

    val wakeUpIn = new WakeUpBundle(fastWakeUpIn.size, slowWakeUpIn.size)
    val wakeUpFpOut = Flipped(new WakeUpBundle(fastFpOut.size, slowFpOut.size))
    val wakeUpIntOut = Flipped(new WakeUpBundle(fastIntOut.size, slowIntOut.size))

    val csrio = new Bundle {
      val fflags = Flipped(Valid(UInt(5.W))) // from roq
      val dirty_fs = Input(Bool()) // from roq
      val frm = Output(UInt(3.W)) // to float
      val exception = Flipped(ValidIO(new RoqExceptionInfo))
      val trapTarget = Output(UInt(VAddrBits.W)) // to roq
      val isXRet = Output(Bool())
      val interrupt = Output(Bool()) // to roq
      val memExceptionVAddr = Input(UInt(VAddrBits.W)) // from lsq
      val externalInterrupt = new ExternalInterruptIO  // from outside
      val tlb = Output(new TlbCsrBundle) // from tlb
      val perfinfo = new Bundle {
        val retiredInstr = Input(UInt(3.W))
      }
    }
    val fenceio = new Bundle {
      val sfence = Output(new SfenceBundle) // to front,mem
      val fencei = Output(Bool())           // to icache
      val sbuffer = new FenceToSbuffer      // to mem
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

  val readPortIndex = RegNext(io.fromCtrlBlock.readPortIndex)
  val reservationStations = exeUnits.map(_.config).zipWithIndex.map({ case (cfg, i) =>
    var certainLatency = -1
    if (cfg.hasCertainLatency) {
      certainLatency = cfg.latency.latencyVal.get
    }

    val readIntRf = cfg.readIntRf

    val inBlockWbData = exeUnits.filter(e => e.config.hasCertainLatency && readIntRf).map(_.io.toInt.bits.data)
    val fastDatas = inBlockWbData ++ io.wakeUpIn.fast.map(_.bits.data)
    val wakeupCnt = fastDatas.length

    val inBlockListenPorts = exeUnits.filter(e => e.config.hasUncertainlatency && readIntRf).map(_.io.toInt)
    val slowPorts = inBlockListenPorts ++ io.wakeUpIn.slow
    val extraListenPortsCnt = slowPorts.length

    val feedback = (cfg == ldExeUnitCfg) || (cfg == stExeUnitCfg)

    println(s"${i}: exu:${cfg.name} wakeupCnt: ${wakeupCnt} slowPorts: ${extraListenPortsCnt} delay:${certainLatency} feedback:${feedback}")

    val rs = Module(new ReservationStation(cfg, wakeupCnt, extraListenPortsCnt, fixedDelay = certainLatency, fastWakeup = certainLatency >= 0, feedback = feedback))

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

    rs.io.fastDatas <> fastDatas
    for ((x, y) <- rs.io.slowPorts.zip(slowPorts)) {
      x.valid := y.fire()
      x.bits := y.bits
    }

    exeUnits(i).io.redirect <> redirect
    exeUnits(i).io.fromInt <> rs.io.deq
    exeUnits(i).io.flush <> flush
    // rs.io.memfeedback := DontCare

    rs.suggestName(s"rs_${cfg.name}")

    rs
  })

  for(rs <- reservationStations){
    val inBlockUops = reservationStations.filter(x =>
      x.exuCfg.hasCertainLatency && x.exuCfg.writeIntRf
    ).map(x => {
      val raw = WireInit(x.io.fastUopOut)
      raw.valid := x.io.fastUopOut.valid && raw.bits.ctrl.rfWen
      raw
    })
    rs.io.fastUopsIn <> inBlockUops ++ io.wakeUpIn.fastUops
  }

  io.wakeUpFpOut.fastUops <> reservationStations.filter(
    rs => fpFastFilter(rs.exuCfg)
  ).map(_.io.fastUopOut).map(fpValid)

  io.wakeUpFpOut.fast <> exeUnits.filter(
    x => fpFastFilter(x.config)
  ).map(_.io.toFp)

  io.wakeUpFpOut.slow <> exeUnits.filter(
    x => fpSlowFilter(x.config)
  ).map(_.io.toFp)

  io.wakeUpIntOut.fastUops <> reservationStations.filter(
    rs => intFastFilter(rs.exuCfg)
  ).map(_.io.fastUopOut).map(intValid)

  io.wakeUpIntOut.fast <> exeUnits.filter(
    x => intFastFilter(x.config)
  ).map(_.io.toInt)

  io.wakeUpIntOut.slow <> exeUnits.filter(
    x => intSlowFilter(x.config)
  ).map(_.io.toInt)

  // send misprediction to brq
  io.toCtrlBlock.exuRedirect.zip(
    exeUnits.filter(_.config.hasRedirect).map(_.io.toInt)
  ).foreach{
    case (x, y) =>
      x.valid := y.fire() && y.bits.redirectValid
      x.bits := y.bits
  }

  jmpExeUnit.csrio <> io.csrio
  jmpExeUnit.fenceio <> io.fenceio
  if (env.DualCoreDifftest) {
    jmpExeUnit.difftestIO.fromCSR <> difftestIO.fromCSR
  }

  // read int rf from ctrl block
  intRf.io.readPorts.zipWithIndex.map{ case(r, i) => r.addr := io.fromCtrlBlock.readRf(i) }
  (0 until NRMemReadPorts).foreach(i => io.toMemBlock.readIntRf(i).data := intRf.io.readPorts(i + 8).data)
  // write int rf arbiter
  val intWbArbiter = Module(new Wb(
    (exeUnits.map(_.config) ++ fastWakeUpIn ++ slowWakeUpIn),
    NRIntWritePorts,
    isFp = false
  ))
  intWbArbiter.io.in <> exeUnits.map(_.io.toInt) ++ io.wakeUpIn.fast ++ io.wakeUpIn.slow

  // set busytable and update roq
  io.toCtrlBlock.wbRegs <> intWbArbiter.io.out

  intRf.io.writePorts.zip(intWbArbiter.io.out).foreach{
    case (rf, wb) =>
      rf.wen := wb.valid && wb.bits.uop.ctrl.rfWen
      rf.addr := wb.bits.uop.pdest
      rf.data := wb.bits.data
  }
}
