package top

import system._
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config
import chisel3.stage.ChiselGeneratorAnnotation
import device._
import freechips.rocketchip.amba.axi4.{AXI4UserYanker, AXI4Xbar, AXI4IdentityNode}
import freechips.rocketchip.diplomacy.{AddressSet, BufferParams, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLToAXI4}
import xiangshan._
import utils._
import ExcitingUtils.Debug

class DiffTestIO extends XSBundle {
  val r = Output(Vec(64, UInt(XLEN.W)))
  val commit = Output(UInt(32.W))
  val thisPC = Output(UInt(XLEN.W))
  val thisINST = Output(UInt(32.W))
  val skip = Output(UInt(32.W))
  val wen = Output(UInt(32.W))
  val wdata = Output(Vec(CommitWidth, UInt(XLEN.W))) // set difftest width to 6
  val wdst = Output(Vec(CommitWidth, UInt(32.W))) // set difftest width to 6
  val wpc = Output(Vec(CommitWidth, UInt(XLEN.W))) // set difftest width to 6
  val isRVC = Output(UInt(32.W))
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

  val scFailed = Output(Bool())
}

class LogCtrlIO extends Bundle {
  val log_begin, log_end = Input(UInt(64.W))
  val log_level = Input(UInt(64.W)) // a cpp uint
}

class TrapIO extends XSBundle {
  val valid = Output(Bool())
  val code = Output(UInt(3.W))
  val pc = Output(UInt(VAddrBits.W))
  val cycleCnt = Output(UInt(XLEN.W))
  val instrCnt = Output(UInt(XLEN.W))
}

class XSSimSoC(axiSim: Boolean)(implicit p: config.Parameters) extends LazyModule with HasXSParameter {
  // address space[0G - 1024G)
  val fullRange = AddressSet(0x0L, 0xffffffffffL)
  // MMIO address space[0G - 2G)
  val mmioRange = AddressSet(base = 0x0000000000L, mask = 0x007fffffffL)
  // DRAM address range[2G - 1024G)
  val dramRange = fullRange.subtract(mmioRange)

  val soc = LazyModule(new XSSoc())

  // 4x1 crossbar
  val xbar = AXI4Xbar()
  soc.mem.map{mem => xbar := mem}

  // AXIRam
  // -----------------------------------
  val axiMem = {
    if (axiSim)
      AXI4IdentityNode()
    else
      LazyModule(new AXI4RAM(
        dramRange,
        memByte = 128 * 1024 * 1024,
        useBlackBox = true,
        beatBytes = L3BusWidth / 8
      )).node
  }
  axiMem := xbar

  // AXI DMA
  // -----------------------------------
  val burst = LazyModule(new AXI4BurstMaster(
    startAddr = 0x80000000L,
    nOp = 0,
    beatBytes = L3BusWidth / 8))
  soc.dma := burst.node

  // AXI MMIO
  // -----------------------------------
  val axiMMIO = LazyModule(new SimMMIO())
  axiMMIO.axiBus := soc.extDev

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val difftest = new DiffTestIO
      val logCtrl = new LogCtrlIO
      val trap = new TrapIO
      val uart = new UARTIO
    })

    dontTouch(io.difftest)
    dontTouch(io.logCtrl)
    dontTouch(io.trap)
    dontTouch(io.uart)

    io.uart <> axiMMIO.module.io.uart
    soc.module.io.meip := false.B

    val difftest = WireInit(0.U.asTypeOf(new DiffTestIO))
    if (!env.FPGAPlatform) {
      ExcitingUtils.addSink(difftest.commit, "difftestCommit", Debug)
      ExcitingUtils.addSink(difftest.thisPC, "difftestThisPC", Debug)
      ExcitingUtils.addSink(difftest.thisINST, "difftestThisINST", Debug)
      ExcitingUtils.addSink(difftest.skip, "difftestSkip", Debug)
      ExcitingUtils.addSink(difftest.isRVC, "difftestIsRVC", Debug)
      ExcitingUtils.addSink(difftest.wen, "difftestWen", Debug)
      ExcitingUtils.addSink(difftest.wdata, "difftestWdata", Debug)
      ExcitingUtils.addSink(difftest.wdst, "difftestWdst", Debug)
      ExcitingUtils.addSink(difftest.wpc, "difftestWpc", Debug)
      ExcitingUtils.addSink(difftest.intrNO, "difftestIntrNO", Debug)
      ExcitingUtils.addSink(difftest.cause, "difftestCause", Debug)
      ExcitingUtils.addSink(difftest.r, "difftestRegs", Debug)
      ExcitingUtils.addSink(difftest.priviledgeMode, "difftestMode", Debug)
      ExcitingUtils.addSink(difftest.mstatus, "difftestMstatus", Debug)
      ExcitingUtils.addSink(difftest.sstatus, "difftestSstatus", Debug)
      ExcitingUtils.addSink(difftest.mepc, "difftestMepc", Debug)
      ExcitingUtils.addSink(difftest.sepc, "difftestSepc", Debug)
      ExcitingUtils.addSink(difftest.mtval, "difftestMtval", Debug)
      ExcitingUtils.addSink(difftest.stval, "difftestStval", Debug)
      ExcitingUtils.addSink(difftest.mtvec, "difftestMtvec", Debug)
      ExcitingUtils.addSink(difftest.stvec, "difftestStvec", Debug)
      ExcitingUtils.addSink(difftest.mcause, "difftestMcause", Debug)
      ExcitingUtils.addSink(difftest.scause, "difftestScause", Debug)
      ExcitingUtils.addSink(difftest.satp, "difftestSatp", Debug)
      ExcitingUtils.addSink(difftest.mip, "difftestMip", Debug)
      ExcitingUtils.addSink(difftest.mie, "difftestMie", Debug)
      ExcitingUtils.addSink(difftest.mscratch, "difftestMscratch", Debug)
      ExcitingUtils.addSink(difftest.sscratch, "difftestSscratch", Debug)
      ExcitingUtils.addSink(difftest.mideleg, "difftestMideleg", Debug)
      ExcitingUtils.addSink(difftest.medeleg, "difftestMedeleg", Debug)
      ExcitingUtils.addSink(difftest.scFailed, "difftestScFailed", Debug)
    }

    // BoringUtils.addSink(difftest.lrscAddr, "difftestLrscAddr")
    io.difftest := difftest

    val trap = WireInit(0.U.asTypeOf(new TrapIO))
    if (!env.FPGAPlatform) {
      ExcitingUtils.addSink(trap.valid, "trapValid")
      ExcitingUtils.addSink(trap.code, "trapCode")
      ExcitingUtils.addSink(trap.pc, "trapPC")
      ExcitingUtils.addSink(trap.cycleCnt, "trapCycleCnt")
      ExcitingUtils.addSink(trap.instrCnt, "trapInstrCnt")
    }

    io.trap := trap

    if (env.EnableDebug) {
      val timer = GTimer()
      val logEnable = (timer >= io.logCtrl.log_begin) && (timer < io.logCtrl.log_end)
      ExcitingUtils.addSource(logEnable, "DISPLAY_LOG_ENABLE")
      ExcitingUtils.addSource(timer, "logTimestamp")
    }

    // Check and dispaly all source and sink connections
    ExcitingUtils.fixConnections()
    ExcitingUtils.checkAndDisplay()
  }
}

class XSSimTop(axiSim: Boolean)(implicit p: config.Parameters) extends LazyModule with HasXSParameter {
  println(axiSim)
  val dut = LazyModule(new XSSimSoC(axiSim))
  val axiSimRam = {
    if (axiSim) LazyModule(new AXI4RAM(
      dut.dramRange,
      memByte = 128 * 1024 * 1024,
      useBlackBox = true,
      beatBytes = L3BusWidth / 8
    ))
    else null
  }
  if (axiSim) {
    axiSimRam.node := dut.axiMem
  }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val difftest = new DiffTestIO
      val logCtrl = new LogCtrlIO
      val trap = new TrapIO
      val uart = new UARTIO
      val memAXI = if (axiSim) chiselTypeOf(axiSimRam.module.io) else Input(Bool())
    })

    io.difftest <> dut.module.io.difftest
    io.logCtrl <> dut.module.io.logCtrl
    io.trap <> dut.module.io.trap
    io.uart <> dut.module.io.uart
    if (axiSim) {
      io.memAXI <> axiSimRam.module.io
    }
    else {
      io.memAXI <> DontCare
    }
  }
}

object TestMain extends App {
  val axiSim = args.contains("--with-dramsim3")

  // set soc parameters
  val socArgs = args.filterNot(_ == "--with-dramsim3")
  Parameters.set(
    if(socArgs.contains("--fpga-platform")) {
      if (socArgs.contains("--dual-core")) Parameters.dualCoreParameters
      else Parameters()
    }
    else if(socArgs.contains("--disable-log")) Parameters.simParameters // sim only, disable log
    else Parameters.debugParameters // open log
  )

  val otherArgs = socArgs.filterNot(_ == "--disable-log").filterNot(_ == "--fpga-platform").filterNot(_ == "--dual-core")
  implicit val p = config.Parameters.empty
  // generate verilog
  XiangShanStage.execute(
    otherArgs,
    Seq(
      ChiselGeneratorAnnotation(() => LazyModule(new XSSimTop(axiSim)).module)
    )
  )
}
