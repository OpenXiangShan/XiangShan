package top

import system._
import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config
import chisel3.stage.ChiselGeneratorAnnotation
import device._
import freechips.rocketchip.amba.axi4.{AXI4IdIndexer, AXI4IdentityNode, AXI4UserYanker, AXI4Xbar}
import freechips.rocketchip.diplomacy.{AddressSet, BufferParams, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.TLToAXI4
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

  val storeCommit = Output(UInt(2.W))
  val storeAddr   = Output(Vec(2, UInt(64.W)))
  val storeData   = Output(Vec(2, UInt(64.W)))
  val storeMask   = Output(Vec(2, UInt(8.W)))

  val sbufferResp = Output(Bool())
  val sbufferAddr = Output(UInt(64.W))
  val sbufferData = Output(Vec(64, UInt(8.W)))
  val sbufferMask = Output(UInt(64.W))

  val lpaddr = Output(Vec(CommitWidth, UInt(64.W)))
  val ltype = Output(Vec(CommitWidth, UInt(32.W)))
  val lfu = Output(Vec(CommitWidth, UInt(4.W)))

  val atomicResp = Output(Bool())
  val atomicAddr = Output(UInt(64.W))
  val atomicData = Output(UInt(64.W))
  val atomicMask = Output(UInt(8.W))
  val atomicFuop = Output(UInt(8.W))
  val atomicOut  = Output(UInt(64.W))

  val ptwResp = Output(Bool())
  val ptwAddr = Output(UInt(64.W))
  val ptwData = Output(Vec(4, UInt(64.W)))
}

class LogCtrlIO extends Bundle {
  val log_begin, log_end = Input(UInt(64.W))
  val log_level = Input(UInt(64.W)) // a cpp uint
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
        memByte = 64L * 1024 * 1024 * 1024,
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
  soc.dma := AXI4IdIndexer(16) := burst.node

  // AXI MMIO
  // -----------------------------------
  val axiMMIO = LazyModule(new SimMMIO())
  axiMMIO.axiBus := soc.extDev

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val difftest  = new DiffTestIO
      val difftest2 = new DiffTestIO
      val logCtrl = new LogCtrlIO
      val trap = new TrapIO
      val trap2 = new TrapIO
      val uart = new UARTIO
    })
    io.difftest2 <> DontCare

    dontTouch(io.difftest)
    dontTouch(io.logCtrl)
    dontTouch(io.trap)
    dontTouch(io.uart)

    io.uart <> axiMMIO.module.io.uart
    val NumCores = top.Parameters.get.socParameters.NumCores
    soc.module.io.extIntrs := 0.U

    val difftest = Seq(WireInit(0.U.asTypeOf(new DiffTestIO)), WireInit(0.U.asTypeOf(new DiffTestIO)))
    val trap = Seq(WireInit(0.U.asTypeOf(new TrapIO)), WireInit(0.U.asTypeOf(new TrapIO)))

    if (!env.FPGAPlatform) {
      for (i <- 0 until NumCores) {
        difftest(i).commit := soc.module.difftestIO(i).fromRoq.commit
        difftest(i).thisPC := soc.module.difftestIO(i).fromRoq.thisPC
        difftest(i).thisINST := soc.module.difftestIO(i).fromRoq.thisINST
        difftest(i).skip := soc.module.difftestIO(i).fromRoq.skip
        difftest(i).isRVC := soc.module.difftestIO(i).fromRoq.isRVC
        difftest(i).wen := soc.module.difftestIO(i).fromRoq.wen
        difftest(i).wdata := soc.module.difftestIO(i).fromRoq.wdata
        difftest(i).wdst := soc.module.difftestIO(i).fromRoq.wdst
        difftest(i).wpc := soc.module.difftestIO(i).fromRoq.wpc
        difftest(i).scFailed := soc.module.difftestIO(i).fromRoq.scFailed
        
        difftest(i).r := soc.module.difftestIO(i).fromXSCore.r

        difftest(i).intrNO := soc.module.difftestIO(i).fromCSR.intrNO
        difftest(i).cause := soc.module.difftestIO(i).fromCSR.cause
        difftest(i).priviledgeMode := soc.module.difftestIO(i).fromCSR.priviledgeMode
        difftest(i).mstatus := soc.module.difftestIO(i).fromCSR.mstatus
        difftest(i).sstatus := soc.module.difftestIO(i).fromCSR.sstatus
        difftest(i).mepc := soc.module.difftestIO(i).fromCSR.mepc
        difftest(i).sepc := soc.module.difftestIO(i).fromCSR.sepc
        difftest(i).mtval := soc.module.difftestIO(i).fromCSR.mtval
        difftest(i).stval := soc.module.difftestIO(i).fromCSR.stval
        difftest(i).mtvec := soc.module.difftestIO(i).fromCSR.mtvec
        difftest(i).stvec := soc.module.difftestIO(i).fromCSR.stvec
        difftest(i).mcause := soc.module.difftestIO(i).fromCSR.mcause
        difftest(i).scause := soc.module.difftestIO(i).fromCSR.scause
        difftest(i).satp := soc.module.difftestIO(i).fromCSR.satp
        difftest(i).mip := soc.module.difftestIO(i).fromCSR.mip
        difftest(i).mie := soc.module.difftestIO(i).fromCSR.mie
        difftest(i).mscratch := soc.module.difftestIO(i).fromCSR.mscratch
        difftest(i).sscratch := soc.module.difftestIO(i).fromCSR.sscratch
        difftest(i).mideleg := soc.module.difftestIO(i).fromCSR.mideleg
        difftest(i).medeleg := soc.module.difftestIO(i).fromCSR.medeleg

        difftest(i).storeCommit := soc.module.difftestIO(i).fromSQ.storeCommit
        difftest(i).storeAddr := soc.module.difftestIO(i).fromSQ.storeAddr
        difftest(i).storeData := soc.module.difftestIO(i).fromSQ.storeData
        difftest(i).storeMask := soc.module.difftestIO(i).fromSQ.storeMask

        difftest(i).sbufferResp := soc.module.difftestIO(i).fromSbuffer.sbufferResp
        difftest(i).sbufferAddr := soc.module.difftestIO(i).fromSbuffer.sbufferAddr
        difftest(i).sbufferData := soc.module.difftestIO(i).fromSbuffer.sbufferData
        difftest(i).sbufferMask := soc.module.difftestIO(i).fromSbuffer.sbufferMask

        difftest(i).lpaddr := soc.module.difftestIO(i).fromRoq.lpaddr
        difftest(i).ltype := soc.module.difftestIO(i).fromRoq.ltype
        difftest(i).lfu := soc.module.difftestIO(i).fromRoq.lfu

        difftest(i).atomicResp := soc.module.difftestIO(i).fromAtomic.atomicResp
        difftest(i).atomicAddr := soc.module.difftestIO(i).fromAtomic.atomicAddr
        difftest(i).atomicData := soc.module.difftestIO(i).fromAtomic.atomicData
        difftest(i).atomicMask := soc.module.difftestIO(i).fromAtomic.atomicMask
        difftest(i).atomicFuop := soc.module.difftestIO(i).fromAtomic.atomicFuop
        difftest(i).atomicOut  := soc.module.difftestIO(i).fromAtomic.atomicOut

        difftest(i).ptwResp := soc.module.difftestIO(i).fromPtw.ptwResp
        difftest(i).ptwAddr := soc.module.difftestIO(i).fromPtw.ptwAddr
        difftest(i).ptwData := soc.module.difftestIO(i).fromPtw.ptwData
      
        trap(i) <> soc.module.trapIO(i)
      }      
    }

    io.difftest := difftest(0)
    io.trap := trap(0)

    if (!env.FPGAPlatform && env.DualCore) {
      io.difftest2 := difftest(1)
      io.trap2 := trap(1)
    }

    if (env.EnableDebug || env.EnablePerfDebug) {
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
  println(s"axiSim:${axiSim}")
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
      val difftest  = new DiffTestIO
      val difftest2 = new DiffTestIO
      val logCtrl = new LogCtrlIO
      val trap = new TrapIO
      val trap2 = new TrapIO
      val uart = new UARTIO
      val memAXI = if (axiSim) chiselTypeOf(axiSimRam.module.io) else Input(Bool())
    })
    io.difftest2 <> DontCare

    io.difftest  <> dut.module.io.difftest
    io.logCtrl <> dut.module.io.logCtrl
    io.trap <> dut.module.io.trap
    io.uart <> dut.module.io.uart
    if (!env.FPGAPlatform && env.DualCore) {
      io.difftest2 <> dut.module.io.difftest2
      io.trap2 <> dut.module.io.trap2
    }
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
    (socArgs.contains("--fpga-platform"), socArgs.contains("--dual-core"), socArgs.contains("--disable-log")) match {
      case (true,  false, _)     => Parameters()
      case (true,   true, _)     => Parameters.dualCoreParameters
      case (false,  true,  true) => Parameters.simDualCoreParameters
      case (false, false,  true) => Parameters.simParameters
      case (false,  true, false) => Parameters.debugDualCoreParameters
      case (false, false, false) => Parameters.debugParameters
    }
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
