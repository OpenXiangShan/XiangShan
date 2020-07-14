package top

import system._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import bus.axi4._
import chisel3.stage.ChiselGeneratorAnnotation
import device.AXI4RAM
import xiangshan._
import utils._

class DiffTestIO extends XSBundle {
  val r = Output(Vec(64, UInt(XLEN.W)))
  val commit = Output(UInt(32.W))
  val thisPC = Output(UInt(VAddrBits.W))
  val thisINST = Output(UInt(32.W))
  val skip = Output(UInt(32.W))
  val wen = Output(UInt(32.W))
  val wdata = Output(Vec(CommitWidth, UInt(XLEN.W))) // set difftest width to 6
  val wdst = Output(Vec(CommitWidth, UInt(32.W))) // set difftest width to 6
  val wpc = Output(Vec(CommitWidth, UInt(VAddrBits.W))) // set difftest width to 6
  val isRVC = Output(Bool())
  val intrNO = Output(UInt(64.W))
  
  val priviledgeMode = Output(UInt(2.W))
  val mstatus = Output(UInt(64.W))
  val sstatus = Output(UInt(64.W))
  val mepc = Output(UInt(64.W))
  val sepc = Output(UInt(64.W))
  val mcause = Output(UInt(64.W))
  val scause = Output(UInt(64.W))
}

class LogCtrlIO extends Bundle {
  val log_begin, log_end = Input(UInt(64.W))
  val log_level = Input(UInt(64.W)) // a cpp uint
}

class XSSimTop extends Module {
  val io = IO(new Bundle{
    val difftest = new DiffTestIO
    val logCtrl = new LogCtrlIO
  })

  lazy val config = XSConfig(FPGAPlatform = false)
  val soc = Module(new XSSoc()(config))
  val mem = Module(new AXI4RAM(memByte = 128 * 1024 * 1024, useBlackBox = true))
  // Be careful with the commit checking of emu.
  // A large delay will make emu incorrectly report getting stuck.
  val memdelay = Module(new AXI4Delayer(0))
  val mmio = Module(new SimMMIO)

  soc.io.frontend := DontCare

  memdelay.io.in <> soc.io.mem
  mem.io.in <> memdelay.io.out

  mmio.io.rw <> soc.io.mmio

  // soc.io.meip := Counter(true.B, 9973)._2  // use prime here to not overlapped by mtip
  soc.io.meip := false.B  // use prime here to not overlapped by mtip

  val difftest = WireInit(0.U.asTypeOf(new DiffTestIO))
  BoringUtils.addSink(difftest.commit, "difftestCommit")
  BoringUtils.addSink(difftest.thisPC, "difftestThisPC")
  BoringUtils.addSink(difftest.thisINST, "difftestThisINST")
  BoringUtils.addSink(difftest.skip, "difftestSkip")
  BoringUtils.addSink(difftest.isRVC, "difftestIsRVC")
  BoringUtils.addSink(difftest.wen, "difftestWen")
  BoringUtils.addSink(difftest.wdata, "difftestWdata")
  BoringUtils.addSink(difftest.wdst, "difftestWdst")
  BoringUtils.addSink(difftest.wpc, "difftestWpc")
  BoringUtils.addSink(difftest.intrNO, "difftestIntrNO")
  BoringUtils.addSink(difftest.r, "difftestRegs")
  BoringUtils.addSink(difftest.priviledgeMode, "difftestMode")
  BoringUtils.addSink(difftest.mstatus, "difftestMstatus")
  BoringUtils.addSink(difftest.sstatus, "difftestSstatus") 
  BoringUtils.addSink(difftest.mepc, "difftestMepc")
  BoringUtils.addSink(difftest.sepc, "difftestSepc")
  BoringUtils.addSink(difftest.mcause, "difftestMcause")
  BoringUtils.addSink(difftest.scause, "difftestScause")
  io.difftest := difftest

  val logEnable = (GTimer() >= io.logCtrl.log_begin) && (GTimer() < io.logCtrl.log_end)
  BoringUtils.addSource(logEnable, "DISPLAY_LOG_ENABLE")
}

object TestMain extends App {
  if (args.contains("--disable-log"))
    XSLog.generateLog = false
  else
    XSLog.generateLog = true

  (new chisel3.stage.ChiselStage).execute(
    args.filterNot(_ == "--disable-log"),
    Seq(ChiselGeneratorAnnotation(() => new XSSimTop))
  )
}

object FirMain extends App{
  firrtl.stage.FirrtlMain.stage.execute(args, Seq(ChiselGeneratorAnnotation(() => new XSSimTop)))
}
