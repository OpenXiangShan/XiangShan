package top

import system._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chipsalliance.rocketchip.config
import chisel3.stage.ChiselGeneratorAnnotation
import device._
import freechips.rocketchip.amba.axi4.{AXI4Fragmenter, AXI4UserYanker}
import freechips.rocketchip.diplomacy.{AddressSet, BufferParams, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLCacheCork, TLFragmenter, TLFuzzer, TLToAXI4, TLXbar}
import xiangshan._
import utils._
import firrtl.stage.RunFirrtlTransformAnnotation
import xstransforms.ShowPrintTransform

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
  val isRVC = Output(Bool())
  val intrNO = Output(UInt(64.W))

  val priviledgeMode = Output(UInt(2.W))
  val mstatus = Output(UInt(64.W))
  val sstatus = Output(UInt(64.W))
  val mepc = Output(UInt(64.W))
  val sepc = Output(UInt(64.W))
  val mcause = Output(UInt(64.W))
  val scause = Output(UInt(64.W))

  val satp = Output(UInt(64.W))
  val mip = Output(UInt(64.W))
  val mie = Output(UInt(64.W))
  val mscratch = Output(UInt(64.W))
  val sscratch = Output(UInt(64.W))
  val mideleg = Output(UInt(64.W))
  val medeleg = Output(UInt(64.W))

  val lrscValid = Output(Bool())
  val lrscAddr = Output(UInt(64.W))
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


class XSSimTop()(implicit p: config.Parameters) extends LazyModule {

  val memAddressSet = AddressSet(0x0L, 0xffffffffffL)

  val soc = LazyModule(new XSSoc())
  val axiRam = LazyModule(new AXI4RAM(
    memAddressSet,
    memByte = 128 * 1024 * 1024,
    useBlackBox = true
  ))
  val axiMMIO = LazyModule(new SimMMIO())

  axiRam.node :=
    AXI4UserYanker() :=
    TLToAXI4() :=
    TLBuffer(BufferParams.default) :=
    DebugIdentityNode() :=
    soc.mem

  axiMMIO.axiBus :=
    AXI4UserYanker() :=
    TLToAXI4() :=
    soc.extDev

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val difftest = new DiffTestIO
      val logCtrl = new LogCtrlIO
      val trap = new TrapIO
      val uart = new UARTIO
    })

    io.uart <> axiMMIO.module.io.uart
    soc.module.io.meip := false.B

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
    BoringUtils.addSink(difftest.satp, "difftestSatp")
    BoringUtils.addSink(difftest.mip, "difftestMip")
    BoringUtils.addSink(difftest.mie, "difftestMie")
    BoringUtils.addSink(difftest.mscratch, "difftestMscratch")
    BoringUtils.addSink(difftest.sscratch, "difftestSscratch")
    BoringUtils.addSink(difftest.mideleg, "difftestMideleg")
    BoringUtils.addSink(difftest.medeleg, "difftestMedeleg")
    BoringUtils.addSink(difftest.lrscValid, "difftestLrscValid")
    BoringUtils.addSink(difftest.lrscAddr, "difftestLrscAddr")
    io.difftest := difftest

    val trap = WireInit(0.U.asTypeOf(new TrapIO))
    ExcitingUtils.addSink(trap.valid, "trapValid")
    ExcitingUtils.addSink(trap.code, "trapCode")
    ExcitingUtils.addSink(trap.pc, "trapPC")
    ExcitingUtils.addSink(trap.cycleCnt, "trapCycleCnt")
    ExcitingUtils.addSink(trap.instrCnt, "trapInstrCnt")
    io.trap := trap

    val timer = GTimer()
    val logEnable = (timer >= io.logCtrl.log_begin) && (timer < io.logCtrl.log_end)
    ExcitingUtils.addSource(logEnable, "DISPLAY_LOG_ENABLE")
    ExcitingUtils.addSource(timer, "logTimestamp")

    // Check and dispaly all source and sink connections
    ExcitingUtils.checkAndDisplay()
  }
}

object TestMain extends App {
  // set parameters
  Parameters.set(
    if(args.contains("--disable-log")) Parameters.simParameters // sim only, disable log
    else Parameters.debugParameters // open log
  )
  implicit val p = config.Parameters.empty
  // generate verilog
  XiangShanStage.execute(
    args.filterNot(_ == "--disable-log"),
    Seq(
      ChiselGeneratorAnnotation(() => LazyModule(new XSSimTop).module)
    )
  )
}
