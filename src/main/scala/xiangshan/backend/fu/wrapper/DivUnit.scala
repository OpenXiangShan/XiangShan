package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util.RegEnable
import utility.{SignExt, ZeroExt, StartStopCounter, XSPerfHistogram}
import xiangshan.DIVOpType
import xiangshan.backend.fu.{FuncUnit, MulDivCtrl, SRT16DividerDataModule}
import xiangshan.backend.fu.FuConfig
import xiangshan.frontend.tracertl.{TraceDummyIntDivider, TraceRTLChoose}

class DivUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {

  val xlen = cfg.destDataBits

  val func = io.in.bits.ctrl.fuOpType
  val ctrl = Wire(new MulDivCtrl)
  ctrl.isW := DIVOpType.isW(func)
  ctrl.isHi := DIVOpType.isH(func)
  ctrl.sign := DIVOpType.isSign(func)

  val divInputCvtFunc: UInt => UInt = (x: UInt) => Mux(
    ctrl.isW,
    Mux(ctrl.sign,
      SignExt(x(31, 0), xlen),
      ZeroExt(x(31, 0), xlen)
    ),
    x
  )

  val robIdxReg = RegEnable(io.in.bits.ctrl.robIdx, io.in.fire)
  val ctrlReg = RegEnable(ctrl, io.in.fire)

  val divDataModule = Module(new SRT16DividerDataModule(cfg.destDataBits))
  val dummyDivDataMod = Module(new TraceDummyIntDivider)

  val kill_w = io.in.bits.ctrl.robIdx.needFlush(io.flush)
  val kill_r = WireInit(!divDataModule.io.in_ready && robIdxReg.needFlush(io.flush))

  val debug_unity_kill = Mux(io.in.ready, kill_w, kill_r)
  if (env.TraceRTLMode && trtl.TraceDummyFixCycleIntDiv) {
    divDataModule.io <> DontCare
    kill_r := !dummyDivDataMod.io.start_ready_o && robIdxReg.needFlush(io.flush)

    dummyDivDataMod.io.start_valid_i := io.in.valid
    dummyDivDataMod.io.flush_i := debug_unity_kill
    dummyDivDataMod.io.format_i := Mux(ctrlReg.isW, 2.U, 3.U)
    dummyDivDataMod.io.is_sqrt_i := false.B
    dummyDivDataMod.io.finish_ready_i := io.out.ready
  } else {
    dummyDivDataMod.io <> DontCare

    divDataModule.io.valid := io.in.valid
    divDataModule.io.src(0) := divInputCvtFunc(io.in.bits.data.src(0))
    divDataModule.io.src(1) := divInputCvtFunc(io.in.bits.data.src(1))
    divDataModule.io.sign := ctrl.sign
    divDataModule.io.kill_w := kill_w
    divDataModule.io.kill_r := kill_r
    divDataModule.io.isHi := ctrlReg.isHi
    divDataModule.io.isW := ctrlReg.isW
    divDataModule.io.out_ready := io.out.ready
  }


  // val validNext = divDataModule.io.out_validNext // if high, io.valid will assert next cycle

  if (env.TraceRTLMode && trtl.TraceDummyFixCycleIntDiv) {
    io.in.ready := dummyDivDataMod.io.start_ready_o
    io.out.valid := dummyDivDataMod.io.finish_valid_o
    io.out.bits.res.data := 0.U
  } else {
    io.in.ready := divDataModule.io.in_ready
    io.out.valid := divDataModule.io.out_valid
    io.out.bits.res.data := divDataModule.io.out_data
  }
  connectNonPipedCtrlSingal

  val exeCycleCounter = StartStopCounter(io.in.fire, io.out.valid, 1, debug_unity_kill)
  XSPerfHistogram("divCycle", exeCycleCounter, io.out.fire, 0, 24, 1)

  val exeWCycleCounter = StartStopCounter(io.in.fire && ctrl.isW, io.out.valid, 1, debug_unity_kill)
  XSPerfHistogram("divWCycle", exeWCycleCounter, io.out.fire, 0, 24, 1)
  val exeDCycleCounter = StartStopCounter(io.in.fire && !ctrl.isW, io.out.valid, 1, debug_unity_kill)
  XSPerfHistogram("divDCycle", exeDCycleCounter, io.out.fire, 0, 24, 1)
}
