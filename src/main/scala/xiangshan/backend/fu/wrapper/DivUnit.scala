package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util.RegEnable
import utility.{SignExt, ZeroExt}
import xiangshan.DIVOpType
import xiangshan.backend.fu.{FuncUnit, MulDivCtrl, SRT16DividerDataModule}
import xiangshan.backend.fu.FuConfig

class DivUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {

  val xlen = cfg.dataBits

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

  val divDataModule = Module(new SRT16DividerDataModule(cfg.dataBits))

  val kill_w = io.in.bits.ctrl.robIdx.needFlush(io.flush)
  val kill_r = !divDataModule.io.in_ready && robIdxReg.needFlush(io.flush)

  divDataModule.io.valid := io.in.valid
  divDataModule.io.src(0) := divInputCvtFunc(io.in.bits.data.src(0))
  divDataModule.io.src(1) := divInputCvtFunc(io.in.bits.data.src(1))
  divDataModule.io.sign := ctrl.sign
  divDataModule.io.kill_w := kill_w
  divDataModule.io.kill_r := kill_r
  divDataModule.io.isHi := ctrlReg.isHi
  divDataModule.io.isW := ctrlReg.isW
  divDataModule.io.out_ready := io.out.ready

  val validNext = divDataModule.io.out_validNext // if high, io.valid will assert next cycle

  io.in.ready := divDataModule.io.in_ready
  io.out.valid := divDataModule.io.out_valid
  io.out.bits.res.data := divDataModule.io.out_data
  connectNonPipedCtrlSingal
}
