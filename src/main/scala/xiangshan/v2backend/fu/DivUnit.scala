package xiangshan.v2backend.fu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.RegEnable
import utility.{SignExt, ZeroExt}
import xiangshan.backend.fu.{MulDivCtrl, SRT16DividerDataModule}
import xiangshan.v2backend.FuConfig
import xiangshan.DIVOpType

class DivUnit(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {

  val xlen = cfg.dataBits

  val func = io.in.bits.fuOpType
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

  val robIdxReg = RegEnable(io.in.bits.robIdx, io.in.fire)
  val ctrlReg = RegEnable(ctrl, io.in.fire)

  val divDataModule = Module(new SRT16DividerDataModule(cfg.dataBits))

  val kill_w = io.in.bits.robIdx.needFlush(io.flush)
  val kill_r = !divDataModule.io.in_ready && robIdxReg.needFlush(io.flush)

  divDataModule.io.valid := io.in.valid
  divDataModule.io.src(0) := divInputCvtFunc(io.in.bits.src(0))
  divDataModule.io.src(1) := divInputCvtFunc(io.in.bits.src(1))
  divDataModule.io.sign := ctrl.sign // Todo: check it if assign with ctrlReg
  divDataModule.io.kill_w := kill_w
  divDataModule.io.kill_r := kill_r
  divDataModule.io.isHi := ctrlReg.isHi
  divDataModule.io.isW := ctrlReg.isW
  divDataModule.io.out_ready := io.out.ready

  io.in.ready := divDataModule.io.in_ready
  io.out.valid := divDataModule.io.out_valid
  io.out.bits.data := divDataModule.io.out_data
  connectCtrlSingal
}
