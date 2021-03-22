package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend._
import xiangshan.backend.decode.ImmUnion
import xiangshan.backend.fu.FunctionUnit._
import xiangshan.backend.decode.isa._

trait HasRedirectOut { this: RawModule =>
  val redirectOutValid = IO(Output(Bool()))
  val redirectOut = IO(Output(new Redirect))
}

class JumpDataModule extends XSModule {
  val io = IO(new Bundle() {
    val src1 = Input(UInt(XLEN.W))
    val pc = Input(UInt(XLEN.W)) // sign-ext to XLEN
    val immMin = Input(UInt(ImmUnion.maxLen.W))
    val func = Input(FuOpType())
    val isRVC = Input(Bool())
    val result, target = Output(UInt(XLEN.W))
    val isAuipc = Output(Bool())
  })
  val (src1, pc, immMin, func, isRVC) = (io.src1, io.pc, io.immMin, io.func, io.isRVC)

  val isJalr = JumpOpType.jumpOpisJalr(func)
  val isAuipc = JumpOpType.jumpOpisAuipc(func)
  val offset = SignExt(ParallelMux(Seq(
    isJalr -> ImmUnion.I.toImm32(immMin),
    isAuipc -> ImmUnion.U.toImm32(immMin),
    !(isJalr || isAuipc) -> ImmUnion.J.toImm32(immMin)
  )), XLEN)

  val snpc = Mux(isRVC, pc + 2.U, pc + 4.U)
  val target = src1 + offset // NOTE: src1 is (pc/rf(rs1)), src2 is (offset)

  io.target := target
  io.result := Mux(JumpOpType.jumpOpisAuipc(func), target, snpc)
  io.isAuipc := isAuipc
}

class Jump extends FunctionUnit with HasRedirectOut {

  val (src1, jalr_target, pc, immMin, func, uop) = (
    io.in.bits.src(0),
    io.in.bits.src(1)(VAddrBits - 1, 0),
    SignExt(io.in.bits.uop.cf.pc, XLEN),
    io.in.bits.uop.ctrl.imm,
    io.in.bits.uop.ctrl.fuOpType,
    io.in.bits.uop
  )

  val redirectHit = uop.roqIdx.needFlush(io.redirectIn, io.flushIn)
  val valid = io.in.valid
  val isRVC = uop.cf.pd.isRVC

  val jumpDataModule = Module(new JumpDataModule)
  jumpDataModule.io.src1 := src1
  jumpDataModule.io.pc := pc
  jumpDataModule.io.immMin := immMin
  jumpDataModule.io.func := func
  jumpDataModule.io.isRVC := isRVC

  redirectOutValid := valid && !jumpDataModule.io.isAuipc
  redirectOut := DontCare
  redirectOut.level := RedirectLevel.flushAfter
  redirectOut.roqIdx := uop.roqIdx
  redirectOut.ftqIdx := uop.cf.ftqPtr
  redirectOut.ftqOffset := uop.cf.ftqOffset
  redirectOut.cfiUpdate.predTaken := true.B
  redirectOut.cfiUpdate.taken := true.B
  redirectOut.cfiUpdate.target := jumpDataModule.io.target
  redirectOut.cfiUpdate.isMisPred := jumpDataModule.io.target(VAddrBits - 1, 0) =/= jalr_target || !uop.cf.pred_taken

  io.in.ready := io.out.ready
  io.out.valid := valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := jumpDataModule.io.result

  // NOTE: the debug info is for one-cycle exec, if FMV needs multi-cycle, may needs change it
  XSDebug(io.in.valid, "In(%d %d) Out(%d %d) Redirect:(%d %d %d)\n",
    io.in.valid,
    io.in.ready,
    io.out.valid,
    io.out.ready,
    io.redirectIn.valid,
    io.redirectIn.bits.level,
    redirectHit
  )
}
