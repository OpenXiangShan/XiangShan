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

class Jump extends FunctionUnit with HasRedirectOut {

  val (src1, jalr_target, pc, immMin, func, uop) = (
    io.in.bits.src(0),
    io.in.bits.src(1)(VAddrBits - 1, 0),
    SignExt(io.in.bits.uop.cf.pc, XLEN),
    io.in.bits.uop.ctrl.imm,
    io.in.bits.uop.ctrl.fuOpType,
    io.in.bits.uop
  )

  val isJalr = JumpOpType.jumpOpisJalr(func)
  val isAuipc = JumpOpType.jumpOpisAuipc(func)
  val offset = SignExt(ParallelMux(Seq(
    isJalr -> ImmUnion.I.toImm32(immMin),
    isAuipc -> ImmUnion.U.toImm32(immMin),
    !(isJalr || isAuipc) -> ImmUnion.J.toImm32(immMin)
  )), XLEN)

  val redirectHit = uop.roqIdx.needFlush(io.redirectIn, io.flushIn)
  val valid = io.in.valid

  val isRVC = uop.cf.pd.isRVC
  val snpc = Mux(isRVC, pc + 2.U, pc + 4.U)
  val target = src1 + offset // NOTE: src1 is (pc/rf(rs1)), src2 is (offset)

  redirectOutValid := valid && !isAuipc
  redirectOut := DontCare
  redirectOut.cfiUpdate.target := target
  redirectOut.level := RedirectLevel.flushAfter
  redirectOut.roqIdx := uop.roqIdx
  redirectOut.ftqIdx := uop.cf.ftqPtr
  redirectOut.ftqOffset := uop.cf.ftqOffset
  redirectOut.cfiUpdate.predTaken := true.B
  redirectOut.cfiUpdate.taken := true.B
  redirectOut.cfiUpdate.target := target
  redirectOut.cfiUpdate.isMisPred := target =/= jalr_target || !uop.cf.pred_taken


  // Output
  val res = Mux(JumpOpType.jumpOpisAuipc(func), target, snpc)

  io.in.ready := io.out.ready
  io.out.valid := valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := res

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
  XSDebug(io.in.valid, "src1:%x offset:%x func:%b type:JUMP pc:%x res:%x\n", src1, offset, func, pc, res)
}
