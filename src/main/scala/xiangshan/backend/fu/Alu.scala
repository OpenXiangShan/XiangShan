package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import utils.{LookupTree, LookupTreeDefault, ParallelMux, SignExt, XSDebug, ZeroExt}
import xiangshan._
import xiangshan.backend.ALUOpType

class Alu extends FunctionUnit with HasRedirectOut {

  val (src1, src2, func, pc, uop) = (
    io.in.bits.src(0),
    io.in.bits.src(1),
    io.in.bits.uop.ctrl.fuOpType,
    SignExt(io.in.bits.uop.cf.pc, AddrBits),
    io.in.bits.uop
  )

  val offset = src2

  val valid = io.in.valid

  val isAdderSub = (func =/= ALUOpType.add) && (func =/= ALUOpType.addw)
  val addRes = src1 +& src2
  val subRes = (src1 +& (~src2).asUInt()) + 1.U
  val xorRes = src1 ^ src2
  val sltu = !subRes(XLEN)
  val slt = xorRes(XLEN-1) ^ sltu

  val shsrc1 = LookupTreeDefault(func, src1, List(
    ALUOpType.srlw -> ZeroExt(src1(31,0), 64),
    ALUOpType.sraw -> SignExt(src1(31,0), 64)
  ))
  val shamt = Mux(ALUOpType.isWordOp(func), src2(4, 0), src2(5, 0))

  val miscRes = ParallelMux(List(
    ALUOpType.sll  -> (shsrc1 << shamt)(XLEN-1, 0),
    ALUOpType.slt  -> ZeroExt(slt, XLEN),
    ALUOpType.sltu -> ZeroExt(sltu, XLEN),
    ALUOpType.xor  -> xorRes,
    ALUOpType.srl  -> (shsrc1 >> shamt),
    ALUOpType.or   -> (src1 | src2),
    ALUOpType.and  -> (src1 & src2),
    ALUOpType.sra  -> (shsrc1.asSInt >> shamt).asUInt
  ).map(x => (x._1 === func(3, 0), x._2)))

  val res = Mux(ALUOpType.isAddSub(func),
    Mux(isAdderSub, subRes, addRes),
    miscRes
  )

  val aluRes = Mux(ALUOpType.isWordOp(func), SignExt(res(31,0), 64), res)

  val branchOpTable = List(
    ALUOpType.getBranchType(ALUOpType.beq)  -> !xorRes.orR,
    ALUOpType.getBranchType(ALUOpType.blt)  -> slt,
    ALUOpType.getBranchType(ALUOpType.bltu) -> sltu
  )

  val isBranch = ALUOpType.isBranch(func)
  val isRVC = uop.cf.pd.isRVC
  val taken = isBranch && LookupTree(ALUOpType.getBranchType(func), branchOpTable) ^ ALUOpType.isBranchInvert(func)
  val target = (pc + offset)(VAddrBits-1,0)
  val snpc = Mux(isRVC, pc + 2.U, pc + 4.U)

  redirectOutValid := io.out.valid && isBranch
  // Only brTag, level, roqIdx are needed
  // other infos are stored in brq
  redirectOut := DontCare
  redirectOut.level := RedirectLevel.flushAfter
  redirectOut.roqIdx := uop.roqIdx
  redirectOut.ftqIdx := uop.cf.ftqPtr
  redirectOut.ftqOffset := uop.cf.ftqOffset
  redirectOut.cfiUpdate.isMisPred := !uop.cf.pred_taken && taken
  redirectOut.cfiUpdate.taken := taken
  redirectOut.cfiUpdate.predTaken := uop.cf.pred_taken

  io.in.ready := io.out.ready
  io.out.valid := valid
  io.out.bits.uop <> io.in.bits.uop
  io.out.bits.data := aluRes
}
