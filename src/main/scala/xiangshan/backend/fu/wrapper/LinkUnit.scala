package xiangshan.backend.fu.wrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.{SignExt, ZeroExt}
import xiangshan.{LinkOpType}
import xiangshan.backend.fu.{FuConfig, PipedFuncUnit}

class LinkUnit(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {

  // link:  next pc -> GPR[rd]
  // auipc: pc + imm -> GPR[rd]

  private val pc = io.instrAddrTransType.get.extend(io.in.bits.data.pc.get, cfg.destDataBits)
  private val imm = io.in.bits.data.imm
  private val func = io.in.bits.ctrl.fuOpType
  val nextPcOffset = io.in.bits.data.nextPcOffset.get

  val isAuipc: Bool = LinkOpType.linkUopisAuipc(func)
  val isLink: Bool = LinkOpType.linkUopisLink(func)

  io.in.ready := io.out.ready
  io.out.valid := io.in.valid
  io.out.bits.res.data := pc + Mux(isAuipc, SignExt(imm, XLEN), nextPcOffset)

  connect0LatencyCtrlSingal
}
