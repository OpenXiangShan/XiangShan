/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.{DynInst, MemExuInput, MemExuOutput}
import xiangshan.backend.fu._
import xiangshan.backend.fu.FuType._
import xiangshan.backend.fu.util.{HasCSRConst, SdtrigExt}
import xiangshan.mem._
import xiangshan.mem.mdp._
import xiangshan.mem.Bundles._
import xiangshan.cache._
import xiangshan.cache.mmu._

class MemExceptionGen(numSources: Int)(implicit p: Parameters) extends XSModule
  with HasTlbConst
  with HasCSRConst {
  val io = IO(new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val tlbCsr = Input(new TlbCsrBundle)
    val in = Vec(numSources, Flipped(ValidIO(new ExceptionAddrIO)))
    val out = ValidIO(new ExceptionAddrIO)
  })

  val highestException = PriorityMux(io.in.map(in => in.valid -> in))
  val exceptionVaddr = highestException.bits.vaddr
  val exceptionVaNeedExt = highestException.bits.vaNeedExt
  val exceptionIsHyper = highestException.bits.isHyper

  def GenExceptionVa(mode: UInt, isVirt: Bool, vaNeedExt: Bool,
      satp: TlbSatpBundle, vsatp: TlbSatpBundle, hgatp: TlbHgatpBundle, vaddr: UInt) = {
    require(VAddrBits >= 50)

    val Sv39 = satp.mode === 8.U
    val Sv48 = satp.mode === 9.U
    val Sv39x4 = vsatp.mode === 8.U || hgatp.mode === 8.U
    val Sv48x4 = vsatp.mode === 9.U || hgatp.mode === 9.U
    val vmEnable = !isVirt && (Sv39 || Sv48) && (mode < ModeM)
    val s2xlateEnable = isVirt && (Sv39x4 || Sv48x4) && (mode < ModeM)

    val s2xlate = MuxCase(noS2xlate, Seq(
      !isVirt                                    -> noS2xlate,
      (vsatp.mode =/= 0.U && hgatp.mode =/= 0.U) -> allStage,
      (vsatp.mode === 0.U)                       -> onlyStage2,
      (hgatp.mode === 0.U)                       -> onlyStage1
    ))
    val onlyS2 = s2xlate === onlyStage2

    val bareAddr   = ZeroExt(vaddr(PAddrBits - 1, 0), XLEN)
    val sv39Addr   = SignExt(vaddr.take(39), XLEN)
    val sv39x4Addr = ZeroExt(vaddr.take(39 + 2), XLEN)
    val sv48Addr   = SignExt(vaddr.take(48), XLEN)
    val sv48x4Addr = ZeroExt(vaddr.take(48 + 2), XLEN)

    val exceptionVa = Wire(UInt(XLEN.W))
    when (vaNeedExt) {
      exceptionVa := Mux1H(Seq(
        (!(vmEnable || s2xlateEnable)) -> bareAddr,
        (!onlyS2 && (Sv39 || Sv39x4))  -> sv39Addr,
        (!onlyS2 && (Sv48 || Sv48x4))  -> sv48Addr,
        ( onlyS2 && (Sv39 || Sv39x4))  -> sv39x4Addr,
        ( onlyS2 && (Sv48 || Sv48x4))  -> sv48x4Addr,
      ))
    } .otherwise {
      exceptionVa := vaddr
    }
    exceptionVa
  }


  //
  io.out.valid := RegNext(highestException.valid)
  io.out.bits := DontCare
  io.out.bits.vaddr := RegNext(
    GenExceptionVa(io.tlbCsr.priv.dmode, io.tlbCsr.priv.virt || exceptionIsHyper, exceptionVaNeedExt,
    io.tlbCsr.satp, io.tlbCsr.vsatp, io.tlbCsr.hgatp, exceptionVaddr)
  )
  io.out.bits.vstart := RegNext(highestException.bits.vstart)
  io.out.bits.vl     := RegNext(highestException.bits.vl)
  io.out.bits.gpaddr := RegNext(highestException.bits.gpaddr)
  io.out.bits.isForVSnonLeafPTE := RegNext(highestException.bits.gpaddr)
}
