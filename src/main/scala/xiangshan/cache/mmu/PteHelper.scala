/***************************************************************************************
* Copyright (c) 2021-2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.cache.mmu

import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._

class PteHelper() extends ExtModule {
  val clock = IO(Input(Clock()))
  val enable = IO(Input(Bool()))
  val satp = IO(Input(UInt(64.W)))
  val vsatp = IO(Input(UInt(64.W)))
  val hgatp = IO(Input(UInt(64.W)))
  val mPBMTE = IO(Input(Bool()))
  val hPBMTE = IO(Input(Bool()))
  val vpn = IO(Input(UInt(64.W)))
  val s2xlate = IO(Input(UInt(8.W)))
  val pte = IO(Output(UInt(64.W)))
  val level = IO(Output(UInt(8.W)))
  val pfType = IO(Output(UInt(8.W)))
  val s1_pte = IO(Output(UInt(64.W)))
  val s2_pte = IO(Output(UInt(64.W)))
  val s1_level = IO(Output(UInt(8.W)))

  def connectCsr(csr: TlbCsrBundle): Unit = {
    satp := Cat(csr.satp.mode, csr.satp.asid, csr.satp.ppn)
    vsatp := Cat(csr.vsatp.mode, csr.vsatp.asid, csr.vsatp.ppn)
    hgatp := Cat(csr.hgatp.mode, csr.hgatp.vmid, csr.hgatp.ppn)
    mPBMTE := csr.mPBMTE
    hPBMTE := csr.hPBMTE
  }

  def connectReq(reqVpn: UInt, reqS2xlate: UInt, reqEnable: Bool): Unit = {
    enable := reqEnable
    vpn := reqVpn
    s2xlate := reqS2xlate
  }
}

trait HasPteHelperResultConst extends HasTlbConst {
  def genFullPPN(ptePPN: UInt, level: UInt, vpn: UInt, napot: Bool): UInt = {
    val napotPPN = Cat(ptePPN(ptePPNLen - 1, pteNapotBits), vpn(pteNapotBits - 1, 0))
    MuxLookup(level, 0.U(ptePPNLen.W))(Seq(
      3.U -> Cat(ptePPN(ptePPNLen - 1, vpnnLen * 3), vpn(vpnnLen * 3 - 1, 0)),
      2.U -> Cat(ptePPN(ptePPNLen - 1, vpnnLen * 2), vpn(vpnnLen * 2 - 1, 0)),
      1.U -> Cat(ptePPN(ptePPNLen - 1, vpnnLen), vpn(vpnnLen - 1, 0)),
      0.U -> Mux(napot, napotPPN, ptePPN)
    ))
  }
}

class PteHelperStageResult(implicit p: Parameters) extends TlbBundle with HasPteHelperResultConst {
  val pte = new PteBundle
  val level = UInt(8.W)

  def fullPPN(vpn: UInt): UInt = {
    genFullPPN(pte.getPPN(), level, vpn, pte.isNapot(level))
  }
}

class PteHelperResult(implicit p: Parameters) extends TlbBundle with HasPteHelperResultConst {
  val pte = new PteBundle
  val level = UInt(8.W)
  val pfType = UInt(8.W)
  val s1 = new PteHelperStageResult
  val s2 = new PteHelperStageResult

  def hasPf: Bool = pfType === 1.U

  def hasGpf: Bool = pfType === 2.U

  def hasNoPf: Bool = pfType === 0.U

  def finalPPN(vpn: UInt, s2xlate: UInt): UInt = {
    val singleStagePPN = genFullPPN(pte.getPPN(), level, vpn, pte.isNapot(level))
    val gpaPPN = s1.fullPPN(vpn)
    val hpaPPN = s2.fullPPN(gpaPPN)
    Mux(s2xlate === allStage, hpaPPN, singleStagePPN)
  }

  def s1TranslatedPPN(vpn: UInt): UInt = {
    s1.fullPPN(vpn)
  }

  def s1RespPte(s2xlate: UInt): PteBundle = {
    Mux(s2xlate === allStage, s1.pte, pte)
  }

  def s2RespPte(s2xlate: UInt): PteBundle = {
    Mux(s2xlate === allStage, s2.pte, pte)
  }

  def fillS1Resp(resp: PtwSectorResp, vpn: UInt, s2xlate: UInt, asid: UInt, vsasid: UInt, vmid: UInt): Unit = {
    val isAllStage = s2xlate === allStage
    val respPte = s1RespPte(s2xlate)
    val respLevel = Mux(isAllStage, s1.level, level)
    val respFullPPN = respPte.getPPN()
    val respNapot = respPte.n.asBool && respPte.ppn(3, 0) === 8.U && respLevel === 0.U

    resp.entry.tag := vpn(vpn.getWidth - 1, sectortlbwidth)
    resp.entry.pbmt := respPte.pbmt
    resp.entry.ppn := respFullPPN(ptePPNLen - 1, sectortlbwidth)
    resp.entry.perm.map(_ := respPte.getPerm())
    resp.entry.level.map(_ := respLevel)
    resp.entry.v := Mux(isAllStage, s1.pte.perm.v, hasNoPf)
    resp.entry.prefetch := false.B
    resp.entry.asid := Mux(s2xlate =/= noS2xlate, vsasid, asid)
    resp.entry.vmid.map(_ := vmid)
    resp.entry.n.map(_ := Mux(respNapot, 1.U, 0.U))
    resp.pf := hasPf
    resp.af := false.B
    resp.addr_low := vpn(sectortlbwidth - 1, 0)
    for (j <- 0 until tlbcontiguous) {
      resp.ppn_low(j) := respFullPPN(sectortlbwidth - 1, 0)
      resp.valididx(j) := (j.U === vpn(sectortlbwidth - 1, 0)) || (respLevel =/= 0.U)
      resp.pteidx(j) := j.U === vpn(sectortlbwidth - 1, 0)
    }
  }

  def fillS2Resp(resp: HptwResp, vpn: UInt, s2xlate: UInt, vmid: UInt): Unit = {
    val isAllStage = s2xlate === allStage
    val respPte = s2RespPte(s2xlate)
    val respLevel = Mux(isAllStage, s2.level, level)
    val s2Tag = Mux(isAllStage, s1TranslatedPPN(vpn)(gvpnLen - 1, 0), vpn(gvpnLen - 1, 0))
    val respNapot = respPte.n.asBool && respPte.ppn(3, 0) === 8.U && respLevel === 0.U
    val hasS2Resp = (s2xlate === onlyStage2) || (isAllStage && respPte.asUInt =/= 0.U)

    resp.entry.tag := s2Tag
    resp.entry.pbmt := respPte.pbmt
    resp.entry.ppn := respPte.ppn
    resp.entry.perm.map(_ := respPte.getPerm())
    resp.entry.level.map(_ := respLevel)
    resp.entry.v := hasS2Resp && !hasGpf
    resp.entry.asid := DontCare
    resp.entry.vmid.map(_ := vmid)
    resp.entry.n.map(_ := Mux(respNapot, 1.U, 0.U))
    resp.entry.prefetch := false.B
    resp.gpf := hasGpf
    resp.gaf := false.B
  }
}

object PteHelperResult {
  def fromHelper(helper: PteHelper, enable: Bool)(implicit p: Parameters): PteHelperResult = {
    val result = Wire(new PteHelperResult)
    result.pte := RegEnable(helper.pte, enable).asTypeOf(new PteBundle)
    result.level := RegEnable(helper.level, enable)
    result.pfType := RegEnable(helper.pfType, enable)
    result.s1.pte := RegEnable(helper.s1_pte, enable).asTypeOf(new PteBundle)
    result.s1.level := RegEnable(helper.s1_level, enable)
    result.s2.pte := RegEnable(helper.s2_pte, enable).asTypeOf(new PteBundle)
    result.s2.level := result.level
    result
  }
}
