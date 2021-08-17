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

package xiangshan.cache.mmu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.roq.RoqPtr
import xiangshan.backend.fu.util.HasCSRConst

/** Bridge TLB is the bridge between L0 TLB(the old, normal TLB) and L2TLB
  * 1. store the entry from L2TLB, and seed it to L0TLB
  * 2. do not translate or do the pma and pmp work, just a small edition fa L2TLB
  */
class BridgeTLB(Width: Int)(implicit p: Parameters) extends TlbModule with HasCSRConst {
  val io = IO(new BridgeTLBIO(Width))

  val req = io.requestor.map(_.req)
  val resp = io.requestor.map(_.resp)
  val ptw = io.ptw

  val sfence = io.sfence
  // val csr    = io.csr
  // val satp   = csr.satp
  // val priv   = csr.priv
  // val mode   = priv.dmode
  // val vmEnable = satp.mode === 8.U // && (mode < ModeM) // FIXME: fix me when boot xv6/linux...
  // val vmEnable = if(EnbaleTlbDebug) (satp.mode === 8.U)
  //                else               (satp.mode === 8.U && (mode < ModeM))
  // TODO: the code above is same with TLB, may need a abstract TLB module class

  val entries = Reg(Vec(BTlbEntrySize, new PtwResp))
  val entries_v = RegInit(VecInit(Seq.fill(BTlbEntrySize)(false.B)))
  val replace = ReplacementPolicy.fromString(Some("random"), BTlbEntrySize)
  val refillIdx = replaceWrapper(entries_v, replace.way)

  // val WaitingSetSize = 4
  // val waiting_set = Reg(Vec(Width, Vec(WaitingSetSize, UInt(vpnLen.W))))
  // val waiting_set_v = RegInit(Vec(Width, Vec(WaitingSetSize, false.B)))

  for (i <- req.indices) {
    val vpn = req(i)(0).bits.vpn
    val hitVec = VecInit(entries.zip(entries_v).map{ case (e, v) =>
      e.entry.hit(vpn, allType = true) && v
    })

    hitVec.suggestName("hitVec")
    /* ============ next cycle =============== */
    val hitVecReg = RegNext(hitVec)
    val hitEntry = Mux1H(hitVecReg, entries)
    val hit = Cat(hitVecReg).orR
    hitEntry.suggestName("hitEntry")
    hit.suggestName("hit")

    resp(i).bits := hitEntry
    resp(i).valid := RegNext(req(i)(0).valid) && hit

    req(i)(0).ready := true.B // TODO: handle the ready
    io.ptw.req(i).valid := RegNext(req(i)(0).valid) && !hit
    io.ptw.req(i).bits.vpn := RegNext(vpn)

    XSPerfAccumulate("access" + Integer.toString(i, 10), req(i)(0).valid)
    XSPerfAccumulate("hit" + Integer.toString(i, 10), !(RegNext(req(i)(0).valid) && !hit))
  }

  when (io.ptw.resp.valid) {
    entries_v(refillIdx) := true.B
    entries(refillIdx) := io.ptw.resp.bits
  }
  io.ptw.resp.ready := true.B

  val sfence_hit = entries.map(_.entry.hit(sfence.bits.addr.asTypeOf(vaBundle).vpn))
  when (sfence.valid) {
    // entries_v := 0.U.asTypeOf(entries_v.cloneType)
    when (sfence.bits.rs1) {
      when (sfence.bits.rs2) {
        entries_v := 0.U.asTypeOf(entries_v.cloneType)
      }.otherwise {
        entries_v.zipWithIndex.map{a => a._1 := a._1 & entries(a._2).entry.perm.get.g}
      }
    }.otherwise {
      when (sfence.bits.rs2) {
        entries_v := (entries_v.zip(sfence_hit).map(a => a._1 & !a._2))
      }.otherwise {
        entries_v := (entries_v.zipWithIndex.map(a => a._1 & !(sfence_hit(a._2) && !entries(a._2).entry.perm.get.g)))
      }
    }
  }
}