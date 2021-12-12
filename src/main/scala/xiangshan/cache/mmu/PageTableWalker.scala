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
import chisel3.internal.naming.chiselName
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}

/* ptw finite state machine, the actual page table walker
 */
class PtwFsmIO()(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
  val req = Flipped(DecoupledIO(new Bundle {
    val req_info = new L2TlbInnerBundle()
    val l1Hit = Bool()
    val ppn = UInt(ppnLen.W)
  }))
  val resp = DecoupledIO(new Bundle {
    val source = UInt(bSourceWidth.W)
    val resp = new PtwResp
  })

  val mq = DecoupledIO(new L2TlbMQInBundle())

  val mem = new Bundle {
    val req = DecoupledIO(new L2TlbMemReqBundle())
    val resp = Flipped(ValidIO(UInt(XLEN.W)))
    val mask = Input(Bool())
  }
  val pmp = new Bundle {
    val req = ValidIO(new PMPReqBundle())
    val resp = Flipped(new PMPRespBundle())
  }

  val refill = Output(new Bundle {
    val req_info = new L2TlbInnerBundle()
    val level = UInt(log2Up(Level).W)
  })
}

@chiselName
class PtwFsm()(implicit p: Parameters) extends XSModule with HasPtwConst with HasPerfEvents {
  val io = IO(new PtwFsmIO)

  val sfence = io.sfence
  val mem = io.mem
  val satp = io.csr.satp
  val flush = io.sfence.valid || io.csr.satp.changed

  val s_idle :: s_addr_check :: s_mem_req :: s_mem_resp :: s_check_pte :: Nil = Enum(5)
  val state = RegInit(s_idle)
  val level = RegInit(0.U(log2Up(Level).W))
  val af_level = RegInit(0.U(log2Up(Level).W)) // access fault return this level
  val ppn = Reg(UInt(ppnLen.W))
  val vpn = Reg(UInt(vpnLen.W))
  val levelNext = level + 1.U
  val l1Hit = Reg(Bool())
  val memPte = mem.resp.bits.asTypeOf(new PteBundle().cloneType)
  io.req.ready := state === s_idle

  val finish = WireInit(false.B)
  val sent_to_pmp = state === s_addr_check || (state === s_check_pte && !finish)
  val accessFault = RegEnable(io.pmp.resp.ld || io.pmp.resp.mmio, sent_to_pmp)
  val pageFault = memPte.isPf(level)
  switch (state) {
    is (s_idle) {
      when (io.req.fire()) {
        val req = io.req.bits
        state := s_addr_check
        level := Mux(req.l1Hit, 1.U, 0.U)
        af_level := Mux(req.l1Hit, 1.U, 0.U)
        ppn := Mux(req.l1Hit, io.req.bits.ppn, satp.ppn)
        vpn := io.req.bits.req_info.vpn
        l1Hit := req.l1Hit
        accessFault := false.B
      }
    }

    is (s_addr_check) {
      state := s_mem_req
    }

    is (s_mem_req) {
      when (mem.req.fire()) {
        state := s_mem_resp
      }
      when (accessFault) {
        state := s_check_pte
      }
    }

    is (s_mem_resp) {
      when(mem.resp.fire()) {
        state := s_check_pte
        af_level := af_level + 1.U
      }
    }

    is (s_check_pte) {
      when (io.resp.valid) { // find pte already or accessFault (mentioned below)
        when (io.resp.fire()) {
          state := s_idle
        }
        finish := true.B
      }.elsewhen(io.mq.valid) { // the next level is pte, go to miss queue
        when (io.mq.fire()) {
          state := s_idle
        }
        finish := true.B
      } otherwise { // go to next level, access the memory, need pmp check first
        when (io.pmp.resp.ld) { // pmp check failed, raise access-fault
          // do nothing, RegNext the pmp check result and do it later (mentioned above)
        }.otherwise { // go to next level.
          assert(level === 0.U)
          level := levelNext
          state := s_mem_req
        }
      }
    }
  }

  when (sfence.valid) {
    state := s_idle
    accessFault := false.B
  }

  // memPte is valid when at s_check_pte. when mem.resp.fire, it's not ready.
  val is_pte = memPte.isLeaf() || memPte.isPf(level)
  val find_pte = is_pte
  val to_find_pte = level === 1.U && !is_pte
  val source = RegEnable(io.req.bits.req_info.source, io.req.fire())
  io.resp.valid := state === s_check_pte && (find_pte || accessFault)
  io.resp.bits.source := source
  io.resp.bits.resp.apply(pageFault && !accessFault, accessFault, Mux(accessFault, af_level, level), memPte, vpn, satp.asid)

  io.mq.valid := state === s_check_pte && to_find_pte && !accessFault
  io.mq.bits.req_info.source := source
  io.mq.bits.req_info.vpn := vpn
  io.mq.bits.l3.valid := true.B
  io.mq.bits.l3.bits := memPte.ppn

  assert(level =/= 2.U || level =/= 3.U)

  val l1addr = MakeAddr(satp.ppn, getVpnn(vpn, 2))
  val l2addr = MakeAddr(Mux(l1Hit, ppn, memPte.ppn), getVpnn(vpn, 1))
  val mem_addr = Mux(af_level === 0.U, l1addr, l2addr)
  io.pmp.req.valid := DontCare // samecycle, do not use valid
  io.pmp.req.bits.addr := mem_addr
  io.pmp.req.bits.size := 3.U // TODO: fix it
  io.pmp.req.bits.cmd := TlbCmd.read

  mem.req.valid := state === s_mem_req && !io.mem.mask && !accessFault
  mem.req.bits.addr := mem_addr
  mem.req.bits.id := FsmReqID.U(bMemID.W)

  io.refill.req_info.vpn := vpn
  io.refill.level := level
  io.refill.req_info.source := source

  XSDebug(p"[fsm] state:${state} level:${level} notFound:${pageFault}\n")

  // perf
  XSPerfAccumulate("fsm_count", io.req.fire())
  for (i <- 0 until PtwWidth) {
    XSPerfAccumulate(s"fsm_count_source${i}", io.req.fire() && io.req.bits.req_info.source === i.U)
  }
  XSPerfAccumulate("fsm_busy", state =/= s_idle)
  XSPerfAccumulate("fsm_idle", state === s_idle)
  XSPerfAccumulate("resp_blocked", io.resp.valid && !io.resp.ready)
  XSPerfAccumulate("mem_count", mem.req.fire())
  XSPerfAccumulate("mem_cycle", BoolStopWatch(mem.req.fire, mem.resp.fire(), true))
  XSPerfAccumulate("mem_blocked", mem.req.valid && !mem.req.ready)

  TimeOutAssert(state =/= s_idle, timeOutThreshold, "page table walker time out")

  val perfEvents = Seq(
    ("fsm_count         ", io.req.fire()                                     ),
    ("fsm_busy          ", state =/= s_idle                                  ),
    ("fsm_idle          ", state === s_idle                                  ),
    ("resp_blocked      ", io.resp.valid && !io.resp.ready                   ),
    ("mem_count         ", mem.req.fire()                                    ),
    ("mem_cycle         ", BoolStopWatch(mem.req.fire, mem.resp.fire(), true)),
    ("mem_blocked       ", mem.req.valid && !mem.req.ready                   ),
  )
  generatePerfEvent()
}
