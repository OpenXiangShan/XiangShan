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

/* ptw finite state machine, the actual page table walker
 */
class PtwFsmIO()(implicit p: Parameters) extends PtwBundle {
  val req = Flipped(DecoupledIO(new Bundle {
    val source = UInt(bPtwWidth.W)
    val l1Hit = Bool()
    val vpn = UInt(vpnLen.W)
    val ppn = UInt(ppnLen.W)
  }))
  val resp = DecoupledIO(new Bundle {
    val source = UInt(bPtwWidth.W)
    val resp = new PtwResp
  })

  val mq = DecoupledIO(new L2TlbMQInBundle())

  val mem = new Bundle {
    val req = DecoupledIO(new L2TlbMemReqBundle())
    val resp = Flipped(ValidIO(UInt(XLEN.W)))
    val mask = Input(Bool())
  }

  val csr = Input(new TlbCsrBundle)
  val sfence = Input(new SfenceBundle)
  val refill = Output(new Bundle {
    val vpn = UInt(vpnLen.W)
    val level = UInt(log2Up(Level).W)
  })
}

@chiselName
class PtwFsm()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new PtwFsmIO)

  val sfence = io.sfence
  val mem = io.mem
  val satp = io.csr.satp

  val s_idle :: s_mem_req :: s_mem_resp :: s_check_pte :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val level = RegInit(0.U(log2Up(Level).W))
  val ppn = Reg(UInt(ppnLen.W))
  val vpn = Reg(UInt(vpnLen.W))
  val levelNext = level + 1.U
  val l1Hit = Reg(Bool())
  val memPte = mem.resp.bits.asTypeOf(new PteBundle().cloneType)
  io.req.ready := state === s_idle

  val pageFault = WireInit(false.B)
  switch (state) {
    is (s_idle) {
      when (io.req.fire()) {
        val req = io.req.bits
        state := s_mem_req
        level := Mux(req.l1Hit, 1.U, 0.U)
        ppn := Mux(req.l1Hit, io.req.bits.ppn, satp.ppn)
        vpn := io.req.bits.vpn
        l1Hit := req.l1Hit
      }
    }

    is (s_mem_req) {
      when (mem.req.fire()) {
        state := s_mem_resp
      }
    }

    is (s_mem_resp) {
      when(mem.resp.fire()) {
        state := s_check_pte
      }
    }

    is (s_check_pte) {
      when (memPte.isLeaf() || memPte.isPf(level)) {
        when (io.resp.fire()) {
          state := s_idle
        }
        pageFault := memPte.isPf(level)
      }.otherwise {
        when (level =/= (Level-2).U) { // when level is 1.U, finish
          level := levelNext
          state := s_mem_req
        }.otherwise {
          when (io.mq.fire()) {
            state := s_idle
          }
        }
      }
    }
  }

  when (sfence.valid) {
    state := s_idle
  }

  val is_pte = memPte.isLeaf() || memPte.isPf(level)
  val find_pte = is_pte
  val to_find_pte = level === 1.U && !is_pte
  val source = RegEnable(io.req.bits.source, io.req.fire())
  io.resp.valid := state === s_check_pte && find_pte
  io.resp.bits.source := source
  io.resp.bits.resp.apply(pageFault, level, memPte, vpn)

  io.mq.valid := state === s_check_pte && to_find_pte
  io.mq.bits.source := source
  io.mq.bits.vpn := vpn
  io.mq.bits.l3.valid := true.B
  io.mq.bits.l3.bits := memPte.ppn

  assert(level =/= 2.U || level =/= 3.U)

  val l1addr = MakeAddr(satp.ppn, getVpnn(vpn, 2))
  val l2addr = MakeAddr(Mux(l1Hit, ppn, memPte.ppn), getVpnn(vpn, 1))
  mem.req.valid := state === s_mem_req && !io.mem.mask
  mem.req.bits.addr := Mux(level === 0.U, l1addr, l2addr)
  mem.req.bits.id := MSHRSize.U(bMemID.W)

  io.refill.vpn := vpn
  io.refill.level := level

  XSDebug(p"[fsm] state:${state} level:${level} notFound:${pageFault}\n")

  // perf
  XSPerfAccumulate("fsm_count", io.req.fire())
  for (i <- 0 until PtwWidth) {
    XSPerfAccumulate(s"fsm_count_source${i}", io.req.fire() && io.req.bits.source === i.U)
  }
  XSPerfAccumulate("fsm_busy", state =/= s_idle)
  XSPerfAccumulate("fsm_idle", state === s_idle)
  XSPerfAccumulate("resp_blocked", io.resp.valid && !io.resp.ready)
  XSPerfAccumulate("mem_count", mem.req.fire())
  XSPerfAccumulate("mem_cycle", BoolStopWatch(mem.req.fire, mem.resp.fire(), true))
  XSPerfAccumulate("mem_blocked", mem.req.valid && !mem.req.ready)

  TimeOutAssert(state =/= s_idle, timeOutThreshold, "page table walker time out")
}
