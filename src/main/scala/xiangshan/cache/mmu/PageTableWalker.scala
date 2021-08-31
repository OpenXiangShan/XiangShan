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
    val l2Hit = Bool()
    val vpn = UInt(vpnLen.W)
    val ppn = UInt(ppnLen.W)
  }))
  val resp = DecoupledIO(new Bundle {
    val source = UInt(bPtwWidth.W)
    val resp = new PtwResp
  })

  val mem = new Bundle {
    val req = DecoupledIO(new Bundle {
      val addr = UInt(PAddrBits.W)
    })
    val resp = Flipped(ValidIO(UInt(XLEN.W)))
  }

  val csr = Input(new TlbCsrBundle)
  val sfence = Input(new SfenceBundle)
  val sfenceLatch = Output(Bool())
  val refill = Output(new Bundle {
    val vpn = UInt(vpnLen.W)
    val level = UInt(log2Up(Level).W)
    val memAddr = UInt(PAddrBits.W)
  })
}

class PtwFsm()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new PtwFsmIO)

  val sfence = io.sfence
  val mem = io.mem
  val satp = io.csr.satp

  val s_idle :: s_mem_req :: s_mem_resp :: s_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val level = RegInit(0.U(log2Up(Level).W))
  val ppn = Reg(UInt(ppnLen.W))
  val vpn = Reg(UInt(vpnLen.W))
  val levelNext = level + 1.U

  val sfenceLatch = RegEnable(false.B, init = false.B, mem.resp.valid) // NOTE: store sfence to disable mem.resp.fire(), but not stall other ptw req
  val memAddrReg = RegEnable(mem.req.bits.addr, mem.req.fire())
  val l1Hit = Reg(Bool())
  val l2Hit = Reg(Bool())

  val memPte = mem.resp.bits.asTypeOf(new PteBundle().cloneType)
  val memPteReg = RegEnable(memPte, mem.resp.fire())

  val notFound = WireInit(false.B)
  switch (state) {
    is (s_idle) {
      when (io.req.fire()) {
        val req = io.req.bits
        state := s_mem_req
        level := Mux(req.l2Hit, 2.U, Mux(req.l1Hit, 1.U, 0.U))
        ppn := Mux(req.l2Hit || req.l1Hit, io.req.bits.ppn, satp.ppn)
        vpn := io.req.bits.vpn
        l1Hit := req.l1Hit
        l2Hit := req.l2Hit
      }
    }

    is (s_mem_req) {
      when (mem.req.fire()) {
        state := s_mem_resp
      }
    }

    is (s_mem_resp) {
      when (mem.resp.fire()) {
        when (memPte.isLeaf() || memPte.isPf(level)) {
          state := s_resp
          notFound := memPte.isPf(level)
        }.otherwise {
          when (level =/= 2.U) {
            level := levelNext
            state := s_mem_req
          }.otherwise {
            state := s_resp
            notFound := true.B
          }
        }
      }
    }

    is (s_resp) {
      when (io.resp.fire()) {
        state := s_idle
      }
    }
  }

  when (sfence.valid) {
    state := s_idle
    when (state === s_mem_resp && !mem.resp.fire() || state === s_mem_req && mem.req.fire()) {
      sfenceLatch := true.B
    }
  }

  val finish = mem.resp.fire()  && (memPte.isLeaf() || memPte.isPf(level) || level === 2.U)
  val resp_pf = Reg(Bool())
  val resp_level = Reg(UInt(2.W))
  val resp_pte = Reg(new PteBundle())
  when (finish && !sfenceLatch) {
    resp_pf := level === 3.U || notFound
    resp_level := level
    resp_pte := memPte
  }
  io.resp.valid := state === s_resp
  io.resp.bits.source := RegEnable(io.req.bits.source, io.req.fire())
  io.resp.bits.resp.apply(resp_pf, resp_level, resp_pte, vpn)
  io.req.ready := state === s_idle

  val l1addr = MakeAddr(satp.ppn, getVpnn(vpn, 2))
  val l2addr = MakeAddr(Mux(l1Hit, ppn, memPteReg.ppn), getVpnn(vpn, 1))
  val l3addr = MakeAddr(Mux(l2Hit, ppn, memPteReg.ppn), getVpnn(vpn, 0))
  mem.req.valid := state === s_mem_req && !sfenceLatch
  mem.req.bits.addr := Mux(level === 0.U, l1addr, Mux(level === 1.U, l2addr, l3addr))

  io.refill.vpn := vpn
  io.refill.level := level
  io.refill.memAddr := memAddrReg
  io.sfenceLatch := sfenceLatch

  XSDebug(p"[fsm] state:${state} level:${level} sfenceLatch:${sfenceLatch} notFound:${notFound}\n")

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
}
