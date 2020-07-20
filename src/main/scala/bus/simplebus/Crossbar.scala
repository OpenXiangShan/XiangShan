/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
* 
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2. 
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2 
* 
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER 
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR 
* FIT FOR A PARTICULAR PURPOSE.  
*
* See the Mulan PSL v2 for more details.  
***************************************************************************************/

package bus.simplebus

import chisel3._
import chisel3.util._

import utils._

class SimpleBusCrossbar1toN(addressSpace: List[(Long, Long)]) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SimpleBusUC)
    val out = Vec(addressSpace.length, new SimpleBusUC)
  })

  val s_idle :: s_resp :: s_error :: Nil = Enum(3)
  val state = RegInit(s_idle)

  // select the output channel according to the address
  val addr = io.in.req.bits.addr
  val outSelVec = VecInit(addressSpace.map(
    range => (addr >= range._1.U && addr < (range._1 + range._2).U)))
  val outSelIdx = PriorityEncoder(outSelVec)
  val outSel = io.out(outSelIdx)
  val outSelIdxResp = RegEnable(outSelIdx, outSel.req.fire() && (state === s_idle))
  val outSelResp = io.out(outSelIdxResp)
  val reqInvalidAddr = io.in.req.valid && !outSelVec.asUInt.orR

  when(!(!io.in.req.valid || outSelVec.asUInt.orR) || !(!(io.in.req.valid && outSelVec.asUInt.andR))){printf("[ERROR] bad addr %x, time %d\n", addr, GTimer())}
  // assert(!io.in.req.valid || outSelVec.asUInt.orR, "address decode error, bad addr = 0x%x\n", addr)
  assert(!(io.in.req.valid && outSelVec.asUInt.andR), "address decode error, bad addr = 0x%x\n", addr)

  // bind out.req channel
  (io.out zip outSelVec).map { case (o, v) => {
    o.req.bits := io.in.req.bits
    o.req.valid := v && (io.in.req.valid && (state === s_idle))
    o.resp.ready := v
  }}

  switch (state) {
    is (s_idle) { 
      when (outSel.req.fire()) { state := s_resp } 
      when (reqInvalidAddr) { state := s_error } 
    }
    is (s_resp) { when (outSelResp.resp.fire()) { state := s_idle } }
    is (s_error) { when(io.in.resp.fire()){ state := s_idle } }
  }

  io.in.resp.valid := outSelResp.resp.fire() || state === s_error
  io.in.resp.bits <> outSelResp.resp.bits
  // io.in.resp.bits.exc.get := state === s_error
  outSelResp.resp.ready := io.in.resp.ready
  io.in.req.ready := outSel.req.ready || reqInvalidAddr

  Debug() {
    when (state === s_idle && io.in.req.valid) {
      printf(p"${GTimer()}: xbar: in.req: ${io.in.req.bits}\n")
    }

    when (outSel.req.fire()) {
      printf(p"${GTimer()}: xbar: outSelIdx = ${outSelIdx}, outSel.req: ${outSel.req.bits}\n")
    }
    when (outSel.resp.fire()) {
      printf(p"${GTimer()}: xbar: outSelIdx= ${outSelIdx}, outSel.resp: ${outSel.resp.bits}\n")
    }

    when (io.in.resp.fire()) {
      printf(p"${GTimer()}: xbar: in.resp: ${io.in.resp.bits}\n")
    }
  }
}

class SimpleBusCrossbarNto1(n: Int, userBits:Int = 0) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(n, new SimpleBusUC(userBits)))
    val out = new SimpleBusUC(userBits)
  })

  val s_idle :: s_readResp :: s_writeResp :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val lockWriteFun = ((x: SimpleBusReqBundle) => x.isWrite() && x.isBurst())
  val inputArb = Module(new LockingArbiter(chiselTypeOf(io.in(0).req.bits), n, 8, Some(lockWriteFun)))
  (inputArb.io.in zip io.in.map(_.req)).map{ case (arb, in) => arb <> in }
  val thisReq = inputArb.io.out
  assert(!(thisReq.valid && !thisReq.bits.isRead() && !thisReq.bits.isWrite()))
  val inflightSrc = Reg(UInt(log2Up(n).W))

  io.out.req.bits := thisReq.bits
  // bind correct valid and ready signals
  io.out.req.valid := thisReq.valid && (state === s_idle)
  thisReq.ready := io.out.req.ready && (state === s_idle)

  io.in.map(_.resp.bits := io.out.resp.bits)
  io.in.map(_.resp.valid := false.B)
  (io.in(inflightSrc).resp, io.out.resp) match { case (l, r) => {
    l.valid := r.valid
    r.ready := l.ready
  }}

  switch (state) {
    is (s_idle) {
      when (thisReq.fire()) {
        inflightSrc := inputArb.io.chosen
        when (thisReq.bits.isRead()) { state := s_readResp }
        .elsewhen (thisReq.bits.isWriteLast() || thisReq.bits.isWriteSingle()) { state := s_writeResp }
      }
    }
    is (s_readResp) { when (io.out.resp.fire() && io.out.resp.bits.isReadLast()) { state := s_idle } }
    is (s_writeResp) { when (io.out.resp.fire()) { state := s_idle } }
  }
}

class SimpleBusCrossbar(n: Int, addressSpace: List[(Long, Long)]) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(n, new SimpleBusUC))
    val out = Vec(addressSpace.length, new SimpleBusUC)
  })

  val inXbar = Module(new SimpleBusCrossbarNto1(n))
  val outXbar = Module(new SimpleBusCrossbar1toN(addressSpace))
  inXbar.io.in <> io.in
  outXbar.io.in <> inXbar.io.out
  io.out <> outXbar.io.out
}
