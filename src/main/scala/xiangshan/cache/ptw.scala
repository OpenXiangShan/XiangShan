package xiangshan.cache

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap
import xiangshan.mem._
import xiangshan.mem.pipeline._
import bus.simplebus._

trait HasPtwConst extends HasTlbConst with MemoryOpConstants{
  val PtwWidth = 2
}

abstract class PtwBundle extends XSBundle with HasPtwConst
abstract class PtwModule extends XSModule with HasPtwConst 

class PteBundle extends PtwBundle{
  val reserved  = UInt(pteResLen.W)
  val ppn  = UInt(ppnLen.W)
  val rsw  = UInt(2.W)
  val perm = new Bundle {
    val d    = Bool()
    val a    = Bool()
    val g    = Bool()
    val u    = Bool()
    val x    = Bool()
    val w    = Bool()
    val r    = Bool()
    val v    = Bool()
  }

  def isPf() = {
    !perm.v || (!perm.r && perm.w)
  }

  def isLeaf() = {
    !isPf() && (perm.r || perm.x)
  }

  override def toPrintable: Printable = {
    p"ppn:0x${Hexadecimal(ppn)} perm:b${Binary(perm.asUInt)}"
  }
}

class PtwEntry(tagLen: Int) extends PtwBundle {
  val tag = UInt(tagLen.W)
  val ppn = UInt(ppnLen.W)
  val perm = new PermBundle

  // TODO: add superpage
  def hit(addr: UInt) = {
    require(addr.getWidth >= PAddrBits)
    tag === addr(PAddrBits-1, PAddrBits-tagLen)
  }

  def refill(addr: UInt, pte: UInt) {
    tag := addr(PAddrBits-1, PAddrBits-tagLen)
    ppn := pte.asTypeOf(pteBundle).ppn
    perm := pte.asTypeOf(pteBundle).perm
  }

  def genPtwEntry(addr: UInt, pte: UInt) = {
    val e = Wire(new PtwEntry(tagLen))
    e.tag := addr(PAddrBits-1, PAddrBits-tagLen)
    e.ppn := pte.asTypeOf(pteBundle).ppn
    e.perm := pte.asTypeOf(pteBundle).perm
    e
  }

  override def cloneType: this.type = (new PtwEntry(tagLen)).asInstanceOf[this.type]

  override def toPrintable: Printable = {
    p"tag:0x${Hexadecimal(tag)} ppn:0x${Hexadecimal(ppn)} perm:${perm}"
  }
}

class PtwReq extends PtwBundle {
  val vpn = UInt(vpnLen.W)
  val idx = UInt(RoqIdxWidth.W) // itlb could ignore it

  override def toPrintable: Printable = {
    p"vpn:0x${Hexadecimal(vpn)} idx:${idx}"
  }
}

class PtwResp extends PtwBundle {
  val entry = new TlbEntry
  val idx = UInt(RoqIdxWidth.W)
  val pf  = Bool() // simple pf no matter cmd

  override def toPrintable: Printable = {
    p"entry:${entry} idx:${idx} pf:${pf}"
  }
}

class PtwIO extends PtwBundle {
  val tlb = Vec(PtwWidth, Flipped(new TlbPtwIO))
  //val mem = new SimpleBusUC(addrBits = PAddrBits) // Use Dcache temp
  val mem = new DCacheLoadIO
}

object ValidHold {
  def apply(infire: Bool, outfire: Bool, flush: Bool = false.B ) = {
    val valid = RegInit(false.B)
    when (outfire) { valid := false.B }
    when (infire) { valid := true.B }
    when (flush) { valid := false.B } // NOTE: the flush will flush in & out, is that ok?
    valid
  }
}

object OneCycleValid {
  def apply(fire: Bool, flush: Bool = false.B) = {
    val valid = RegInit(false.B)
    when (valid) { valid := false.B }
    when (fire) { valid := true.B }
    when (false.B) { valid := false.B }
    valid
  }
}

class PTW extends PtwModule {
  val io = IO(new PtwIO)

  val arb = Module(new Arbiter(io.tlb(0).req.bits.cloneType, PtwWidth))
  arb.io.in <> io.tlb.map(_.req)
  val arbChosen = RegEnable(arb.io.chosen, arb.io.out.fire())
  val req = RegEnable(arb.io.out.bits, arb.io.out.fire())
  val resp  = VecInit(io.tlb.map(_.resp))
  
  val valid = ValidHold(arb.io.out.fire(), resp(arbChosen).fire())
  val validOneCycle = OneCycleValid(arb.io.out.fire())
  arb.io.out.ready := !valid || resp(arbChosen).fire()

  val mem    = io.mem
  val sfence = WireInit(0.U.asTypeOf(new SfenceBundle))
  val csr    = WireInit(0.U.asTypeOf(new TlbCsrBundle))
  val satp   = csr.satp
  val priv   = csr.priv
  BoringUtils.addSink(sfence, "SfenceBundle")
  BoringUtils.addSink(csr, "TLBCSRIO")

  val memRdata = mem.resp.bits.data
  val memPte = memRdata.asTypeOf(new PteBundle)

  // two level: l2-tlb-cache && pde/pte-cache
  // l2-tlb-cache is ram-larger-edition tlb
  // pde/pte-cache is cache of page-table, speeding up ptw

  // may seperate valid bits to speed up sfence's flush
  // Reg/Mem/SyncReadMem is not sure now
  val tagLen1 = PAddrBits - log2Up(XLEN/8)
  val tagLen2 = PAddrBits - log2Up(XLEN/8) - log2Up(PtwL2EntrySize)
  val tlbl2 = SyncReadMem(TlbL2EntrySize, new TlbEntry)
  val tlbv  = RegInit(0.U(TlbL2EntrySize.W))
  val ptwl1 = Reg(Vec(PtwL1EntrySize, new PtwEntry(tagLen = tagLen1)))
  val l1v   = RegInit(0.U(PtwL1EntrySize.W))
  val ptwl2 = SyncReadMem(PtwL2EntrySize, new PtwEntry(tagLen = tagLen2)) // NOTE: the Mem could be only single port(r&w)
  val l2v   = RegInit(0.U(PtwL2EntrySize.W))

  // fsm
  val state_idle :: state_req :: state_wait_resp :: state_wait_ready :: Nil = Enum(4)
  val state = RegInit(state_idle)
  val level = RegInit(0.U(2.W)) // 0/1/2
  val latch = Reg(resp(0).bits.cloneType)

  // tlbl2
  val (tlbHit, tlbHitData) = {
    // tlbl2 is by addr
    // TODO: optimize tlbl2'l2 tag len
    val ramData = tlbl2.read(req.vpn(log2Up(TlbL2EntrySize)-1, 0), validOneCycle)
    val vidx = RegEnable(tlbv(req.vpn(log2Up(TlbL2EntrySize)-1, 0)), validOneCycle)
    (ramData.hit(req.vpn) && vidx, ramData) // TODO: optimize tag
    // TODO: add exception and refill
  }

  def MakeAddr(ppn: UInt, off: UInt) = {
    require(off.getWidth == 9)
    Cat(ppn, off, 0.U(log2Up(XLEN/8).W))(PAddrBits-1, 0)
  }

  def getVpnn(vpn: UInt, idx: Int) = {
    vpn(vpnnLen*(idx+1)-1, vpnnLen*idx)
  }

  // ptwl1
  val l1addr = MakeAddr(satp.ppn, getVpnn(req.vpn, 2))
  val (l1Hit, l1HitData) = { // TODO: add excp
    // 16 terms may casue long latency, so divide it into 2 stage, like l2tlb
    val hitVecT = ptwl1.zipWithIndex.map{case (a,b) => a.hit(l1addr) && l1v(b) }
    val hitVec  = hitVecT.map(RegEnable(_, validOneCycle)) // TODO: could have useless init value
    val hitData = ParallelMux(hitVec zip ptwl1)
    val hit     = ParallelOR(hitVec).asBool
    (hit, hitData)
  }

  // ptwl2
  val l1MemBack = mem.resp.fire() && state===state_wait_resp && level===0.U
  val l1Res = Mux(l1Hit, l1HitData.ppn, RegEnable(memPte.ppn, l1MemBack))
  val l2addr = MakeAddr(l1Res, getVpnn(req.vpn, 1))
  val (l2Hit, l2HitData) = { // TODO: add excp
    val readRam = (l1Hit && level===0.U && state===state_req) || (mem.resp.fire() && state===state_wait_resp && level===0.U)
    val ridx = l2addr(log2Up(PtwL2EntrySize)-1+log2Up(XLEN/8), log2Up(XLEN/8))
    val ramData = ptwl2.read(ridx, readRam)
    val vidx = RegEnable(l2v(ridx), readRam)
    (ramData.hit(l2addr), ramData) // TODO: optimize tag
  }

  // ptwl3
  /* ptwl3 has not cache
   * ptwl3 may be functional conflict with l2-tlb
   * if l2-tlb does not hit, ptwl3 would not hit (mostly)
   */
  val l2MemBack = mem.resp.fire() && state===state_wait_resp && level===1.U
  val l2Res = Mux(l2Hit, l2HitData.ppn, RegEnable(memPte.ppn, l1MemBack))
  val l3addr = MakeAddr(l2Res, getVpnn(req.vpn, 0))

  // fsm
  assert(!(level===3.U))
  assert(!(tlbHit && (mem.req.valid || state===state_wait_resp))) // when tlb hit, should not req/resp.valid

  switch (state) {
    is (state_idle) {
      when (valid) {
        state := state_req
        level := 0.U
      }
    }

    is (state_req) {
      when (tlbHit) {
        when (resp(arbChosen).ready) {
          state := state_idle
        }.otherwise {
          state := state_wait_ready
        }
      }.elsewhen (l1Hit && level===0.U || l2Hit && level===1.U) {
        level := level + 1.U // TODO: consider superpage
      }.elsewhen (mem.req.ready) {
        state := state_wait_resp
        assert(!(level === 3.U)) // NOTE: pte is not found after 3 layers(software system is wrong)
      }
    }

    is (state_wait_resp) {
      when (mem.resp.fire()) {
        when (memPte.isLeaf() || memPte.isPf()) {
          when (resp(arbChosen).ready) {
            state := state_idle
          }.otherwise {
            state := state_wait_ready
            latch.entry := new TlbEntry().genTlbEntry(memRdata, level, req.vpn)
            latch.pf := memPte.isPf()
          }
        }.otherwise {
          state := state_req
          level := level + 1.U
        }
      }
    }

    is (state_wait_ready) {
      when (resp(arbChosen).ready) {
        state := state_idle
      }
    }
  }

  // mem:
  mem.req.valid := state === state_req && 
                      ((level===0.U && !tlbHit && !l1Hit) ||
                      (level===1.U && !l2Hit) ||
                      (level===2.U))
  mem.req.bits.cmd := M_XRD
  mem.req.bits.addr := Mux(level===0.U, l1addr/*when l1Hit, DontCare, when l1miss, l1addr*/,
           Mux(level===1.U, Mux(l2Hit, l3addr, l2addr)/*when l2Hit, l3addr, when l2miss, l2addr*/,
           l3addr))
  mem.req.bits.data := DontCare
  mem.req.bits.mask := VecInit(Fill(mem.req.bits.mask.getWidth, true.B)).asUInt
  mem.req.bits.meta := DontCare // TODO: check it
  mem.resp.ready := true.B // TODO: mem.resp.ready := state===state_wait_resp
  assert(!mem.resp.valid || state===state_wait_resp, "mem.resp.valid:%d state:%d", mem.resp.valid, state)

  // resp
  val ptwFinish = (state===state_req && tlbHit && level===0.U) || ((memPte.isLeaf() || memPte.isPf()) && mem.resp.fire()) || state===state_wait_ready
  for(i <- 0 until PtwWidth) {
    resp(i).valid := valid && arbChosen===i.U && ptwFinish // TODO: add resp valid logic
    resp(i).bits.entry := Mux(tlbHit, tlbHitData,
      Mux(state===state_wait_ready, latch.entry, new TlbEntry().genTlbEntry(memRdata, level, req.vpn)))
    resp(i).bits.idx := req.idx
    resp(i).bits.pf  := Mux(tlbHit, false.B, Mux(state===state_wait_ready, latch.pf, memPte.isPf())) 
    // TODO: the pf must not be correct, check it
  }

  // sfence
  // for ram is syncReadMem, so could not flush conditionally
  // l3 may be conflict with l2tlb??, may be we could combine l2-tlb with l3-ptw
  when (sfence.valid) {
    tlbv := 0.U
    l1v := 0.U
    l2v := 0.U
  }

  // refill
  assert(!mem.resp.fire() || state===state_wait_resp)
  when (mem.resp.fire() && !memPte.isPf()) {
    when (state===state_wait_resp && level===0.U && !memPte.isPf) {
      val refillIdx = LFSR64()(log2Up(PtwL1EntrySize)-1,0) // TODO: may be LRU
      ptwl1(refillIdx).refill(l1addr, memRdata)
      l1v := l1v | UIntToOH(refillIdx)
    }
    when (state===state_wait_resp && level===1.U && !memPte.isPf) {
      val l2addrStore = RegEnable(l2addr, mem.req.fire() && state===state_req && level===1.U)
      val refillIdx = getVpnn(req.vpn, 1)(log2Up(PtwL2EntrySize)-1, 0)
      ptwl2.write(refillIdx, new PtwEntry(tagLen2).genPtwEntry(l2addrStore, memRdata))
      l2v := l2v | UIntToOH(refillIdx)
    }
    when (state===state_wait_resp && memPte.isLeaf() && !memPte.isPf) {
      val refillIdx = getVpnn(req.vpn, 0)(log2Up(TlbL2EntrySize)-1, 0)
      tlbl2.write(refillIdx, new TlbEntry().genTlbEntry(memRdata, level, req.vpn))
      tlbv := tlbv | UIntToOH(refillIdx)
    }
  }

  def PrintFlag(en: Bool, flag: Bool, nameEnable: String, nameDisable: String): Unit = {
    when(flag) {
      XSDebug(false, en, nameEnable)
    }.otherwise {
      XSDebug(false, en, nameDisable)
    }
  }

  XSDebug(validOneCycle, "**New Ptw Req from ")
  PrintFlag(validOneCycle, arbChosen===0.U, "DTLB**:", "ITLB**:")
  XSDebug(false, validOneCycle, p"(v:${validOneCycle} r:${arb.io.out.ready}) vpn:0x${Hexadecimal(req.vpn)} (roq)idx:${req.idx}\n")
  XSDebug(resp(arbChosen).fire(), "**Ptw Resp to ")
  PrintFlag(resp(arbChosen).fire(), arbChosen===0.U, "DTLB**:\n", "ITLB**\n")
  XSDebug(resp(arbChosen).fire(), p"(v:${resp(arbChosen).valid} r:${resp(arbChosen).ready}) entry:${resp(arbChosen).bits.entry} (roq)idx:${resp(arbChosen).bits.idx} pf:${resp(arbChosen).bits.pf}\n")

  XSDebug(sfence.valid, p"Sfence: sfence instr here ${sfence.bits}\n")
  XSDebug(valid, p"CSR: ${csr}\n")

  XSDebug(valid, p"vpn2:0x${Hexadecimal(getVpnn(req.vpn, 2))} vpn1:0x${Hexadecimal(getVpnn(req.vpn, 1))} vpn0:0x${Hexadecimal(getVpnn(req.vpn, 0))}\n")
  XSDebug(valid, p"state:${state} level:${level} tlbHit:${tlbHit} l1addr:0x${Hexadecimal(l1addr)} l1Hit:${l1Hit} l2addr:0x${Hexadecimal(l2addr)} l2Hit:${l2Hit}  l3addr:0x${Hexadecimal(l3addr)} memReq(v:${mem.req.valid} r:${mem.req.ready})\n")

  XSDebug(mem.req.fire(), p"mem req fire addr:0x${Hexadecimal(mem.req.bits.addr)}\n")
  XSDebug(mem.resp.fire(), p"mem resp fire rdata:0x${Hexadecimal(mem.resp.bits.data)} Pte:${memPte}\n")
}