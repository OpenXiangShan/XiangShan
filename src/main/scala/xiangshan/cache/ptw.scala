package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.ExcitingUtils._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLClientNode, TLMasterParameters, TLMasterPortParameters}

trait HasPtwConst extends HasTlbConst with MemoryOpConstants{
  val PtwWidth = 2

  def MakeAddr(ppn: UInt, off: UInt) = {
    require(off.getWidth == 9)
    Cat(ppn, off, 0.U(log2Up(XLEN/8).W))(PAddrBits-1, 0)
  }

  def getVpnn(vpn: UInt, idx: Int) = {
    vpn(vpnnLen*(idx+1)-1, vpnnLen*idx)
  }
}

abstract class PtwBundle extends XSBundle with HasPtwConst
abstract class PtwModule(outer: PTW) extends LazyModuleImp(outer)
  with HasXSParameter with HasXSLog with HasPtwConst

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

  override def toPrintable: Printable = {
    p"vpn:0x${Hexadecimal(vpn)}"
  }
}

class PtwResp extends PtwBundle {
  val entry = new TlbEntry
  val pf  = Bool() // simple pf no matter cmd

  override def toPrintable: Printable = {
    p"entry:${entry} pf:${pf}"
  }
}

class PtwIO extends PtwBundle {
  val tlb = Vec(PtwWidth, Flipped(new TlbPtwIO))
  val sfence = Input(new SfenceBundle)
  val csr = Input(new TlbCsrBundle)
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

class PTW()(implicit p: Parameters) extends LazyModule {

  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = Seq(TLMasterParameters.v1(
      "ptw"
    ))
  )))

  lazy val module = new PTWImp(this)
}

class PTWImp(outer: PTW) extends PtwModule(outer){

  val (mem, edge) = outer.node.out.head
  require(mem.d.bits.data.getWidth == l1BusDataWidth, "PTW: tilelink width does not match")

  val io = IO(new PtwIO)

  val arb = Module(new Arbiter(new PtwReq, PtwWidth))
  arb.io.in <> VecInit(io.tlb.map(_.req))
  val arbChosen = RegEnable(arb.io.chosen, arb.io.out.fire())
  val req = RegEnable(arb.io.out.bits, arb.io.out.fire())
  val resp  = VecInit(io.tlb.map(_.resp))


  val valid = ValidHold(arb.io.out.fire(), resp(arbChosen).fire())
  val validOneCycle = OneCycleValid(arb.io.out.fire())
  arb.io.out.ready := !valid// || resp(arbChosen).fire()

  val sfence = io.sfence
  val csr    = io.csr
  val satp   = csr.satp
  val priv   = csr.priv

  // two level: l2-tlb-cache && pde/pte-cache
  // l2-tlb-cache is ram-larger-edition tlb
  // pde/pte-cache is cache of page-table, speeding up ptw

  // may seperate valid bits to speed up sfence's flush
  // Reg/Mem/SyncReadMem is not sure now
  val tagLen1 = PAddrBits - log2Up(XLEN/8)
  val tagLen2 = PAddrBits - log2Up(XLEN/8) - log2Up(PtwL2EntrySize)
  // val tlbl2 = SyncReadMem(TlbL2EntrySize, new TlbEntry)
  val tlbl2 = Module(new SRAMTemplate(new TlbEntry, set = TlbL2EntrySize))
  val tlbv  = RegInit(0.U(TlbL2EntrySize.W)) // valid
  val tlbg  = RegInit(0.U(TlbL2EntrySize.W)) // global
  val ptwl1 = Reg(Vec(PtwL1EntrySize, new PtwEntry(tagLen = tagLen1)))
  val l1v   = RegInit(0.U(PtwL1EntrySize.W)) // valid
  val l1g   = VecInit((ptwl1.map(_.perm.g))).asUInt
  // val ptwl2 = SyncReadMem(PtwL2EntrySize, new PtwEntry(tagLen = tagLen2)) // NOTE: the Mem could be only single port(r&w)
  val ptwl2 = Module(new SRAMTemplate(new PtwEntry(tagLen = tagLen2), set = PtwL2EntrySize))
  val l2v   = RegInit(0.U(PtwL2EntrySize.W)) // valid
  val l2g   = RegInit(0.U(PtwL2EntrySize.W)) // global
  
  // mem alias
  // val memRdata = mem.d.bits.data
  val memRdata = Wire(UInt(XLEN.W))
  val memPte = memRdata.asTypeOf(new PteBundle)
  val memValid = mem.d.valid
  val memRespReady = mem.d.ready
  val memRespFire = mem.d.fire()
  val memReqReady = mem.a.ready
  val memReqFire = mem.a.fire()

  // fsm
  val state_idle :: state_req :: state_wait_resp :: state_wait_ready :: Nil = Enum(4)
  val state = RegInit(state_idle)
  val level = RegInit(0.U(2.W)) // 0/1/2
  val levelNext = level + 1.U
  val latch = Reg(new PtwResp)
  val sfenceLatch = RegEnable(false.B, init = false.B, memValid) // NOTE: store sfence to disable mem.resp.fire(), but not stall other ptw req

  /*
   * tlbl2
   */
  val (tlbHit, tlbHitData) = {
    // tlbl2 is by addr
    // TODO: optimize tlbl2'l2 tag len
    assert(tlbl2.io.r.req.ready)
    tlbl2.io.r.req.valid := validOneCycle
    tlbl2.io.r.req.bits.apply(setIdx = req.vpn(log2Up(TlbL2EntrySize-1), 0))
    val ramData = tlbl2.io.r.resp.data(0)
    // val ramData = tlbl2.r(req.vpn(log2Up(TlbL2EntrySize)-1, 0), validOneCycle)
    val vidx = RegEnable(tlbv(req.vpn(log2Up(TlbL2EntrySize)-1, 0)), validOneCycle)
    (ramData.hit(req.vpn) && vidx, ramData) // TODO: optimize tag
    // TODO: add exception and refill
  }

  /*
   * ptwl1
   */
  val l1addr = MakeAddr(satp.ppn, getVpnn(req.vpn, 2))
  val (l1Hit, l1HitData) = { // TODO: add excp
    // 16 terms may casue long latency, so divide it into 2 stage, like l2tlb
    val hitVecT = ptwl1.zipWithIndex.map{case (a,b) => a.hit(l1addr) && l1v(b) }
    val hitVec  = hitVecT.map(RegEnable(_, validOneCycle)) // TODO: could have useless init value
    val hitData = ParallelMux(hitVec zip ptwl1)
    val hit     = ParallelOR(hitVec).asBool
    (hit, hitData)
  }

  /*
   * ptwl2
   */
  val l1MemBack = memRespFire && state===state_wait_resp && level===0.U
  val l1Res = Mux(l1Hit, l1HitData.ppn, RegEnable(memPte.ppn, l1MemBack))
  val l2addr = MakeAddr(l1Res, getVpnn(req.vpn, 1))
  val (l2Hit, l2HitData) = { // TODO: add excp
    val readRam = (l1Hit && level===0.U && state===state_req) || (memRespFire && state===state_wait_resp && level===0.U)
    val ridx = l2addr(log2Up(PtwL2EntrySize)-1+log2Up(XLEN/8), log2Up(XLEN/8))
    
    assert(ptwl2.io.r.req.ready)
    ptwl2.io.r.req.valid := readRam
    ptwl2.io.r.req.bits.apply(setIdx = ridx)
    val ramData = ptwl2.io.r.resp.data(0)
    // val ramData = ptwl2.read(ridx, readRam)
    val vidx = RegEnable(l2v(ridx), readRam)
    (ramData.hit(l2addr) && vidx, ramData) // TODO: optimize tag
  }

  /* ptwl3
   * ptwl3 has not cache
   * ptwl3 may be functional conflict with l2-tlb
   * if l2-tlb does not hit, ptwl3 would not hit (mostly)
   */
  val l2MemBack = memRespFire && state===state_wait_resp && level===1.U
  val l2Res = Mux(l2Hit, l2HitData.ppn, RegEnable(memPte.ppn, l2MemBack))
  val l3addr = MakeAddr(l2Res, getVpnn(req.vpn, 0))

  /*
   * fsm
   */
  assert(!(tlbHit && (mem.a.valid || state===state_wait_resp))) // when tlb hit, should not req/resp.valid

  val notFound = WireInit(false.B)
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
      } .elsewhen (l1Hit && level===0.U || l2Hit && level===1.U) {
        level := levelNext // TODO: consider superpage
      } .elsewhen (memReqReady && !sfenceLatch) {
        state := state_wait_resp
      }
    }

    is (state_wait_resp) {
      when (memRespFire) {
        when (memPte.isLeaf() || memPte.isPf()) {
          when (resp(arbChosen).ready) {
            state := state_idle
          }.otherwise {
            state := state_wait_ready
            latch.entry := new TlbEntry().genTlbEntry(memRdata, level, req.vpn)
            latch.pf := memPte.isPf()
          }
        }.otherwise {
          level := levelNext
          when (level=/=2.U) {
            state := state_req
          } .otherwise {
            notFound := true.B
            when (resp(arbChosen).ready) {
              state := state_idle
            } .otherwise {
              state := state_wait_ready
            }
          }
        }
      }
    }

    is (state_wait_ready) {
      when (resp(arbChosen).ready) {
        state := state_idle
      }
    }
  }

  /*
   * mem
   */
  val memAddr =  Mux(level===0.U, l1addr/*when l1Hit, DontCare, when l1miss, l1addr*/,
                 Mux(level===1.U, Mux(l2Hit, l3addr, l2addr)/*when l2Hit, l3addr, when l2miss, l2addr*/, l3addr))
  val pteRead =  edge.Get(
    fromSource = 0.U/*id*/,
    // toAddress  = memAddr(log2Up(CacheLineSize / 2 / 8) - 1, 0),
    toAddress  = Cat(memAddr(PAddrBits - 1, log2Up(l1BusDataWidth/8)), 0.U(log2Up(l1BusDataWidth/8).W)),
    lgSize     = log2Up(l1BusDataWidth/8).U
  )._2
  mem.a.bits  := pteRead
  mem.a.valid := state === state_req && 
               ((level===0.U && !tlbHit && !l1Hit) ||
                (level===1.U && !l2Hit) ||
                (level===2.U)) && !sfenceLatch && !sfence.valid
  mem.d.ready := state === state_wait_resp || sfenceLatch

  val memAddrLatch = RegEnable(memAddr, mem.a.valid)
  memRdata := (mem.d.bits.data >> (memAddrLatch(log2Up(l1BusDataWidth/8) - 1, log2Up(XLEN/8)) << log2Up(XLEN)))(XLEN - 1, 0)

  /*
   * resp
   */
  val ptwFinish = (state===state_req && tlbHit && level===0.U) || ((memPte.isLeaf() || memPte.isPf() || (!memPte.isLeaf() && level===2.U)) && memRespFire && !sfenceLatch) || state===state_wait_ready
  for(i <- 0 until PtwWidth) {
    resp(i).valid := valid && arbChosen===i.U && ptwFinish // TODO: add resp valid logic
    resp(i).bits.entry := Mux(tlbHit, tlbHitData,
      Mux(state===state_wait_ready, latch.entry, new TlbEntry().genTlbEntry(memRdata, Mux(level===3.U, 2.U, level), req.vpn)))
    resp(i).bits.pf  := Mux(level===3.U || notFound, true.B, Mux(tlbHit, false.B, Mux(state===state_wait_ready, latch.pf, memPte.isPf())))
    // TODO: the pf must not be correct, check it
  }

  /*
   * refill
   */
  ptwl2.io.w.req <> DontCare
  tlbl2.io.w.req <> DontCare
  ptwl2.io.w.req.valid := false.B
  tlbl2.io.w.req.valid := false.B
  assert(!memRespFire || (state===state_wait_resp || sfenceLatch))
  when (memRespFire && !memPte.isPf() && !sfenceLatch) {
    when (level===0.U && !memPte.isLeaf) {
      val refillIdx = LFSR64()(log2Up(PtwL1EntrySize)-1,0) // TODO: may be LRU
      ptwl1(refillIdx).refill(l1addr, memRdata)
      l1v := l1v | UIntToOH(refillIdx)
    }
    when (level===1.U && !memPte.isLeaf) {
      val l2addrStore = RegEnable(l2addr, memReqFire && state===state_req && level===1.U)
      val refillIdx = getVpnn(req.vpn, 1)(log2Up(PtwL2EntrySize)-1, 0)
      
      assert(ptwl2.io.w.req.ready)
      // ptwl2.io.w.req.valid := true.B
      ptwl2.io.w.apply(valid = true.B, setIdx = refillIdx, data = new PtwEntry(tagLen2).genPtwEntry(l2addrStore, memRdata), waymask = -1.S.asUInt)
      // ptwl2.write(refillIdx, new PtwEntry(tagLen2).genPtwEntry(l2addrStore, memRdata))
      l2v := l2v | UIntToOH(refillIdx)
      l2g := l2g | Mux(memPte.perm.g, UIntToOH(refillIdx), 0.U)
    }
    when (memPte.isLeaf()) {
      val refillIdx = getVpnn(req.vpn, 0)(log2Up(TlbL2EntrySize)-1, 0)
      
      assert(tlbl2.io.w.req.ready)
      // tlbl2.io.w.req.valid := true.B
      tlbl2.io.w.apply(valid = true.B, setIdx = refillIdx, data = new TlbEntry().genTlbEntry(memRdata, level, req.vpn), waymask = -1.S.asUInt)
      // tlbl2.write(refillIdx, new TlbEntry().genTlbEntry(memRdata, level, req.vpn))
      tlbv := tlbv | UIntToOH(refillIdx)
      tlbg := tlbg | Mux(memPte.perm.g, UIntToOH(refillIdx), 0.U)
    }
  }

  /* sfence
   * for ram is syncReadMem, so could not flush conditionally
   * l3 may be conflict with l2tlb??, may be we could combine l2-tlb with l3-ptw
   */
  when (sfence.valid) { // TODO: flush optionally
    valid := false.B
    state := state_idle
    when (state===state_wait_resp && !memRespFire) {
      sfenceLatch := true.B // NOTE: every req need a resp
    }

    when (sfence.bits.rs1/*va*/) {
      when (sfence.bits.rs2) {
        // all va && all asid
        tlbv := 0.U
        tlbg := 0.U
        l1v  := 0.U
        l2v  := 0.U
        l2g  := 0.U
      } .otherwise {
        // all va && specific asid except global
        tlbv := tlbv & tlbg
        l1v  := l1v  & l1g
        l2v  := l2v  & l2g
      }
    } .otherwise {
      when (sfence.bits.rs2) {
        // specific leaf of addr && all asid
        tlbv := tlbv & ~UIntToOH(sfence.bits.addr(log2Up(TlbL2EntrySize)-1+offLen, 0+offLen))
        tlbg := tlbg & ~UIntToOH(sfence.bits.addr(log2Up(TlbL2EntrySize)-1+offLen, 0+offLen))
      } .otherwise {
        // specific leaf of addr && specific asid
        tlbv := tlbv & (~UIntToOH(sfence.bits.addr(log2Up(TlbL2EntrySize)-1+offLen, 0+offLen)) | tlbg)
      }
    }
  }

  if (!env.FPGAPlatform) {
    ExcitingUtils.addSource(validOneCycle, "perfCntPtwReqCnt", Perf)
    ExcitingUtils.addSource(valid, "perfCntPtwCycleCnt", Perf)
    ExcitingUtils.addSource(valid && tlbHit && state===state_req && level===0.U, "perfCntPtwL2TlbHit", Perf)
  }

  assert(level=/=3.U)

  def PrintFlag(en: Bool, flag: Bool, nameEnable: String, nameDisable: String): Unit = {
    when(flag) {
      XSDebug(false, en, nameEnable)
    }.otherwise {
      XSDebug(false, en, nameDisable)
    }
  }

  XSDebug(validOneCycle, "**New Ptw Req from ")
  PrintFlag(validOneCycle, arbChosen===0.U, "DTLB**:", "ITLB**:")
  XSDebug(false, validOneCycle, p"(v:${validOneCycle} r:${arb.io.out.ready}) vpn:0x${Hexadecimal(req.vpn)}\n")
  XSDebug(resp(arbChosen).fire(), "**Ptw Resp to ")
  PrintFlag(resp(arbChosen).fire(), arbChosen===0.U, "DTLB**:\n", "ITLB**\n")
  XSDebug(resp(arbChosen).fire(), p"(v:${resp(arbChosen).valid} r:${resp(arbChosen).ready}) entry:${resp(arbChosen).bits.entry} pf:${resp(arbChosen).bits.pf}\n")

  XSDebug(sfence.valid, p"Sfence: sfence instr here ${sfence.bits}\n")
  XSDebug(valid, p"CSR: ${csr}\n")

  XSDebug(valid, p"vpn2:0x${Hexadecimal(getVpnn(req.vpn, 2))} vpn1:0x${Hexadecimal(getVpnn(req.vpn, 1))} vpn0:0x${Hexadecimal(getVpnn(req.vpn, 0))}\n")
  XSDebug(valid, p"state:${state} level:${level} tlbHit:${tlbHit} l1addr:0x${Hexadecimal(l1addr)} l1Hit:${l1Hit} l2addr:0x${Hexadecimal(l2addr)} l2Hit:${l2Hit}  l3addr:0x${Hexadecimal(l3addr)} memReq(v:${mem.a.valid} r:${mem.a.ready})\n")

  XSDebug(memReqFire, p"mem req fire addr:0x${Hexadecimal(memAddr)}\n")
  XSDebug(memRespFire, p"mem resp fire rdata:0x${Hexadecimal(mem.d.bits.data)} Pte:${memPte}\n")

  XSDebug(sfenceLatch, p"ptw has a flushed req waiting for resp... state:${state} mem.a(${mem.a.valid} ${mem.a.ready}) d($memValid} ${memRespReady})\n")

  // TODO: add ptw perf cnt
}
