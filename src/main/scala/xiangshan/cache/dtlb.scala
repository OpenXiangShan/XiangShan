package xiangshan.cache

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap
import xiangshan.mem._
import bus.simplebus._
import xiangshan.backend.fu.HasCSRConst
import chisel3.ExcitingUtils._

trait HasTlbConst extends HasXSParameter {
  val Level = 3

  val offLen  = 12
  val ppnLen  = PAddrBits - offLen
  val vpnnLen = 9
  val vpnLen  = VAddrBits - offLen
  val flagLen = 8
  val pteResLen = XLEN - ppnLen - 2 - flagLen
  val asidLen = 16

  def vaBundle = new Bundle {
    val vpn  = UInt(vpnLen.W)
    val off  = UInt(offLen.W)
  }
  def pteBundle = new Bundle {
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
  }
}

abstract class TlbBundle extends XSBundle with HasTlbConst
abstract class TlbModule extends XSModule with HasTlbConst

class PermBundle(val hasV: Boolean = true) extends TlbBundle {
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()
  if (hasV) { val v = Bool() }

  override def toPrintable: Printable = {
    p"d:${d} a:${a} g:${g} u:${u} x:${x} w:${w} r:${r}"// + 
    //(if(hasV) (p"v:${v}") else p"")
  }
}

class TlbEntry extends TlbBundle {
  val vpn = UInt(vpnLen.W) // tag is vpn
  val ppn = UInt(ppnLen.W)
  val level = UInt(log2Up(Level).W) // 0 for 4KB, 1 for 2MB, 2 for 1GB
  // val asid = UInt(asidLen.W), asid maybe expensive to support, but useless
  // val v = Bool() // v&g is special, may need sperate storage?
  val perm = new PermBundle(hasV = false)

  def vpnHit(vpn: UInt):Bool = {
    val fullMask = VecInit((Seq.fill(vpnLen)(true.B))).asUInt
    val maskLevel = VecInit((Level-1 to 0 by -1).map{i => // NOTE: level 2 for 4KB, 1 for 2MB, 0 for 1GB
      VecInit(Seq.fill(vpnLen-i*vpnnLen)(true.B) ++ Seq.fill(i*vpnnLen)(false.B)).asUInt})
    val mask = maskLevel(level)
    (mask&this.vpn) === (mask&vpn)
  }

  // def asidHit(asid: UInt) = {
  //   this.asid === asid
  // }

  def hit(vpn: UInt/*, asid: UInt*/):Bool = {
    vpnHit(vpn)// && asidHit(asid)
  }

  def genTlbEntry(pte: UInt, level: UInt, vpn: UInt/*, asid: UInt*/) = {
    val e = Wire(new TlbEntry)
    e.ppn := pte.asTypeOf(pteBundle).ppn
    e.level := level
    e.vpn := vpn
    e.perm := pte.asTypeOf(pteBundle).perm
    // e.asid := asid
    e
  }

  override def toPrintable: Printable = {
    p"vpn:0x${Hexadecimal(vpn)} ppn:0x${Hexadecimal(ppn)} level:${level} perm:${perm}"
  }
}

object TlbCmd {
  def read  = "b00".U
  def write = "b01".U
  def exec  = "b10".U

  def apply() = UInt(2.W)
  def isRead(a: UInt) = a===read
  def isWrite(a: UInt) = a===write
  def isExec(a: UInt) = a===exec
}

class TlbReq extends TlbBundle {
  val vaddr = UInt(VAddrBits.W)
  val cmd = TlbCmd()
  val roqIdx = UInt(RoqIdxWidth.W)
  val debug = new Bundle {
    val pc = UInt(XLEN.W)
    val lsroqIdx = UInt(LsroqIdxWidth.W)
  }

  override def toPrintable: Printable = {
    p"vaddr:0x${Hexadecimal(vaddr)} cmd:${cmd} pc:0x${Hexadecimal(debug.pc)} roqIdx:${roqIdx} lsroqIdx:${debug.lsroqIdx}"
  }
}

class TlbResp extends TlbBundle {
  val paddr = UInt(PAddrBits.W)
  val miss = Bool()
  val excp = new Bundle {
    val pf = new Bundle {
      val ld = Bool()
      val st = Bool()
      val instr = Bool()
    }
  }
  override def toPrintable: Printable = {
    p"paddr:0x${Hexadecimal(paddr)} miss:${miss} excp.pf: ld:${excp.pf.ld} st:${excp.pf.st} instr:${excp.pf.instr}"
  }
}

class TlbRequestIO() extends TlbBundle {
  val req = Valid(new TlbReq)
  val resp = Flipped(Valid(new TlbResp))

  // override def cloneType: this.type = (new TlbRequestIO(Width)).asInstanceOf[this.type]
}

class TlbPtwIO extends TlbBundle {
  val req = DecoupledIO(new PtwReq)
  val resp = Flipped(DecoupledIO(new PtwResp))
}

class TlbIO(Width: Int) extends TlbBundle {
  val requestor = Vec(Width, Flipped(new TlbRequestIO))
  val ptw = new TlbPtwIO

  override def cloneType: this.type = (new TlbIO(Width)).asInstanceOf[this.type]
}


class TLB(Width: Int, isDtlb: Boolean) extends TlbModule with HasCSRConst{
  val io = IO(new TlbIO(Width))

  val req    = io.requestor.map(_.req)
  val resp   = io.requestor.map(_.resp)
  val ptw    = io.ptw

  val sfence = WireInit(0.U.asTypeOf(new SfenceBundle))
  val csr    = WireInit(0.U.asTypeOf(new TlbCsrBundle))
  val satp   = csr.satp
  val priv   = csr.priv
  val ifecth = if (isDtlb) false.B else true.B
  val mode   = if (isDtlb) priv.dmode else priv.imode
  val vmEnable = satp.mode === 8.U // && (mode < ModeM) // FIXME: fix me when boot xv6/linux...
  BoringUtils.addSink(sfence, "SfenceBundle")
  BoringUtils.addSink(csr, "TLBCSRIO")

  val reqAddr = req.map(_.bits.vaddr.asTypeOf(vaBundle))
  val cmd     = req.map(_.bits.cmd)
  val valid   = req.map(_.valid)

  def widthMapSeq[T <: Seq[Data]](f: Int => T) = (0 until Width).map(f)
  def widthMap[T <: Data](f: Int => T) = (0 until Width).map(f)

  val v = RegInit(0.U(TlbEntrySize.W))
  val pf = RegInit(0.U(TlbEntrySize.W)) // TODO: when ptw resp a pf(now only page not found), store here
  val entry = Reg(Vec(TlbEntrySize, new TlbEntry))
  val g = Cat(entry.map(_.perm.g)) // TODO: need check if reverse is needed

  val entryHitVec = widthMapSeq{i => VecInit(entry.map(_.hit(reqAddr(i).vpn/*, satp.asid*/))) }
  val hitVec  = widthMapSeq{ i => (v.asBools zip entryHitVec(i)).map{ case (a,b) => a&b } }
  val pfHitVec   = widthMapSeq{ i => (pf.asBools zip entryHitVec(i)).map{ case (a,b) => a&b } }
  val pfArray = widthMap{ i => ParallelOR(pfHitVec(i)).asBool && valid(i) && vmEnable }
  val hit     = widthMap{ i => ParallelOR(hitVec(i)).asBool && valid(i) && vmEnable && ~pfArray(i) }
  val miss    = widthMap{ i => !hit(i) && valid(i) && vmEnable && ~pfArray(i) }
  val hitppn  = widthMap{ i => ParallelMux(hitVec(i) zip entry.map(_.ppn)) }
  val hitPerm = widthMap{ i => ParallelMux(hitVec(i) zip entry.map(_.perm)) }
  val multiHit = {
    val hitSum = widthMap{ i => PopCount(hitVec(i)) }
    val pfHitSum = widthMap{ i => PopCount(pfHitVec(i)) }
    ParallelOR(widthMap{ i => !(hitSum(i)===0.U || hitSum(i)===1.U) || !(pfHitSum(i)===0.U || pfHitSum(i)===1.U)})
  }

  // resp  // TODO: A/D has not being concerned
  for(i <- 0 until Width) {
    resp(i).valid := valid(i)
    resp(i).bits.paddr := Mux(vmEnable, Cat(hitppn(i), reqAddr(i).off), SignExt(req(i).bits.vaddr, PAddrBits))
    resp(i).bits.miss := miss(i)

    val perm = hitPerm(i) // NOTE: given the excp, the out module choose one to use?
    val update = false.B && hit(i) && (!hitPerm(i).a || !hitPerm(i).d && TlbCmd.isWrite(cmd(i))) // update A/D through exception
    val modeCheck = !(mode === ModeU && !perm.u || mode === ModeS && perm.u && (!priv.sum || ifecth))
    val ldPf = (pfArray(i) && TlbCmd.isRead(cmd(i)) && true.B /*!isAMO*/) || hit(i) && !(modeCheck && (perm.r || priv.mxr && perm.x)) && (TlbCmd.isRead(cmd(i)) && true.B/*!isAMO*/) // TODO: handle isAMO
    val stPf = (pfArray(i) && TlbCmd.isWrite(cmd(i)) || false.B /*isAMO*/ ) || hit(i) && !(modeCheck && perm.w) && (TlbCmd.isWrite(cmd(i)) || false.B/*TODO isAMO. */)
    val instrPf = (pfArray(i) && TlbCmd.isExec(cmd(i))) || hit(i) && !(modeCheck && perm.x) && TlbCmd.isExec(cmd(i))
    resp(i).bits.excp.pf.ld    := ldPf || update
    resp(i).bits.excp.pf.st    := stPf || update
    resp(i).bits.excp.pf.instr := instrPf || update
  }

  // ptw
  val state_idle :: state_wait :: Nil = Enum(2)
  val state = RegInit(state_idle)

  ptw <> DontCare // TODO: need check it
  ptw.req.valid := ParallelOR(miss).asBool && state===state_idle
  ptw.resp.ready := state===state_wait
  class comBundle extends TlbBundle with HasRoqIdx{
    val valid = Bool()
    val bits = new PtwReq

    def isPrior(that: comBundle): Bool = {
      (this.valid && !that.valid) || (this.valid && that.valid && (that isAfter this))
    }
  }
  object Compare {
    def apply[T<:Data](xs: Seq[comBundle]): comBundle = {
      ParallelOperation(xs, (a: comBundle, b: comBundle) => Mux(a isPrior b, a, b))
    }
  }

  // val ptwReqSeq = Wire(Seq.fill(Width)(new comBundle()))
  val ptwReqSeq = Seq.fill(Width)(Wire(new comBundle()))
  for (i <- 0 until Width) {
    ptwReqSeq(i).valid := valid(i) && miss(i)
    ptwReqSeq(i).roqIdx := req(i).bits.roqIdx
    ptwReqSeq(i).bits.vpn := reqAddr(i).vpn
  }
  ptw.req.bits := Compare(ptwReqSeq).bits

  switch (state) {
    is (state_idle) {
      when (ParallelOR(miss).asBool) {
        state := state_wait
      }
      assert(!ptw.resp.valid)
    }

    is (state_wait) {
      when (ptw.resp.fire()) {
        state := state_idle
      }
    }
  }

  // reset pf when pf hit
  val pfHitReset = WireInit(0.U(TlbEntrySize.W))
  when (ParallelOR(pfArray).asBool /* or ParallelOR(valid)*/) {
    val pfHitAndValid = widthMap{i => Mux(valid(i), VecInit(pfHitVec(i)).asUInt, 0.U) }
    pfHitReset := ParallelOR(pfHitAndValid)
  }

  // refill
  val refill = ptw.resp.fire()
  val randIdx = LFSR64()(log2Up(TlbEntrySize)-1,0)
  val priorIdx = PriorityEncoder(~v)
  val refillIdx = Mux(ParallelAND((v|pf).asBools), randIdx, priorIdx)
  val pfRefill = WireInit(0.U(TlbEntrySize.W))
  when (refill) {
    v := Mux(ptw.resp.bits.pf, v & ~UIntToOH(refillIdx), v | UIntToOH(refillIdx))
    pfRefill := Mux(ptw.resp.bits.pf, UIntToOH(refillIdx), 0.U)
    entry(refillIdx) := ptw.resp.bits.entry
    XSDebug(p"Refill: idx:${refillIdx} entry:${ptw.resp.bits.entry}\n")
  }

  // pf update
  when (refill || ParallelOR(pfArray).asBool /* or ParallelOR(valid)*/) {
    pf := (pf & ~pfHitReset) | pfRefill
  }

  // sfence (flush)
  when (sfence.valid) {
    when (sfence.bits.rs1) { // virtual address *.rs1 <- (rs1===0.U)
      when (sfence.bits.rs2) { // asid, but i do not want to support asid, *.rs2 <- (rs2===0.U)
        // all addr and all asid
        v := 0.U
        pf := 0.U
      }.otherwise {
        // all addr but specific asid
        v := v & g // TODO: need check if reverse is needed
        pf := pf & g
      }
    }.otherwise {
      when (sfence.bits.rs2) {
        // specific addr but all asid
        v := v & ~VecInit(entry.map(_.hit(sfence.bits.addr.asTypeOf(vaBundle).vpn))).asUInt
        pf := pf & ~VecInit(entry.map(_.hit(sfence.bits.addr.asTypeOf(vaBundle).vpn))).asUInt
      }.otherwise {
        // specific addr and specific asid
        v := v & ~VecInit(entry.map(e => e.hit(sfence.bits.addr.asTypeOf(vaBundle).vpn) && (/*e.asid === sfence.bits.asid && */!e.perm.g))).asUInt
        pf := pf & ~VecInit(entry.map(e => e.hit(sfence.bits.addr.asTypeOf(vaBundle).vpn) && (/*e.asid === sfence.bits.asid && */!e.perm.g))).asUInt
      }
    }
  }

  if (!env.FPGAPlatform) {
    ExcitingUtils.addSource(valid(0)/* && vmEnable*/, "perfCntDtlbReqCnt0", Perf)
    ExcitingUtils.addSource(valid(1)/* && vmEnable*/, "perfCntDtlbReqCnt1", Perf)
    ExcitingUtils.addSource(valid(2)/* && vmEnable*/, "perfCntDtlbReqCnt2", Perf)
    ExcitingUtils.addSource(valid(3)/* && vmEnable*/, "perfCntDtlbReqCnt3", Perf)
    ExcitingUtils.addSource(valid(0)/* && vmEnable*/ && miss(0), "perfCntDtlbMissCnt0", Perf)
    ExcitingUtils.addSource(valid(1)/* && vmEnable*/ && miss(1), "perfCntDtlbMissCnt1", Perf)
    ExcitingUtils.addSource(valid(2)/* && vmEnable*/ && miss(2), "perfCntDtlbMissCnt2", Perf)
    ExcitingUtils.addSource(valid(3)/* && vmEnable*/ && miss(3), "perfCntDtlbMissCnt3", Perf)
  }

  // Log
  for(i <- 0 until Width) {
    XSDebug(req(i).valid, p"req(${i.U}): ${req(i).bits}\n")
    XSDebug(resp(i).valid, p"resp(${i.U}): ${resp(i).bits}\n")
  }

  XSDebug(sfence.valid, p"Sfence: ${sfence}\n")
  XSDebug(ParallelOR(valid)|| ptw.resp.valid, p"CSR: ${csr}\n")
  XSDebug(ParallelOR(valid) || ptw.resp.valid, p"vmEnable:${vmEnable} hit:${Binary(VecInit(hit).asUInt)} miss:${Binary(VecInit(miss).asUInt)} v:${Hexadecimal(v)} pf:${Hexadecimal(pf)} state:${state}\n")
  XSDebug(ptw.req.fire(), p"PTW req:${ptw.req.bits}\n")
  XSDebug(ptw.resp.valid, p"PTW resp:${ptw.resp.bits} (v:${ptw.resp.valid}r:${ptw.resp.ready}) \n")

  // assert check, can be remove when tlb can work
  for(i <- 0 until Width) {
    assert((hit(i)&pfArray(i))===false.B, "hit(%d):%d pfArray(%d):%d v:0x%x pf:0x%x", i.U, hit(i), i.U, pfArray(i), v, pf)
  }
  for(i <- 0 until Width) {
    XSDebug(multiHit, p"vpn:0x${Hexadecimal(reqAddr(i).vpn)} hitVec:0x${Hexadecimal(VecInit(hitVec(i)).asUInt)} pfHitVec:0x${Hexadecimal(VecInit(pfHitVec(i)).asUInt)}\n")
  }
  for(i <- 0 until TlbEntrySize) {
    XSDebug(multiHit, p"entry(${i.U}): v:${v(i)} ${entry(i)}\n")
  }
  assert(!multiHit) // add multiHit here, later it should be removed (maybe), turn to miss and flush

  for (i <- 0 until Width) {
    XSDebug(resp(i).valid && hit(i) && !(req(i).bits.vaddr===resp(i).bits.paddr), p"vaddr:0x${Hexadecimal(req(i).bits.vaddr)} paddr:0x${Hexadecimal(resp(i).bits.paddr)} hitVec:0x${Hexadecimal(VecInit(hitVec(i)).asUInt)}}\n")
    when (resp(i).valid && hit(i) && !(req(i).bits.vaddr===resp(i).bits.paddr)) {
      for (j <- 0 until TlbEntrySize) {
        XSDebug(true.B, p"TLBEntry(${j.U}): v:${v(j)} ${entry(j)}\n")
      }
    } // FIXME: remove me when tlb may be ok
    when(resp(i).valid && hit(i)) {
      assert(req(i).bits.vaddr===resp(i).bits.paddr, "vaddr:0x%x paddr:0x%x hitVec:%x ", req(i).bits.vaddr, resp(i).bits.paddr, VecInit(hitVec(i)).asUInt)
    } // FIXME: remove me when tlb may be ok
  }
  
  assert((v&pf)===0.U, "v and pf can't be true at same time: v:0x%x pf:0x%x", v, pf)
}
