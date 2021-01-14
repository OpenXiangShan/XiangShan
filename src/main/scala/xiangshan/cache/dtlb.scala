package xiangshan.cache

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.roq.RoqPtr
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

class comBundle extends TlbBundle with HasCircularQueuePtrHelper{
  val roqIdx = new RoqPtr
  val valid = Bool()
  val bits = new PtwReq
  def isPrior(that: comBundle): Bool = {
    (this.valid && !that.valid) || (this.valid && that.valid && isAfter(that.roqIdx, this.roqIdx))
  }
}
object Compare {
  def apply[T<:Data](xs: Seq[comBundle]): comBundle = {
    ParallelOperation(xs, (a: comBundle, b: comBundle) => Mux(a isPrior b, a, b))
  }
}

// multi-read && single-write
// input is data, output is hot-code(not one-hot)
class CAMTemplate[T <: Data](val gen: T, val set: Int, val readWidth: Int) extends TlbModule {
  val io = IO(new Bundle {
    val r = new Bundle {
      val req = Input(Vec(readWidth, gen))
      val resp = Output(Vec(readWidth, UInt(set.W)))
    }
    val w = Flipped(ValidIO(new Bundle {
      val index = UInt(log2Up(set).W)
      val data = gen
    }))
  })

  val wordType = UInt(gen.getWidth.W)
  val array = Reg(Vec(set, wordType))

  io.r.resp.zipWithIndex.map{ case (a,i) =>
    a := VecInit(array.map(io.r.req(i).asUInt === _)).asUInt
  }

  when (io.w.valid) {
    array(io.w.bits.index) := io.w.bits.data
  }
}

class TlbEntryData extends TlbBundle {
  val ppn = UInt(ppnLen.W)
  val perm = new PermBundle(hasV = false)
  // TODO: change perm to every kinds of pf check

  override def toPrintable: Printable = {
    p"ppn:0x${Hexadecimal(ppn)} perm:${perm}"
  }
}

class TlbEntry(superpage: Boolean = false, superpageOnly: Boolean = false) extends TlbBundle {
  val tag = UInt(vpnLen.W) // tag is vpn
  val level = UInt(log2Up(Level).W) // 2 for 4KB, 1 for 2MB, 0 for 1GB
  val data = new TlbEntryData

  def vpnHit(vpn: UInt):Bool = {
    if (superpage) {
      val fullMask = VecInit((Seq.fill(vpnLen)(true.B))).asUInt
      val maskLevel = VecInit((Level-1 to 0 by -1).map{i => // NOTE: level 2 for 4KB, 1 for 2MB, 0 for 1GB
        Reverse(VecInit(Seq.fill(vpnLen-i*vpnnLen)(true.B) ++ Seq.fill(i*vpnnLen)(false.B)).asUInt)})
      val mask = maskLevel(level)
      (mask&this.tag) === (mask&vpn)
    } else {
      tag === vpn
    }

  }

  def hit(vpn: UInt):Bool = {
    vpnHit(vpn)
  }

  def genTlbEntry(superPage: Boolean = false, superpageOnly: Boolean = false, pte: UInt, level: UInt, vpn: UInt) = {
    val e = Wire(new TlbEntry(superPage, superpageOnly))
    e.tag := vpn
    e.level := level
    e.data.ppn := pte.asTypeOf(pteBundle).ppn
    e.data.perm := pte.asTypeOf(pteBundle).perm
    e
  }

  override def toPrintable: Printable = {
    p"vpn:0x${Hexadecimal(tag)} level:${level} data:${data}"
  }

  override def cloneType: this.type = (new TlbEntry(superpage, superpageOnly)).asInstanceOf[this.type]
}

class TlbEntries(num: Int, tagLen: Int) extends TlbBundle {
  require(log2Up(num)==log2Down(num))
  /* vpn can be divide into three part */
  // vpn: tagPart(17bit) + addrPart(8bit) + cutLenPart(2bit)
  val cutLen  = log2Up(num)

  val tag     = UInt(tagLen.W) // NOTE: high part of vpn
  val ppns    = Vec(num, UInt(ppnLen.W))
  val perms    = Vec(num, new PermBundle(hasV = false))
  val vs      = Vec(num, Bool())

  def tagClip(vpn: UInt) = { // full vpn => tagLen
    vpn(vpn.getWidth-1, tagLen)
  }

  // NOTE: get insize idx
  def idxClip(vpn: UInt) = {
    vpn(cutLen-1, 0)
  }

  def hit(vpn: UInt) = {
    (tag === tagClip(vpn)) && vs(idxClip(vpn))
  }

  def genEntries(data: UInt, level: UInt, vpn: UInt): TlbEntries = {
    require((data.getWidth / XLEN) == num,
      "input data length must be multiple of pte length")
    assert(level===2.U, "tlb entries only support 4K pages")

    val ts = Wire(new TlbEntries(num, tagLen))
    ts.tag := tagClip(vpn)
    for (i <- 0 until num) {
      val pte = data((i+1)*XLEN-1, i*XLEN).asTypeOf(new PteBundle)
      ts.ppns(i) := pte.ppn
      ts.perms(i):= pte.perm // this.perms has no v
      ts.vs(i)   := !pte.isPf(level) && pte.isLeaf() // legal and leaf, store to l2Tlb
    }

    ts
  }

  def get(vpn: UInt): TlbEntry = {
    val t = Wire(new TlbEntry(false, false))
    val idx = idxClip(vpn)
    t.tag := vpn // Note: Use input vpn, not vpn in TlbL2
    t.level := 2.U
    t.data.ppn := ppns(idx)
    t.data.perm := perms(idx)
    t
  }

  override def cloneType: this.type = (new TlbEntries(num, tagLen)).asInstanceOf[this.type]
  override def toPrintable: Printable = {
    require(num == 4, "if num is not 4, please comment this toPrintable")
    // NOTE: if num is not 4, please comment this toPrintable
    p"tag:${Hexadecimal(tag)} ppn(0):${Hexadecimal(ppns(0))} ppn(1):${Hexadecimal(ppns(1))}" +
    p"ppn(2):${Hexadecimal(ppns(2))} ppn(3):${Hexadecimal(ppns(3))} " +
    p"perms(0):${perms(0)} perms(1):${perms(1)} perms(2):${perms(2)} perms(3):${perms(3)} vs:${Binary(vs.asUInt)}"
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
  val roqIdx = new RoqPtr
  val debug = new Bundle {
    val pc = UInt(XLEN.W)
  }

  override def toPrintable: Printable = {
    p"vaddr:0x${Hexadecimal(vaddr)} cmd:${cmd} pc:0x${Hexadecimal(debug.pc)} roqIdx:${roqIdx}"
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
  val req = DecoupledIO(new TlbReq)
  val resp = Flipped(DecoupledIO(new TlbResp))
}

class BlockTlbRequestIO() extends TlbBundle {
  val req = DecoupledIO(new TlbReq)
  val resp = Flipped(DecoupledIO(new TlbResp))
}

class TlbPtwIO extends TlbBundle {
  val req = DecoupledIO(new PtwReq)
  val resp = Flipped(DecoupledIO(new PtwResp))
}

class TlbIO(Width: Int) extends TlbBundle {
  val requestor = Vec(Width, Flipped(new TlbRequestIO))
  val ptw = new TlbPtwIO
  val sfence = Input(new SfenceBundle)
  val csr = Input(new TlbCsrBundle)

  override def cloneType: this.type = (new TlbIO(Width)).asInstanceOf[this.type]
}


class TLB(Width: Int, isDtlb: Boolean) extends TlbModule with HasCSRConst{
  val io = IO(new TlbIO(Width))

  val req    = io.requestor.map(_.req)
  val resp   = io.requestor.map(_.resp)
  val ptw    = io.ptw

  val sfence = io.sfence
  val csr    = io.csr
  val satp   = csr.satp
  val priv   = csr.priv
  val ifecth = if (isDtlb) false.B else true.B
  val mode   = if (isDtlb) priv.dmode else priv.imode
  // val vmEnable = satp.mode === 8.U // && (mode < ModeM) // FIXME: fix me when boot xv6/linux...
  val vmEnable = satp.mode === 8.U && (mode < ModeM)

  val reqAddr = req.map(_.bits.vaddr.asTypeOf(vaBundle))
  val cmd     = req.map(_.bits.cmd)
  val valid   = req.map(_.valid)

  def widthMapSeq[T <: Seq[Data]](f: Int => T) = (0 until Width).map(f)
  def widthMap[T <: Data](f: Int => T) = (0 until Width).map(f)

  // normal page: 4k
  val v = RegInit(0.U(TlbEntrySize.W))
  val pf = RegInit(0.U(TlbEntrySize.W))
  val entry = Reg(Vec(TlbEntrySize, new TlbEntry))
  val g = VecInit(entry.map(_.data.perm.g)).asUInt

  /**
    * PTW refill
    */
  val refill = ptw.resp.fire()
  val randIdx = LFSR64()(log2Up(TlbEntrySize)-1,0)
  val priorIdx = PriorityEncoder(~(v|pf))
  val tlbfull = ParallelAND((v|pf).asBools)
  val refillIdx = Mux(tlbfull, randIdx, priorIdx)
  val refillIdxOH = UIntToOH(refillIdx)
  when (refill) {
    v := Mux(ptw.resp.bits.pf, v & ~refillIdxOH, v | refillIdxOH)
    entry(refillIdx) := ptw.resp.bits.entry
    XSDebug(p"Refill: idx:${refillIdx} entry:${ptw.resp.bits.entry}\n")
  }

  /**
    * L1 TLB read
    */
  val tlb_read_mask = Mux(refill, refillIdxOH, 0.U(TlbEntrySize.W))
  def TLBNormalRead(i: Int) = {
    val entryHitVec = (
      if (isDtlb)
        VecInit((tlb_read_mask.asBools zip entry).map{ case (r, e) => !r && e.hit(reqAddr(i).vpn/*, satp.asid*/)})
      else
        VecInit(entry.map(_.hit(reqAddr(i).vpn/*, satp.asid*/)))
    )

    val reqAddrReg = if (isDtlb) RegNext(reqAddr(i)) else reqAddr(i)
    val cmdReg = if (isDtlb) RegNext(cmd(i)) else cmd(i)
    val validReg = if (isDtlb) RegNext(valid(i)) else valid(i)
    val entryHitVecReg = if (isDtlb) RegNext(entryHitVec) else entryHitVec

    val hitVec  = (v.asBools zip entryHitVecReg).map{ case (a,b) => a&b }
    val pfHitVec   = (pf.asBools zip entryHitVecReg).map{ case (a,b) => a&b }
    val pfArray = ParallelOR(pfHitVec).asBool && validReg && vmEnable
    val hit     = ParallelOR(hitVec).asBool && validReg && vmEnable && ~pfArray
    val miss    = !hit && validReg && vmEnable && ~pfArray
    val hitppn  = ParallelMux(hitVec zip entry.map(_.data.ppn))
    val hitPerm = ParallelMux(hitVec zip entry.map(_.data.perm))
    val hitLevel= ParallelMux(hitVec zip entry.map(_.level))
    val multiHit = {
      val hitSum = PopCount(hitVec)
      val pfHitSum = PopCount(pfHitVec)
      !(hitSum===0.U || hitSum===1.U) || !(pfHitSum===0.U || pfHitSum===1.U)
    }

    // resp  // TODO: A/D has not being concerned
    val paddr = LookupTreeDefault(hitLevel, Cat(hitppn, reqAddrReg.off), List(
      0.U -> Cat(hitppn(ppnLen - 1, 2*vpnnLen), reqAddrReg.vpn(2*vpnnLen - 1, 0), reqAddrReg.off),
      1.U -> Cat(hitppn(ppnLen - 1, vpnnLen), reqAddrReg.vpn(vpnnLen - 1, 0), reqAddrReg.off),
      2.U -> Cat(hitppn, reqAddrReg.off)
    ))
    val vaddr = SignExt(req(i).bits.vaddr, PAddrBits)

    req(i).ready := resp(i).ready
    resp(i).valid := validReg
    resp(i).bits.paddr := Mux(vmEnable, paddr, if (isDtlb) RegNext(vaddr) else vaddr)
    resp(i).bits.miss := miss

    val perm = hitPerm // NOTE: given the excp, the out module choose one to use?
    val update = false.B && hit && (!hitPerm.a || !hitPerm.d && TlbCmd.isWrite(cmdReg)) // update A/D through exception
    val modeCheck = !(mode === ModeU && !perm.u || mode === ModeS && perm.u && (!priv.sum || ifecth))
    val ldPf = (pfArray && TlbCmd.isRead(cmdReg) && true.B /*!isAMO*/) || hit && !(modeCheck && (perm.r || priv.mxr && perm.x)) && (TlbCmd.isRead(cmdReg) && true.B/*!isAMO*/) // TODO: handle isAMO
    val stPf = (pfArray && TlbCmd.isWrite(cmdReg) || false.B /*isAMO*/ ) || hit && !(modeCheck && perm.w) && (TlbCmd.isWrite(cmdReg) || false.B/*TODO isAMO. */)
    val instrPf = (pfArray && TlbCmd.isExec(cmdReg)) || hit && !(modeCheck && perm.x) && TlbCmd.isExec(cmdReg)
    resp(i).bits.excp.pf.ld    := ldPf || update
    resp(i).bits.excp.pf.st    := stPf || update
    resp(i).bits.excp.pf.instr := instrPf || update

    (hit, miss, pfHitVec, multiHit)
  }

  val readResult = (0 until Width).map(TLBNormalRead(_))
  val hitVec = readResult.map(res => res._1)
  val missVec = readResult.map(res => res._2)
  val pfHitVecVec = readResult.map(res => res._3)
  val multiHitVec = readResult.map(res => res._4)
  val hasMissReq = Cat(missVec).orR

  // ptw
  ptw <> DontCare // TODO: need check it
  ptw.req.valid := hasMissReq && !sfence.valid
  ptw.resp.ready := true.B

  // val ptwReqSeq = Wire(Seq.fill(Width)(new comBundle()))
  val ptwReqSeq = Seq.fill(Width)(Wire(new comBundle()))
  for (i <- 0 until Width) {
    ptwReqSeq(i).valid := ((if (isDtlb) RegNext(valid(i)) else valid(i)) && missVec(i))
    ptwReqSeq(i).roqIdx := (if (isDtlb) RegNext(req(i).bits.roqIdx) else req(i).bits.roqIdx)
    ptwReqSeq(i).bits.vpn := (if (isDtlb) RegNext(reqAddr(i).vpn) else reqAddr(i).vpn)
  }
  ptw.req.bits := Compare(ptwReqSeq).bits

  // reset pf when pf hit
  val pfHitReset = ParallelOR(widthMap{i => Mux(resp(i).fire(), VecInit(pfHitVecVec(i)).asUInt, 0.U) })
  val pfHitRefill = false.B//ParallelOR(pfHitReset.asBools)

  // pf update
  when (refill) {
    when (pfHitRefill) {
      pf := Mux(ptw.resp.bits.pf, pf | refillIdxOH, pf & ~refillIdxOH) & ~pfHitReset
    } .otherwise {
      pf := Mux(ptw.resp.bits.pf, pf | refillIdxOH, pf & ~refillIdxOH)
    }
  } .otherwise {
    when (pfHitRefill) {
      pf := pf & ~pfHitReset
    }
  }
  when (PopCount(pf) > 10.U) { // when too much pf, just clear
    pf := Mux(refill && ptw.resp.bits.pf, refillIdxOH, 0.U)
  }

  // sfence (flush)
  when (sfence.valid) {
    ptw.req.valid := false.B
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
      val sfenceVpn = sfence.bits.addr.asTypeOf(vaBundle).vpn
      when (sfence.bits.rs2) {
        // specific addr but all asid
        v := v & ~VecInit(entry.map(_.hit(sfenceVpn))).asUInt
        pf := pf & ~VecInit(entry.map(_.hit(sfenceVpn))).asUInt
      }.otherwise {
        // specific addr and specific asid
        v := v & ~VecInit(entry.map(e => e.hit(sfenceVpn) && (/*e.asid === sfence.bits.asid && */!e.data.perm.g))).asUInt
        pf := pf & ~VecInit(entry.map(e => e.hit(sfenceVpn) && (/*e.asid === sfence.bits.asid && */!e.data.perm.g))).asUInt
      }
    }
  }

  if (!env.FPGAPlatform && isDtlb) {
    ExcitingUtils.addSource(valid(0) && vmEnable, "perfCntDtlbReqCnt0", Perf)
    ExcitingUtils.addSource(valid(1) && vmEnable, "perfCntDtlbReqCnt1", Perf)
    ExcitingUtils.addSource(valid(2) && vmEnable, "perfCntDtlbReqCnt2", Perf)
    ExcitingUtils.addSource(valid(3) && vmEnable, "perfCntDtlbReqCnt3", Perf)
    ExcitingUtils.addSource(valid(0) && vmEnable && missVec(0), "perfCntDtlbMissCnt0", Perf)
    ExcitingUtils.addSource(valid(1) && vmEnable && missVec(1), "perfCntDtlbMissCnt1", Perf)
    ExcitingUtils.addSource(valid(2) && vmEnable && missVec(2), "perfCntDtlbMissCnt2", Perf)
    ExcitingUtils.addSource(valid(3) && vmEnable && missVec(3), "perfCntDtlbMissCnt3", Perf)
  }

  if (!env.FPGAPlatform && !isDtlb) {
    ExcitingUtils.addSource(valid(0) && vmEnable, "perfCntItlbReqCnt0", Perf)
    ExcitingUtils.addSource(valid(0) && vmEnable && missVec(0), "perfCntItlbMissCnt0", Perf)
  }

  // Log
  for(i <- 0 until Width) {
    XSDebug(req(i).valid, p"req(${i.U}): (${req(i).valid} ${req(i).ready}) ${req(i).bits}\n")
    XSDebug(resp(i).valid, p"resp(${i.U}): (${resp(i).valid} ${resp(i).ready}) ${resp(i).bits}\n")
  }

  XSDebug(sfence.valid, p"Sfence: ${sfence}\n")
  XSDebug(ParallelOR(valid)|| ptw.resp.valid, p"CSR: ${csr}\n")
  XSDebug(ParallelOR(valid) || ptw.resp.valid, p"vmEnable:${vmEnable} hit:${Binary(VecInit(hitVec).asUInt)} miss:${Binary(VecInit(missVec).asUInt)} v:${Hexadecimal(v)} pf:${Hexadecimal(pf)}\n")
  XSDebug(ptw.req.fire(), p"PTW req:${ptw.req.bits}\n")
  XSDebug(ptw.resp.valid, p"PTW resp:${ptw.resp.bits} (v:${ptw.resp.valid}r:${ptw.resp.ready}) \n")
}

object TLB {
  def apply
  (
    in: Seq[BlockTlbRequestIO],
    sfence: SfenceBundle,
    csr: TlbCsrBundle,
    width: Int,
    isDtlb: Boolean,
    shouldBlock: Boolean
  ) = {
    require(in.length == width)

    val tlb = Module(new TLB(width, isDtlb))

    tlb.io.sfence <> sfence
    tlb.io.csr <> csr

    if (!shouldBlock) { // dtlb
      for (i <- 0 until width) {
        tlb.io.requestor(i) <> in(i)
        // tlb.io.requestor(i).req.valid := in(i).req.valid
        // tlb.io.requestor(i).req.bits := in(i).req.bits
        // in(i).req.ready := tlb.io.requestor(i).req.ready

        // in(i).resp.valid := tlb.io.requestor(i).resp.valid
        // in(i).resp.bits := tlb.io.requestor(i).resp.bits
        // tlb.io.requestor(i).resp.ready := in(i).resp.ready
      }
    } else { // itlb
      require(width == 1)
      tlb.io.requestor(0).req.valid := in(0).req.valid
      tlb.io.requestor(0).req.bits := in(0).req.bits
      in(0).req.ready := !tlb.io.requestor(0).resp.bits.miss && in(0).resp.ready && tlb.io.requestor(0).req.ready

      in(0).resp.valid := tlb.io.requestor(0).resp.valid && !tlb.io.requestor(0).resp.bits.miss
      in(0).resp.bits := tlb.io.requestor(0).resp.bits
      tlb.io.requestor(0).resp.ready := in(0).resp.ready
    }

    tlb.io.ptw
  }
}
