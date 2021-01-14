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

class TlbPermBundle extends TlbBundle {
  val pf = Bool() // NOTE: if this is true, just raise pf
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()

  // pma perm check
  // val at = Bool() // Access Type
  // val as = Bool() // Atomic Swap
  // val al = Bool() // Atomic Logical
  // val aa = Bool() // Atomic Arithmetic
  // TODO: add pma check
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
  val perm = new TlbPermBundle
  // TODO: change perm to every kinds of pf check

  override def toPrintable: Printable = {
    p"ppn:0x${Hexadecimal(ppn)} perm:${perm}"
  }
}

class TlbEntry(superpage: Boolean = false, superpageOnly: Boolean = false) extends TlbBundle {
  val tag = UInt(vpnLen.W) // tag is vpn
  val level = UInt(log2Up(Level).W) // 2 for 4KB, 1 for 2MB, 0 for 1GB
  val data = new TlbEntryData

  def hit(vpn: UInt):Bool = {
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

  def apply(vpn: UInt, ppn: UInt, level: UInt, perm: UInt, pf: Bool) = {
    this.tag := vpn
    this.level := level
    this.data.ppn := ppn
    val ptePerm = perm.asTypeOf(new PermBundle)
    this.data.perm.pf:= pf
    this.data.perm.d := ptePerm.d
    this.data.perm.a := ptePerm.a
    this.data.perm.g := ptePerm.g
    this.data.perm.u := ptePerm.u
    this.data.perm.x := ptePerm.x
    this.data.perm.w := ptePerm.w
    this.data.perm.r := ptePerm.r

    XSDebug(p"refill: vpn:${Hexadecimal(vpn)} level:${level} ppn:${Hexadecimal(ppn)} pf:${pf} d:${ptePerm.d} a:${ptePerm.a} g:${ptePerm.g} u:${ptePerm.u} x:${ptePerm.x} w:${ptePerm.w} r:${ptePerm.r}\n")("Tlbrefill")

    this
  }

  override def toPrintable: Printable = {
    p"vpn:0x${Hexadecimal(tag)} level:${level} data:${data}"
  }

  override def cloneType: this.type = (new TlbEntry(superpage, superpageOnly)).asInstanceOf[this.type]
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
  val vmEnable = satp.mode === 8.U // && (mode < ModeM) // FIXME: fix me when boot xv6/linux...
  // val vmEnable = satp.mode === 8.U && (mode < ModeM)

  val reqAddr = req.map(_.bits.vaddr.asTypeOf(vaBundle))
  val cmd     = req.map(_.bits.cmd)
  val valid   = req.map(_.valid)

  def widthMapSeq[T <: Seq[Data]](f: Int => T) = (0 until Width).map(f)
  def widthMap[T <: Data](f: Int => T) = (0 until Width).map(f)

  // normal page: 4k
  val v = RegInit(0.U(TlbEntrySize.W))
  // val pf = RegInit(0.U(TlbEntrySize.W))
  val entry = Reg(Vec(TlbEntrySize, new TlbEntry))
  val pf = VecInit(entry.map(_.data.perm.pf)).asUInt & v
  val g = VecInit(entry.map(_.data.perm.g)).asUInt

  /**
    * PTW refill
    */
  val refill = ptw.resp.fire()
  val randIdx = LFSR64()(log2Up(TlbEntrySize)-1,0)
  val priorIdx = PriorityEncoder(~(v))
  val tlbfull = ParallelAND((v).asBools)
  val refillIdx = Mux(tlbfull, randIdx, priorIdx)
  val refillIdxOH = UIntToOH(refillIdx)
  when (refill) {
    val resp = ptw.resp.bits
    v := v | refillIdxOH
    entry(refillIdx).apply(
      vpn   = resp.entry.tag,
      ppn   = resp.entry.ppn,
      level = resp.entry.level,
      perm  = Cat(VecInit(resp.entry.perm).asUInt, 0.U(1.W)).asUInt,
      pf    = resp.pf
    )
    XSDebug(p"Refill: idx:${refillIdx} entry:${resp.entry} pf:${resp.pf}\n")
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
      !(hitSum===0.U || hitSum===1.U)
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

  when (PopCount(pf) > 10.U) { // when too much pf, just clear
    // pf := Mux(refill && ptw.resp.bits.pf, refillIdxOH, 0.U)
    v := v & ~pf
  }

  // sfence (flush)
  when (sfence.valid) {
    ptw.req.valid := false.B
    when (sfence.bits.rs1) { // virtual address *.rs1 <- (rs1===0.U)
      when (sfence.bits.rs2) { // asid, but i do not want to support asid, *.rs2 <- (rs2===0.U)
        // all addr and all asid
        v := 0.U
      }.otherwise {
        // all addr but specific asid
        v := v & g // TODO: need check if reverse is needed
      }
    }.otherwise {
      val sfenceVpn = sfence.bits.addr.asTypeOf(vaBundle).vpn
      when (sfence.bits.rs2) {
        // specific addr but all asid
        v := v & ~VecInit(entry.map(_.hit(sfenceVpn))).asUInt
      }.otherwise {
        // specific addr and specific asid
        v := v & ~VecInit(entry.map(e => e.hit(sfenceVpn) && !e.data.perm.g)).asUInt
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

  // NOTE: just for simple tlb debug, comment it after tlb's debug
  for (i <- 0 until Width) {
    if(isDtlb) {
      XSDebug(!(!vmEnable || RegNext(req(i).bits.vaddr)===resp(i).bits.paddr || !resp(i).valid || resp(i).bits.miss), p"Dtlb: vaddr:${Hexadecimal(RegNext(req(i).bits.vaddr))} paddr:${Hexadecimal(resp(i).bits.paddr)} should be equal\n")
      assert(!vmEnable || RegNext(req(i).bits.vaddr)===resp(i).bits.paddr || !resp(i).valid || resp(i).bits.miss)
    } else {
      XSDebug(!(!vmEnable || req(i).bits.vaddr===resp(i).bits.paddr || !resp(i).valid) || resp(i).bits.miss, p"Itlb: vaddr:${Hexadecimal(RegNext(req(i).bits.vaddr))} paddr:${Hexadecimal(resp(i).bits.paddr)} should be equal\n")
      assert(!vmEnable || req(i).bits.vaddr===resp(i).bits.paddr || !resp(i).valid || resp(i).bits.miss)
    }
  }
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
