package xiangshan.cache

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.roq.RoqPtr
import xiangshan.backend.fu.util.HasCSRConst
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

class PtePermBundle extends TlbBundle {
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()

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
  override def toPrintable: Printable = {
    p"pf:${pf} d:${d} a:${a} g:${g} u:${u} x:${x} w:${w} r:${r}"
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
  val perm = new TlbPermBundle
  // TODO: change perm to every kinds of pf check

  override def toPrintable: Printable = {
    p"ppn:0x${Hexadecimal(ppn)} perm:${perm}"
  }
}

class TlbEntry(superpage: Boolean = false) extends TlbBundle {
  val tag = UInt(vpnLen.W) // tag is vpn
  val level = if(superpage) Some(UInt(1.W)) else None // /*2 for 4KB,*/ 1 for 2MB, 0 for 1GB
  val data = new TlbEntryData


  def hit(vpn: UInt): Bool = {
    if (superpage) {
      val insideLevel = level.getOrElse(0.U)
      val a = tag(vpnnLen*3-1, vpnnLen*2) === vpn(vpnnLen*3-1, vpnnLen*2)
      val b = tag(vpnnLen*2-1, vpnnLen*1) === vpn(vpnnLen*2-1, vpnnLen*1)
      XSDebug(Mux(insideLevel.asBool, a&b, a), p"Hit superpage: hit:${Mux(insideLevel.asBool, a&b, a)} tag:${Hexadecimal(tag)} level:${insideLevel} data:${data} a:${a} b:${b} vpn:${Hexadecimal(vpn)}\n")
      Mux(insideLevel.asBool, a&b, a)
    } else {
      XSDebug(tag === vpn, p"Hit normalpage: hit:${tag === vpn} tag:${Hexadecimal(tag)} data:${data}  vpn:${Hexadecimal(vpn)}\n")
      tag === vpn
    }
  }

  def ppn(vpn: UInt): UInt = {
    if (superpage) {
      val insideLevel = level.getOrElse(0.U)
      Mux(insideLevel.asBool, Cat(data.ppn(data.ppn.getWidth-1, vpnnLen*1), vpn(vpnnLen*1-1, 0)),
                              Cat(data.ppn(data.ppn.getWidth-1, vpnnLen*2), vpn(vpnnLen*2-1, 0)))
    } else {
      data.ppn
    }
  }

  def apply(vpn: UInt, ppn: UInt, level: UInt, perm: UInt, pf: Bool) = {
    this.tag := vpn
    this.level.map(_ := level(0))
    this.data.ppn := ppn
    val ptePerm = perm.asTypeOf(new PtePermBundle)
    this.data.perm.pf:= pf
    this.data.perm.d := ptePerm.d
    this.data.perm.a := ptePerm.a
    this.data.perm.g := ptePerm.g
    this.data.perm.u := ptePerm.u
    this.data.perm.x := ptePerm.x
    this.data.perm.w := ptePerm.w
    this.data.perm.r := ptePerm.r

    this
  }

  override def toPrintable: Printable = {
    val insideLevel = level.getOrElse(0.U)
    p"vpn:0x${Hexadecimal(tag)} level:${insideLevel} data:${data}"
  }

  override def cloneType: this.type = (new TlbEntry(superpage)).asInstanceOf[this.type]
}

object TlbCmd {
  def read  = "b00".U
  def write = "b01".U
  def exec  = "b10".U

  def atom_read  = "b100".U // lr
  def atom_write = "b101".U // sc / amo

  def apply() = UInt(3.W)
  def isRead(a: UInt) = a(1,0)===read
  def isWrite(a: UInt) = a(1,0)===write
  def isExec(a: UInt) = a(1,0)===exec

  def isAtom(a: UInt) = a(2)
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
  val mmio = Bool()
  val excp = new Bundle {
    val pf = new Bundle {
      val ld = Bool()
      val st = Bool()
      val instr = Bool()
    }
    val af = new Bundle {
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

  // Normal page && Super page
  val nv = RegInit(VecInit(Seq.fill(TlbEntrySize)(false.B)))
  val nentry = Reg(Vec(TlbEntrySize, new TlbEntry(false)))
  val sv = RegInit(VecInit(Seq.fill(TlbSPEntrySize)(false.B)))
  val sentry = Reg(Vec(TlbSPEntrySize, new TlbEntry(true)))
  val v = nv ++ sv
  val entry = nentry ++ sentry
  val g = VecInit(entry.map(_.data.perm.g))
  val pf = VecInit(entry.zip(v).map{ case(e, vi) => e.data.perm.pf & vi })

  /**
    * PTW refill
    */
  val refill = ptw.resp.fire()
  def randReplace(v: UInt) = {
    val width = v.getWidth
    val randIdx = LFSR64()(log2Up(width)-1, 0)
    val priorIdx = PriorityEncoder(~(v))
    val full = Cat(v).andR
    Mux(full, randIdx, priorIdx)
  }

  when (refill) {
    val resp = ptw.resp.bits
    when (resp.entry.level === 2.U) {
      val refillIdx = randReplace(nv.asUInt)
      nv(refillIdx) := true.B
      nentry(refillIdx).apply(
        vpn   = resp.entry.tag,
        ppn   = resp.entry.ppn,
        level = resp.entry.level,
        perm  = VecInit(resp.entry.perm).asUInt,
        pf    = resp.pf
      )
      XSDebug(p"Refill normal: idx:${refillIdx} entry:${resp.entry} pf:${resp.pf}\n")
    }.otherwise {
      val refillIdx = randReplace(sv.asUInt)
      sv(refillIdx) := true.B
      sentry(refillIdx).apply(
        vpn   = resp.entry.tag,
        ppn   = resp.entry.ppn,
        level = resp.entry.level,
        perm  = VecInit(resp.entry.perm).asUInt,
        pf    = resp.pf
      )
      XSDebug(p"Refill superpage: idx:${refillIdx} entry:${resp.entry} pf:${resp.pf}\n")
    }
  }

  /**
    * L1 TLB read
    */
  // val tlb_read_mask = Mux(refill, ((1<<(TlbEntrySize+TlbSPEntrySize))-1).U, 0.U((TlbEntrySize+TlbSPEntrySize).W))
  def TLBNormalRead(i: Int) = {
    val entryHitVec = (
      if (isDtlb)
        VecInit(entry.map{ e => ~refill && e.hit(reqAddr(i).vpn/*, satp.asid*/)})
      else
        VecInit(entry.map(_.hit(reqAddr(i).vpn/*, satp.asid*/)))
    )

    val reqAddrReg = if (isDtlb) RegNext(reqAddr(i)) else reqAddr(i)
    val cmdReg = if (isDtlb) RegNext(cmd(i)) else cmd(i)
    val validReg = if (isDtlb) RegNext(valid(i)) else valid(i)
    val entryHitVecReg = if (isDtlb) RegNext(entryHitVec) else entryHitVec

    val hitVec  = (v zip entryHitVecReg).map{ case (a,b) => a&b }
    val pfHitVec   = (pf zip entryHitVecReg).map{ case (a,b) => a&b }
    val pfArray = ParallelOR(pfHitVec).asBool && validReg && vmEnable
    val hit     = ParallelOR(hitVec).asBool && validReg && vmEnable && ~pfArray
    val miss    = !hit && validReg && vmEnable && ~pfArray
    val hitppn  = ParallelMux(hitVec zip entry.map(_.ppn(reqAddrReg.vpn)))
    val hitPerm = ParallelMux(hitVec zip entry.map(_.data.perm))

    XSDebug(valid(i), p"(${i.U}) entryHit:${Hexadecimal(entryHitVec.asUInt)}\n")
    XSDebug(validReg, p"(${i.U}) entryHitReg:${Hexadecimal(entryHitVecReg.asUInt)} hitVec:${Hexadecimal(VecInit(hitVec).asUInt)} pfHitVec:${Hexadecimal(VecInit(pfHitVec).asUInt)} pfArray:${Hexadecimal(pfArray.asUInt)} hit:${hit} miss:${miss} hitppn:${Hexadecimal(hitppn)} hitPerm:${hitPerm}\n")

    val multiHit = {
      val hitSum = PopCount(hitVec)
      !(hitSum===0.U || hitSum===1.U)
    }

    // resp  // TODO: A/D has not being concerned
    val paddr = Cat(hitppn, reqAddrReg.off)
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

    val (pmaMode, accessWidth) = AddressSpace.memmapAddrMatch(resp(i).bits.paddr)
    resp(i).bits.mmio := Mux(TlbCmd.isExec(cmdReg), !PMAMode.icache(pmaMode), !PMAMode.dcache(pmaMode))
    resp(i).bits.excp.af.ld    := Mux(TlbCmd.isAtom(cmdReg), !PMAMode.atomic(pmaMode), !PMAMode.read(pmaMode)) && TlbCmd.isRead(cmdReg)
    resp(i).bits.excp.af.st    := Mux(TlbCmd.isAtom(cmdReg), !PMAMode.atomic(pmaMode), !PMAMode.write(pmaMode)) && TlbCmd.isWrite(cmdReg)
    resp(i).bits.excp.af.instr := Mux(TlbCmd.isAtom(cmdReg), false.B, !PMAMode.execute(pmaMode))

    (hit, miss, pfHitVec, multiHit)
  }

  val readResult = (0 until Width).map(TLBNormalRead(_))
  val hitVec = readResult.map(res => res._1)
  val missVec = readResult.map(res => res._2)
  val pfHitVecVec = readResult.map(res => res._3)
  val multiHitVec = readResult.map(res => res._4)
  val hasMissReq = Cat(missVec).orR

  // ptw
  val waiting = RegInit(false.B)
  when (ptw.req.fire()) {
    waiting := true.B
  }.elsewhen (sfence.valid || ptw.resp.valid) {
    waiting := false.B
  }
  // ptw <> DontCare // TODO: need check it
  ptw.req.valid := hasMissReq && !sfence.valid && !waiting && !RegNext(refill)
  ptw.resp.ready := waiting

  // val ptwReqSeq = Wire(Seq.fill(Width)(new comBundle()))
  val ptwReqSeq = Seq.fill(Width)(Wire(new comBundle()))
  for (i <- 0 until Width) {
    ptwReqSeq(i).valid := ((if (isDtlb) RegNext(valid(i)) else valid(i)) && missVec(i))
    ptwReqSeq(i).roqIdx := (if (isDtlb) RegNext(req(i).bits.roqIdx) else req(i).bits.roqIdx)
    ptwReqSeq(i).bits.vpn := (if (isDtlb) RegNext(reqAddr(i).vpn) else reqAddr(i).vpn)
  }
  ptw.req.bits := Compare(ptwReqSeq).bits

  val tooManyPf = PopCount(pf) > 5.U
  when (tooManyPf) { // when too much pf, just clear
    XSDebug(p"Too many pf just flush all the pf v:${Hexadecimal(VecInit(v).asUInt)} pf:${Hexadecimal(pf.asUInt)}\n")
    v.zipWithIndex.map{ case (a, i) => a := a & !pf(i) }
  }

  // sfence (flush)
  when (sfence.valid) {
    ptw.req.valid := false.B
    when (sfence.bits.rs1) { // virtual address *.rs1 <- (rs1===0.U)
      when (sfence.bits.rs2) { // asid, but i do not want to support asid, *.rs2 <- (rs2===0.U)
        // all addr and all asid
        v.map(_ := false.B)
      }.otherwise {
        // all addr but specific asid
        v.zipWithIndex.map{ case (a,i) => a := a & g(i) }
      }
    }.otherwise {
      val sfenceVpn = sfence.bits.addr.asTypeOf(vaBundle).vpn
      when (sfence.bits.rs2) {
        // specific addr but all asid
        v.zipWithIndex.map{ case (a,i) => a := a & !entry(i).hit(sfenceVpn) }
      }.otherwise {
        // specific addr and specific asid
        v.zipWithIndex.map{ case (a,i) => a := a & !(entry(i).hit(sfenceVpn) && !g(i))}
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
  XSDebug(ParallelOR(valid) || ptw.resp.valid, p"vmEnable:${vmEnable} hit:${Binary(VecInit(hitVec).asUInt)} miss:${Binary(VecInit(missVec).asUInt)} v:${Hexadecimal(VecInit(v).asUInt)} pf:${Hexadecimal(pf.asUInt)}\n")
  XSDebug(ptw.req.fire(), p"PTW req:${ptw.req.bits}\n")
  XSDebug(ptw.resp.valid, p"PTW resp:${ptw.resp.bits} (v:${ptw.resp.valid}r:${ptw.resp.ready}) \n")

//   // NOTE: just for simple tlb debug, comment it after tlb's debug
//   for (i <- 0 until Width) {
//     if(isDtlb) {
//       XSDebug(!(!vmEnable || RegNext(req(i).bits.vaddr)===resp(i).bits.paddr || !resp(i).valid || resp(i).bits.miss || Cat(VecInit(resp(i).bits.excp.pf).asUInt).orR), p"Dtlb: vaddr:${Hexadecimal(RegNext(req(i).bits.vaddr))} paddr:${Hexadecimal(resp(i).bits.paddr)} should be equal\n")
//       assert(!vmEnable || RegNext(req(i).bits.vaddr)===resp(i).bits.paddr || !resp(i).valid || resp(i).bits.miss || Cat(VecInit(resp(i).bits.excp.pf).asUInt).orR)
//     } else {
//       XSDebug(!(!vmEnable || req(i).bits.vaddr===resp(i).bits.paddr || !resp(i).valid || resp(i).bits.miss || Cat(VecInit(resp(i).bits.excp.pf).asUInt).orR), p"Itlb: vaddr:${Hexadecimal(RegNext(req(i).bits.vaddr))} paddr:${Hexadecimal(resp(i).bits.paddr)} should be equal\n")
//       assert(!vmEnable || req(i).bits.vaddr===resp(i).bits.paddr || !resp(i).valid || resp(i).bits.miss || Cat(VecInit(resp(i).bits.excp.pf).asUInt).orR)
//     }
//   }
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
