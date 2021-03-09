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
  val MemBandWidth  = 256 // TODO: change to IO bandwidth param

  // ptwl1: fully-associated
  val PtwL1TagLen = vpnnLen
  val ptwl1Replacer = Some("plru")

  /* +-------+----------+-------------+
   * |  Tag  |  SetIdx  |  SectorIdx  |
   * +-------+----------+-------------+
   */
  // ptwl2: 8-way group-associated
  val PtwL2WayNum = 8
  val PtwL2WaySize = PtwL2EntrySize / PtwL2WayNum
  val PtwL2SectorSize = MemBandWidth/XLEN
  val PtwL2LineSize = PtwL2SectorSize * PtwL2WayNum
  val PtwL2LineNum  = PtwL2EntrySize / PtwL2LineSize
  val PtwL2IdxLen = log2Up(PtwL2WaySize)
  val PtwL2SectorIdxLen = log2Up(PtwL2SectorSize)
  val PtwL2SetIdxLen = log2Up(PtwL2LineNum)
  val PtwL2TagLen = vpnnLen * 2 - PtwL2IdxLen
  val ptwl2Replacer = Some("setplru")

  // ptwl3: 16-way group-associated
  val PtwL3WayNum = 16
  val PtwL3WaySize = PtwL3EntrySize / PtwL3WayNum
  val PtwL3SectorSize = MemBandWidth / XLEN
  val PtwL3LineSize = PtwL3SectorSize * PtwL3WayNum
  val PtwL3LineNum  = PtwL3EntrySize / PtwL3LineSize
  val PtwL3IdxLen = log2Up(PtwL3WaySize)
  val PtwL3SectorIdxLen = log2Up(PtwL3SectorSize)
  val PtwL3SetIdxLen = log2Up(PtwL3LineNum)
  val PtwL3TagLen = vpnnLen * 3 - PtwL3IdxLen
  val ptwl3Replacer = Some("setplru")

  // super page, including 1GB and 2MB page
  val SPTagLen = vpnnLen * 2
  val spReplacer = Some("plru")

  def genPtwL2Idx(vpn: UInt) = {
    (vpn(vpnLen - 1, vpnnLen))(PtwL2IdxLen - 1, 0)
  }

  def genPtwL2SectorIdx(vpn: UInt) = {
    genPtwL2Idx(vpn)(PtwL2SectorIdxLen - 1, 0)
  }

  def genPtwL2SetIdx(vpn: UInt) = {
    genPtwL2Idx(vpn)(PtwL2SetIdxLen + PtwL2SectorIdxLen - 1, PtwL2SectorIdxLen)
  }

  def genPtwL3Idx(vpn: UInt) = {
    vpn(PtwL3IdxLen - 1, 0)
  }

  def genPtwL3SectorIdx(vpn: UInt) = {
    genPtwL3Idx(vpn)(PtwL3SectorIdxLen - 1, 0)
  }

  def genPtwL3SetIdx(vpn: UInt) = {
    genPtwL3Idx(vpn)(PtwL3SetIdxLen + PtwL3SectorIdxLen - 1, PtwL3SectorIdxLen)
  }

  def MakeAddr(ppn: UInt, off: UInt) = {
    require(off.getWidth == 9)
    Cat(ppn, off, 0.U(log2Up(XLEN/8).W))(PAddrBits-1, 0)
  }

  def getVpnn(vpn: UInt, idx: Int) = {
    vpn(vpnnLen*(idx+1)-1, vpnnLen*idx)
  }

  def getVpnClip(vpn: UInt, level: Int) = {
    // level 0  /* vpnn2 */
    // level 1  /* vpnn2 * vpnn1 */
    // level 2  /* vpnn2 * vpnn1 * vpnn0*/
    vpn(vpnLen - 1, (2 - level) * vpnnLen)
  }

  def printVec[T <: Data](x: Seq[T]): Printable = {
    (0 until x.length).map(i => p"(${i.U})${x(i)} ").reduce(_+_)
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

  def unaligned(level: UInt) = {
    assert(level=/=3.U)
    isLeaf() && !(level === 2.U ||
                  level === 1.U && ppn(vpnnLen-1,   0) === 0.U ||
                  level === 0.U && ppn(vpnnLen*2-1, 0) === 0.U)
  }

  def isPf(level: UInt) = {
    !perm.v || (!perm.r && perm.w) || unaligned(level)
  }

  def isLeaf() = {
    perm.r || perm.x || perm.w
  }

  def getPerm() = {
    val p = Wire(new PtePermBundle)
    p.d := perm.d
    p.a := perm.a
    p.g := perm.g
    p.u := perm.u
    p.x := perm.x
    p.w := perm.w
    p.r := perm.r
    p
  }

  override def toPrintable: Printable = {
    p"ppn:0x${Hexadecimal(ppn)} perm:b${Binary(perm.asUInt)}"
  }
}

class PtwEntry(tagLen: Int, hasPerm: Boolean = false, hasLevel: Boolean = false) extends PtwBundle {
  val tag = UInt(tagLen.W)
  val ppn = UInt(ppnLen.W)
  val perm = if (hasPerm) Some(new PtePermBundle) else None
  val level = if (hasLevel) Some(UInt(log2Up(Level).W)) else None

  def hit(vpn: UInt) = {
    require(vpn.getWidth == vpnLen)
    if (hasLevel) {
      val hit0 = tag(tagLen - 1, tagLen - vpnnLen) === vpn(vpnLen - 1, vpnLen - vpnnLen)
      val hit1 = tag(tagLen - vpnnLen - 1, tagLen - vpnnLen * 2) === vpn(vpnLen - vpnnLen - 1, vpnLen - vpnnLen * 2)
      Mux(level.getOrElse(0.U) === 0.U, hit0, hit0 && hit1)
    } else {
      tag === vpn(vpnLen - 1, vpnLen - tagLen)
    }
  }

  def refill(vpn: UInt, pte: UInt, level: UInt = 0.U) {
    tag := vpn(vpnLen - 1, vpnLen - tagLen)
    ppn := pte.asTypeOf(pteBundle).ppn
    perm.map(_ := pte.asTypeOf(pteBundle).perm)
    this.level.map(_ := level)
  }

  def genPtwEntry(vpn: UInt, pte: UInt, level: UInt = 0.U) = {
    val e = Wire(new PtwEntry(tagLen, hasPerm, hasLevel))
    e.refill(vpn, pte, level)
    e
  }

  override def cloneType: this.type = (new PtwEntry(tagLen, hasPerm, hasLevel)).asInstanceOf[this.type]

  override def toPrintable: Printable = {
    // p"tag:0x${Hexadecimal(tag)} ppn:0x${Hexadecimal(ppn)} perm:${perm}"
    p"tag:0x${Hexadecimal(tag)} ppn:0x${Hexadecimal(ppn)} " +
      (if (hasPerm) p"perm:${perm.getOrElse(0.U.asTypeOf(new PtePermBundle))} " else p"") +
      (if (hasLevel) p"level:${level.getOrElse(0.U)}" else p"")
  }
}

class PtwEntries(num: Int, tagLen: Int, level: Int, hasPerm: Boolean) extends PtwBundle {
  require(log2Up(num)==log2Down(num))

  val tag  = UInt(tagLen.W)
  val ppns = Vec(num, UInt(ppnLen.W))
  val vs   = Vec(num, Bool())
  val perms = if (hasPerm) Some(Vec(num, new PtePermBundle)) else None
  // println(s"PtwEntries: tag:1*${tagLen} ppns:${num}*${ppnLen} vs:${num}*1")

  def tagClip(vpn: UInt) = {
    require(vpn.getWidth == vpnLen)
    vpn(vpnLen - 1, vpnLen - tagLen)
  }

  def sectorIdxClip(vpn: UInt, level: Int) = {
    getVpnClip(vpn, level)(log2Up(num) - 1, 0)
  }

  def hit(vpn: UInt) = {
    tag === tagClip(vpn) && vs(sectorIdxClip(vpn, level)) // TODO: optimize this. don't need to compare each with tag
  }

  def genEntries(vpn: UInt, data: UInt, levelUInt: UInt) = {
    require((data.getWidth / XLEN) == num,
      "input data length must be multiple of pte length")

    val ps = Wire(new PtwEntries(num, tagLen, level, hasPerm))
    ps.tag := tagClip(vpn)
    for (i <- 0 until num) {
      val pte = data((i+1)*XLEN-1, i*XLEN).asTypeOf(new PteBundle)
      ps.ppns(i) := pte.ppn
      ps.vs(i)   := !pte.isPf(levelUInt) && (if (hasPerm) pte.isLeaf() else !pte.isLeaf())
      ps.perms.map(_(i) := pte.perm)
    }
    ps
  }

  override def cloneType: this.type = (new PtwEntries(num, tagLen, level, hasPerm)).asInstanceOf[this.type]
  override def toPrintable: Printable = {
    // require(num == 4, "if num is not 4, please comment this toPrintable")
    // NOTE: if num is not 4, please comment this toPrintable
    val permsInner = perms.getOrElse(0.U.asTypeOf(Vec(num, new PtePermBundle)))
    p"tag:0x${Hexadecimal(tag)} ppns:${printVec(ppns)} vs:${Binary(vs.asUInt)} " +
      (if (hasPerm) p"perms:${printVec(permsInner)}" else p"")
  }
}

class PtwReq extends PtwBundle {
  val vpn = UInt(vpnLen.W)

  override def toPrintable: Printable = {
    p"vpn:0x${Hexadecimal(vpn)}"
  }
}

class PtwResp extends PtwBundle {
  val entry = new PtwEntry(tagLen = vpnLen, hasPerm = true, hasLevel = true)
  val pf  = Bool()

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
    when (flush) { valid := false.B }
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

class PTWImp(outer: PTW) extends PtwModule(outer) {

  val (mem, edge) = outer.node.out.head
  require(mem.d.bits.data.getWidth == l1BusDataWidth, "PTW: tilelink width does not match")

  val io = IO(new PtwIO)
  val difftestIO = IO(new Bundle() {
    val ptwResp = Output(Bool())
    val ptwAddr = Output(UInt(64.W))
    val ptwData = Output(Vec(4, UInt(64.W)))
  })

  difftestIO <> DontCare

  val arb = Module(new Arbiter(new PtwReq, PtwWidth))
  arb.io.in <> VecInit(io.tlb.map(_.req))
  val arbChosen = RegEnable(arb.io.chosen, arb.io.out.fire())
  val req = RegEnable(arb.io.out.bits, arb.io.out.fire())
  val resp  = VecInit(io.tlb.map(_.resp))
  val vpn = req.vpn
  val sfence = io.sfence
  val csr    = io.csr
  val satp   = csr.satp
  val priv   = csr.priv

  val valid = ValidHold(arb.io.out.fire(), resp(arbChosen).fire(), sfence.valid)
  val validOneCycle = OneCycleValid(arb.io.out.fire(), sfence.valid)
  arb.io.out.ready := !valid// || resp(arbChosen).fire()

  // l1: level 0 non-leaf pte
  val l1 = Reg(Vec(PtwL1EntrySize, new PtwEntry(tagLen = PtwL1TagLen)))
  val l1v = RegInit(0.U(PtwL1EntrySize.W))
  val l1g = Reg(UInt(PtwL1EntrySize.W))

  // l2: level 1 non-leaf pte
  val l2 = Module(new SRAMTemplate(
    new PtwEntries(num = PtwL2SectorSize, tagLen = PtwL2TagLen, level = 1, hasPerm = false),
    set = PtwL2LineNum,
    way = PtwL2WayNum,
    singlePort = true
  ))
  val l2v = RegInit(0.U((PtwL2LineNum * PtwL2WayNum).W))
  val l2g = Reg(UInt((PtwL2LineNum * PtwL2WayNum).W))
  def getl2vSet(vpn: UInt) = {
    require(log2Up(PtwL2WayNum) == log2Down(PtwL2WayNum))
    val set = genPtwL2SetIdx(vpn)
    require(set.getWidth == log2Up(PtwL2LineNum))
    val l2vVec = l2v.asTypeOf(Vec(PtwL2LineNum, UInt(PtwL2WayNum.W)))
    l2vVec(set)
  }

  // l3: level 2 leaf pte of 4KB pages
  val l3 = Module(new SRAMTemplate(
    new PtwEntries(num = PtwL3SectorSize, tagLen = PtwL3TagLen, level = 2, hasPerm = true),
    set = PtwL3LineNum,
    way = PtwL3WayNum,
    singlePort = true
  ))
  val l3v = RegInit(0.U((PtwL3LineNum * PtwL3WayNum).W))
  val l3g = Reg(UInt((PtwL3LineNum * PtwL3WayNum).W))
  def getl3vSet(vpn: UInt) = {
    require(log2Up(PtwL3WayNum) == log2Down(PtwL3WayNum))
    val set = genPtwL3SetIdx(vpn)
    require(set.getWidth == log2Up(PtwL3LineNum))
    val l3vVec = l3v.asTypeOf(Vec(PtwL3LineNum, UInt(PtwL3WayNum.W)))
    l3vVec(set)
  }

  // sp: level 0/1 leaf pte of 1GB/2MB super pages
  val sp = Reg(Vec(PtwSPEntrySize, new PtwEntry(tagLen = SPTagLen, hasPerm = true, hasLevel = true)))
  val spv = RegInit(0.U(PtwSPEntrySize.W))
  val spg = Reg(UInt(PtwSPEntrySize.W))

  // mem alias
  val memRespFire = mem.d.fire()
  val memRdata = mem.d.bits.data
  val memSelData = Wire(UInt(XLEN.W))
  val memPte   = memSelData.asTypeOf(new PteBundle)
  val memPteReg = RegEnable(memPte, memRespFire)
  val memPtes  =(0 until PtwL3SectorSize).map(i => memRdata((i+1)*XLEN-1, i*XLEN).asTypeOf(new PteBundle))
  val memValid = mem.d.valid
  val memRespReady = mem.d.ready
  val memReqReady = mem.a.ready
  val memReqFire = mem.a.fire()

  // fsm
  val s_idle :: s_read_ptw :: s_req :: s_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val level = RegInit(0.U(log2Up(Level).W))
  val levelNext = level + 1.U
  val sfenceLatch = RegEnable(false.B, init = false.B, memValid) // NOTE: store sfence to disable mem.resp.fire(), but not stall other ptw req

  // Access Perf
  val l1AccessPerf = Wire(Vec(PtwL1EntrySize, Bool()))
  val l2AccessPerf = Wire(Vec(PtwL2WayNum, Bool()))
  val l3AccessPerf = Wire(Vec(PtwL3WayNum, Bool()))
  val spAccessPerf = Wire(Vec(PtwSPEntrySize, Bool()))
  l1AccessPerf.map(_ := false.B)
  l2AccessPerf.map(_ := false.B)
  l3AccessPerf.map(_ := false.B)
  spAccessPerf.map(_ := false.B)

  // l1
  val ptwl1replace = ReplacementPolicy.fromString(ptwl1Replacer, PtwL1EntrySize)
  val l1HitReg = Reg(Bool())
  val l1HitPPNReg = Reg(UInt(ppnLen.W))
  val (l1Hit, l1HitPPN) = {
    val hitVecT = l1.zipWithIndex.map { case (e, i) => e.hit(vpn) && l1v(i) }
    val hitVec = hitVecT.map(RegEnable(_, validOneCycle))
    val hitPPN = ParallelPriorityMux(hitVec zip l1.map(_.ppn))
    val hit = ParallelOR(hitVec) && RegNext(validOneCycle)

    when (hit) { ptwl1replace.access(OHToUInt(hitVec)) }

    l1AccessPerf.zip(hitVec).map{ case (l, h) => l := h && RegNext(validOneCycle) }
    for (i <- 0 until PtwL1EntrySize) {
      XSDebug(validOneCycle, p"[l1] l1(${i.U}) ${l1(i)} hit:${l1(i).hit(vpn)}\n")
    }
    XSDebug(validOneCycle, p"[l1] l1v:${Binary(l1v)} hitVecT:${Binary(VecInit(hitVecT).asUInt)}\n")
    XSDebug(valid, p"[l1] l1Hit:${hit} l1HitPPN:0x${Hexadecimal(hitPPN)} l1HitReg:${l1HitReg} l1HitPPNReg:${Hexadecimal(l1HitPPNReg)} hitVec:${VecInit(hitVec).asUInt}\n")

    VecInit(hitVecT).suggestName(s"l1_hitVecT")
    VecInit(hitVec).suggestName(s"l1_hitVec")

    (hit, hitPPN)
  }

  // l2
  val ptwl2replace = ReplacementPolicy.fromString(ptwl2Replacer,PtwL2WayNum,PtwL2LineNum)
  val l2HitReg = Reg(Bool())
  val l2HitPPNReg = Reg(UInt(ppnLen.W))
  val (l2Hit, l2HitPPN) = {
    val ridx = genPtwL2SetIdx(vpn)
    val vidx = RegEnable(VecInit(getl2vSet(vpn).asBools), validOneCycle)
    l2.io.r.req.valid := validOneCycle
    l2.io.r.req.bits.apply(setIdx = ridx)
    val ramDatas = l2.io.r.resp.data
    // val hitVec = VecInit(ramDatas.map{wayData => wayData.hit(vpn) })
    val hitVec = VecInit(ramDatas.zip(vidx).map { case (wayData, v) => wayData.hit(vpn) && v })
    val hitWayData = ParallelPriorityMux(hitVec zip ramDatas)
    val hit = ParallelOR(hitVec) && RegNext(validOneCycle)
    val hitWay = ParallelPriorityMux(hitVec zip (0 until PtwL2WayNum).map(_.U))

    ridx.suggestName(s"l2_ridx")
    vidx.suggestName(s"l2_vidx")
    ramDatas.suggestName(s"l2_ramDatas")
    hitVec.suggestName(s"l2_hitVec")
    hitWayData.suggestName(s"l2_hitWayData")
    hitWay.suggestName(s"l2_hitWay")

    when (hit) { ptwl2replace.access(genPtwL2SetIdx(vpn), hitWay) }

    l2AccessPerf.zip(hitVec).map{ case (l, h) => l := h && RegNext(validOneCycle) }
    XSDebug(validOneCycle, p"[l2] ridx:0x${Hexadecimal(ridx)}\n")
    for (i <- 0 until PtwL2WayNum) {
      XSDebug(RegNext(validOneCycle), p"[l2] ramDatas(${i.U}) ${ramDatas(i)}  l2v:${vidx(i)}  hit:${ramDatas(i).hit(vpn)}\n")
    }
    XSDebug(valid, p"[l2] l2Hit:${hit} l2HitPPN:0x${Hexadecimal(hitWayData.ppns(genPtwL2SectorIdx(vpn)))} l2HitReg:${l2HitReg} l2HitPPNReg:0x${Hexadecimal(l2HitPPNReg)} hitVec:${Binary(hitVec.asUInt)} hitWay:${hitWay} vidx:${Binary(vidx.asUInt)}\n")

    (hit, hitWayData.ppns(genPtwL2SectorIdx(vpn)))
  }

  // l3
  val ptwl3replace = ReplacementPolicy.fromString(ptwl3Replacer,PtwL3WayNum,PtwL3LineNum)
  val l3HitReg = Reg(Bool())
  val (l3Hit, l3HitData) = {
    val ridx = genPtwL3SetIdx(vpn)
    val vidx = RegEnable(VecInit(getl3vSet(vpn).asBools), validOneCycle)
    l3.io.r.req.valid := validOneCycle
    l3.io.r.req.bits.apply(setIdx = ridx)
    val ramDatas = l3.io.r.resp.data
    val hitVec = VecInit(ramDatas.zip(vidx).map{ case (wayData, v) => wayData.hit(vpn) && v })
    val hitWayData = ParallelPriorityMux(hitVec zip ramDatas)
    val hit = ParallelOR(hitVec) && RegNext(validOneCycle)
    val hitWay = ParallelPriorityMux(hitVec zip (0 until PtwL3WayNum).map(_.U))

    when (hit) { ptwl3replace.access(genPtwL3SetIdx(vpn), hitWay) }

    l3AccessPerf.zip(hitVec).map{ case (l, h) => l := h && RegNext(validOneCycle) }
    XSDebug(validOneCycle, p"[l3] ridx:0x${Hexadecimal(ridx)}\n")
    for (i <- 0 until PtwL3WayNum) {
      XSDebug(RegNext(validOneCycle), p"[l3] ramDatas(${i.U}) ${ramDatas(i)}  l3v:${vidx(i)}  hit:${ramDatas(i).hit(vpn)}\n")
    }
    XSDebug(valid, p"[l3] l3Hit:${hit} l3HitData:${hitWayData} l3HitReg:${l3HitReg} hitVec:${Binary(hitVec.asUInt)} hitWay:${hitWay} vidx:${Binary(vidx.asUInt)}\n")

    ridx.suggestName(s"l3_ridx")
    vidx.suggestName(s"l3_vidx")
    ramDatas.suggestName(s"l3_ramDatas")
    hitVec.suggestName(s"l3_hitVec")
    hitWay.suggestName(s"l3_hitWay")

    (hit, hitWayData)
  }
  val l3HitPPN = l3HitData.ppns(genPtwL3SectorIdx(vpn))
  val l3HitPerm = l3HitData.perms.getOrElse(0.U.asTypeOf(Vec(PtwL3SectorSize, new PtePermBundle)))(genPtwL3SectorIdx(vpn))

  // super page
  val spreplace = ReplacementPolicy.fromString(spReplacer, PtwSPEntrySize)
  val spHitReg = Reg(Bool())
  val (spHit, spHitData) = {
    val hitVecT = sp.zipWithIndex.map { case (e, i) => e.hit(vpn) && spv(i) }
    val hitVec = hitVecT.map(RegEnable(_, validOneCycle))
    val hitData = ParallelPriorityMux(hitVec zip sp)
    val hit = ParallelOR(hitVec) && RegNext(validOneCycle)

    when (hit) { spreplace.access(OHToUInt(hitVec)) }

    spAccessPerf.zip(hitVec).map{ case (s, h) => s := h && RegNext(validOneCycle) }
    for (i <- 0 until PtwSPEntrySize) {
      XSDebug(validOneCycle, p"[sp] sp(${i.U}) ${sp(i)} hit:${sp(i).hit(vpn)} spv:${spv(i)}\n")
    }
    XSDebug(valid, p"[sp] spHit:${hit} spHitData:${hitData} hitVec:${Binary(VecInit(hitVec).asUInt)}\n")

    VecInit(hitVecT).suggestName(s"sp_hitVecT")
    VecInit(hitVec).suggestName(s"sp_hitVec")

    (hit, hitData)
  }
  val spHitPerm = spHitData.perm.getOrElse(0.U.asTypeOf(new PtePermBundle))
  val spHitLevel = spHitData.level.getOrElse(0.U)

  // default values
  // resp.map(_.valid := false.B)
  // resp.map(_.bits := DontCare)
  l2.io.w.req <> DontCare
  l3.io.w.req <> DontCare
  l2.io.w.req.valid := false.B
  l3.io.w.req.valid := false.B

  // fsm
  val pteHit = l3Hit || spHit
  val notFound = WireInit(false.B)
  notFound.suggestName("PtwNotFound")
  switch (state) {
    is (s_idle) {
      when (valid) {
        state := s_read_ptw
        level := 0.U
      }
    }

    is (s_read_ptw) {
      when (pteHit) {
        state := s_idle
      }.otherwise {
        state := s_req
        level := Mux(l2Hit, 2.U, Mux(l1Hit, 1.U, 0.U))
      }
      l1HitReg := l1Hit
      l2HitReg := l2Hit
      l1HitPPNReg := l1HitPPN
      l2HitPPNReg := l2HitPPN
      l3HitReg := l3Hit
      spHitReg := spHit
    }

    is (s_req) {
      when (memReqFire && !sfenceLatch) {
        state := s_resp
      }
    }

    is (s_resp) {
      when (memRespFire) {
        when (memPte.isLeaf() || memPte.isPf(level)) {
          state := s_idle
          notFound := memPte.isPf(level)
        }.otherwise {
          when (level =/= 2.U) {
            level := levelNext
            state := s_req
          }.otherwise {
            state := s_idle
            notFound := true.B
          }
        }
      }
    }
  }

  // mem
  val l1addr = MakeAddr(satp.ppn, getVpnn(vpn, 2))
  val l2addr = MakeAddr(Mux(l1HitReg, l1HitPPNReg, memPteReg.ppn), getVpnn(vpn, 1))
  val l3addr = MakeAddr(Mux(l2HitReg, l2HitPPNReg, memPteReg.ppn), getVpnn(vpn, 0))
  val memAddr = Mux(level === 0.U, l1addr, Mux(level === 1.U, l2addr, l3addr))
  val memAddrReg = RegEnable(memAddr, mem.a.fire())
  val pteRead =  edge.Get(
    fromSource = 0.U/*id*/,
    // toAddress  = memAddr(log2Up(CacheLineSize / 2 / 8) - 1, 0),
    toAddress  = Cat(memAddr(PAddrBits - 1, log2Up(l1BusDataWidth/8)), 0.U(log2Up(l1BusDataWidth/8).W)),
    lgSize     = log2Up(l1BusDataWidth/8).U
  )._2
  mem.a.bits := pteRead
  mem.a.valid := state === s_req && !sfenceLatch && !sfence.valid
  mem.d.ready := state === s_resp || sfenceLatch
  memSelData := memRdata.asTypeOf(Vec(MemBandWidth/XLEN, UInt(XLEN.W)))(memAddrReg(log2Up(l1BusDataWidth/8) - 1, log2Up(XLEN/8)))

  // resp
  val ptwFinish = state === s_read_ptw && pteHit ||
                  memRespFire && !sfenceLatch && (memPte.isLeaf() || memPte.isPf(level) || level === 2.U)
  for (i <- 0 until PtwWidth) {
    resp(i).valid := valid && ptwFinish && arbChosen === i.U
    resp(i).bits.entry.tag := vpn
    resp(i).bits.entry.ppn := Mux(memRespFire, memPte.ppn, Mux(l3Hit, l3HitPPN, spHitData.ppn))
    resp(i).bits.entry.perm.map(_ := Mux(memRespFire, memPte.getPerm(), Mux(l3Hit, l3HitPerm, spHitPerm)))
    resp(i).bits.entry.level.map(_ := Mux(memRespFire, level, Mux(l3Hit, 2.U, spHitLevel)))
    resp(i).bits.pf := level === 3.U || notFound
  }

  // refill Perf
  val l1RefillPerf = Wire(Vec(PtwL1EntrySize, Bool()))
  val l2RefillPerf = Wire(Vec(PtwL2WayNum, Bool()))
  val l3RefillPerf = Wire(Vec(PtwL3WayNum, Bool()))
  val spRefillPerf = Wire(Vec(PtwSPEntrySize, Bool()))
  l1RefillPerf.map(_ := false.B)
  l2RefillPerf.map(_ := false.B)
  l3RefillPerf.map(_ := false.B)
  spRefillPerf.map(_ := false.B)

  // refill
  when (memRespFire && !memPte.isPf(level) && !sfenceLatch) {
    when (level === 0.U && !memPte.isLeaf()) {
      // val refillIdx = LFSR64()(log2Up(PtwL1EntrySize)-1,0) // TODO: may be LRU
      val refillIdx = replaceWrapper(l1v, ptwl1replace.way)
      refillIdx.suggestName(s"PtwL1RefillIdx")
      val rfOH = UIntToOH(refillIdx)
      l1(refillIdx).refill(vpn, memSelData)
      ptwl1replace.access(refillIdx)
      l1v := l1v | rfOH
      l1g := (l1g & ~rfOH) | Mux(memPte.perm.g, rfOH, 0.U)

      for (i <- 0 until PtwL1EntrySize) {
          l1RefillPerf(i) := i.U === refillIdx
      }

      XSDebug(p"[l1 refill] refillIdx:${refillIdx} refillEntry:${l1(refillIdx).genPtwEntry(vpn, memSelData)}\n")
      XSDebug(p"[l1 refill] l1v:${Binary(l1v)}->${Binary(l1v | rfOH)} l1g:${Binary(l1g)}->${Binary((l1g & ~rfOH) | Mux(memPte.perm.g, rfOH, 0.U))}\n")

      refillIdx.suggestName(s"l1_refillIdx")
      rfOH.suggestName(s"l1_rfOH")
    }

    when (level === 1.U && !memPte.isLeaf()) {
      val refillIdx = genPtwL2SetIdx(vpn)
      val victimWay = replaceWrapper(RegEnable(VecInit(getl2vSet(vpn).asBools).asUInt, validOneCycle), ptwl2replace.way(refillIdx))
      val victimWayOH = UIntToOH(victimWay)
      val rfvOH = UIntToOH(Cat(refillIdx, victimWay))
      l2.io.w.apply(
        valid = true.B,
        setIdx = refillIdx,
        data = (new PtwEntries(num = PtwL2SectorSize, tagLen = PtwL2TagLen, level = 1, hasPerm = false)).genEntries(
          vpn = vpn, data = memRdata, levelUInt = 1.U
        ),
        waymask = victimWayOH
      )
      ptwl2replace.access(refillIdx, victimWay)
      l2v := l2v | rfvOH
      l2g := l2g & ~rfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, rfvOH, 0.U)

      for (i <- 0 until PtwL2WayNum) {
        l2RefillPerf(i) := i.U === victimWay
      }

      XSDebug(p"[l2 refill] refillIdx:0x${Hexadecimal(refillIdx)} victimWay:${victimWay} victimWayOH:${Binary(victimWayOH)} rfvOH(in UInt):${Cat(refillIdx, victimWay)}\n")
      XSDebug(p"[l2 refill] refilldata:0x${
        (new PtwEntries(num = PtwL2SectorSize, tagLen = PtwL2TagLen, level = 1, hasPerm = false)).genEntries(
          vpn = vpn, data = memRdata, levelUInt = 1.U)
      }\n")
      XSDebug(p"[l2 refill] l2v:${Binary(l2v)} -> ${Binary(l2v | rfvOH)}\n")
      XSDebug(p"[l2 refill] l2g:${Binary(l2g)} -> ${Binary(l2g & ~rfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, rfvOH, 0.U))}\n")

      refillIdx.suggestName(s"l2_refillIdx")
      victimWay.suggestName(s"l2_victimWay")
      victimWayOH.suggestName(s"l2_victimWayOH")
      rfvOH.suggestName(s"l2_rfvOH")
    }

    when (level === 2.U && memPte.isLeaf()) {
      val refillIdx = genPtwL3SetIdx(vpn)
      val victimWay = replaceWrapper(RegEnable(VecInit(getl3vSet(vpn).asBools).asUInt, validOneCycle), ptwl3replace.way(refillIdx))
      val victimWayOH = UIntToOH(victimWay)
      val rfvOH = UIntToOH(Cat(refillIdx, victimWay))
      l3.io.w.apply(
        valid = true.B,
        setIdx = refillIdx,
        data = (new PtwEntries(num = PtwL3SectorSize, tagLen = PtwL3TagLen, level = 2, hasPerm = true)).genEntries(
          vpn = vpn, data = memRdata, levelUInt = 2.U
        ),
        waymask = victimWayOH
      )
      ptwl3replace.access(refillIdx, victimWay)
      l3v := l3v | rfvOH
      l3g := l3g & ~rfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, rfvOH, 0.U)

        for (i <- 0 until PtwL3WayNum) {
          l3RefillPerf(i) := i.U === victimWay
        }

      XSDebug(p"[l3 refill] refillIdx:0x${Hexadecimal(refillIdx)} victimWay:${victimWay} victimWayOH:${Binary(victimWayOH)} rfvOH(in UInt):${Cat(refillIdx, victimWay)}\n")
      XSDebug(p"[l3 refill] refilldata:0x${
        (new PtwEntries(num = PtwL3SectorSize, tagLen = PtwL3TagLen, level = 2, hasPerm = true)).genEntries(
          vpn = vpn, data = memRdata, levelUInt = 2.U)
      }\n")
      XSDebug(p"[l3 refill] l3v:${Binary(l3v)} -> ${Binary(l3v | rfvOH)}\n")
      XSDebug(p"[l3 refill] l3g:${Binary(l3g)} -> ${Binary(l3g & ~rfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, rfvOH, 0.U))}\n")

      refillIdx.suggestName(s"l3_refillIdx")
      victimWay.suggestName(s"l3_victimWay")
      victimWayOH.suggestName(s"l3_victimWayOH")
      rfvOH.suggestName(s"l3_rfvOH")
    }

    when ((level === 0.U || level === 1.U) && memPte.isLeaf()) {
      val refillIdx = spreplace.way// LFSR64()(log2Up(PtwSPEntrySize)-1,0) // TODO: may be LRU
      val rfOH = UIntToOH(refillIdx)
      sp(refillIdx).refill(vpn, memSelData, level)
      spreplace.access(refillIdx)
      spv := spv | rfOH
      spg := spg & ~rfOH | Mux(memPte.perm.g, rfOH, 0.U)

      for (i <- 0 until PtwSPEntrySize) {
        spRefillPerf(i) := i.U === refillIdx
      }

      XSDebug(p"[sp refill] refillIdx:${refillIdx} refillEntry:${sp(refillIdx).genPtwEntry(vpn, memSelData, level)}\n")
      XSDebug(p"[sp refill] spv:${Binary(spv)}->${Binary(spv | rfOH)} spg:${Binary(spg)}->${Binary(spg & ~rfOH | Mux(memPte.perm.g, rfOH, 0.U))}\n")

      refillIdx.suggestName(s"sp_refillIdx")
      rfOH.suggestName(s"sp_rfOH")
    }
  }
  
  // sfence
  when (sfence.valid) {
    state := s_idle
    when (state === s_resp && !memRespFire) {
      sfenceLatch := true.B
    }

    when (sfence.bits.rs1/*va*/) {
      when (sfence.bits.rs2) {
        // all va && all asid
        l1v := 0.U
        l2v := 0.U
        l3v := 0.U
        spv := 0.U
      } .otherwise {
        // all va && specific asid except global
        l1v := l1v & l1g
        l2v := l2v & l2g
        l3v := l3v & l3g
        spv := spv & spg
      }
    } .otherwise {
      // val flushMask = UIntToOH(genTlbL2Idx(sfence.bits.addr(sfence.bits.addr.getWidth-1, offLen)))
      val flushSetIdxOH = UIntToOH(genPtwL3SetIdx(sfence.bits.addr(sfence.bits.addr.getWidth-1, offLen)))
      // val flushMask = VecInit(flushSetIdxOH.asBools.map(Fill(PtwL3WayNum, _.asUInt))).asUInt
      val flushMask = VecInit(flushSetIdxOH.asBools.map { a => Fill(PtwL3WayNum, a.asUInt) }).asUInt
      flushSetIdxOH.suggestName(s"sfence_nrs1_flushSetIdxOH")
      flushMask.suggestName(s"sfence_nrs1_flushMask")
      when (sfence.bits.rs2) {
        // specific leaf of addr && all asid
        l3v := l3v & ~flushMask
        l3g := l3g & ~flushMask
      } .otherwise {
        // specific leaf of addr && specific asid
        l3v := l3v & (~flushMask | l3g)
      }
      spv := 0.U
    }
  }

  // Perf Count
  XSPerf("access", validOneCycle)
  XSPerf("l1_hit", l1Hit)
  XSPerf("l2_hit", l2Hit)
  XSPerf("l3_hit", l3Hit)
  XSPerf("sp_hit", spHit)
  XSPerf("pte_hit", pteHit)
  XSPerf("mem_count", memReqFire)
  XSPerf("mem_cycle", BoolStopWatch(memReqFire, memRespFire, true))
  XSPerf("mem_blocked_cycle", mem.a.valid && !memReqReady)
  l1AccessPerf.zipWithIndex.map{ case (l, i) => XSPerf(s"L1AccessIndex${i}", l) }
  l2AccessPerf.zipWithIndex.map{ case (l, i) => XSPerf(s"L2AccessIndex${i}", l) }
  l3AccessPerf.zipWithIndex.map{ case (l, i) => XSPerf(s"L3AccessIndex${i}", l) }
  spAccessPerf.zipWithIndex.map{ case (l, i) => XSPerf(s"SPAccessIndex${i}", l) }
  l1RefillPerf.zipWithIndex.map{ case (l, i) => XSPerf(s"L1RefillIndex${i}", l) }
  l2RefillPerf.zipWithIndex.map{ case (l, i) => XSPerf(s"L2RefillIndex${i}", l) }
  l3RefillPerf.zipWithIndex.map{ case (l, i) => XSPerf(s"L3RefillIndex${i}", l) }
  spRefillPerf.zipWithIndex.map{ case (l, i) => XSPerf(s"SPRefillIndex${i}", l) }


  // debug info
  for (i <- 0 until PtwWidth) {
    XSDebug(p"[io.tlb(${i.U})] ${io.tlb(i)}\n")
  }
  XSDebug(p"[io.sfence] ${io.sfence}\n")
  XSDebug(p"[io.csr] ${io.csr}\n")

  XSDebug(p"req:${req} arb.io.out:(${arb.io.out.valid},${arb.io.out.ready}) arbChosen:${arbChosen} ptwFinish:${ptwFinish}\n")

  XSDebug(p"[mem][A] (${mem.a.valid},${mem.a.ready})\n")
  XSDebug("[mem][A] memAddr:0x${Hexadecimal(memAddr)} l1addr:0x${Hexadecimal(l1addr)} l2addr:0x${Hexadecimal(l2addr)} l3addr:0x${Hexadecimal(l3addr)} memAddrReg:0x${Hexadecimal(memAddrReg)} memPteReg.ppn:0x${Hexadecimal(memPteReg.ppn)}")
  XSDebug(p"[mem][D] (${mem.d.valid},${mem.d.ready}) memSelData:0x${Hexadecimal(memSelData)} memPte:${memPte} memPte.isLeaf:${memPte.isLeaf()} memPte.isPf(${level}):${memPte.isPf(level)}\n")
  XSDebug(memRespFire, p"[mem][D] memPtes:${printVec(memPtes)}\n")

  XSDebug(p"[fsm] state:${state} level:${level} pteHit:${pteHit} sfenceLatch:${sfenceLatch} notFound:${notFound}\n")
  XSDebug(sfence.valid, p"[sfence] original v and g vector:\n")
  XSDebug(sfence.valid, p"[sfence] l1v:${Binary(l1v)}\n")
  XSDebug(sfence.valid, p"[sfence] l2v:${Binary(l2v)}\n")
  XSDebug(sfence.valid, p"[sfence] l3v:${Binary(l3v)}\n")
  XSDebug(sfence.valid, p"[sfence] l3g:${Binary(l3g)}\n")
  XSDebug(sfence.valid, p"[sfence] spv:${Binary(spv)}\n")
  XSDebug(RegNext(sfence.valid), p"[sfence] new v and g vector:\n")
  XSDebug(RegNext(sfence.valid), p"[sfence] l1v:${Binary(l1v)}\n")
  XSDebug(RegNext(sfence.valid), p"[sfence] l2v:${Binary(l2v)}\n")
  XSDebug(RegNext(sfence.valid), p"[sfence] l3v:${Binary(l3v)}\n")
  XSDebug(RegNext(sfence.valid), p"[sfence] l3g:${Binary(l3g)}\n")
  XSDebug(RegNext(sfence.valid), p"[sfence] spv:${Binary(spv)}\n")
}

class PTWRepeater extends XSModule with HasXSParameter with HasXSLog with HasPtwConst {
  val io = IO(new Bundle {
    val tlb = Flipped(new TlbPtwIO)
    val ptw = new TlbPtwIO
    val sfence = Input(new SfenceBundle)
  })

  val (tlb, ptw, sfence) = (io.tlb, io.ptw, io.sfence.valid)
  val req = RegEnable(tlb.req.bits, tlb.req.fire())
  val resp = RegEnable(ptw.resp.bits, ptw.resp.fire())
  val haveOne = BoolStopWatch(tlb.req.fire(), tlb.resp.fire() || sfence)
  val sent = BoolStopWatch(ptw.req.fire(), tlb.req.fire() || sfence)
  val recv = BoolStopWatch(ptw.resp.fire(), tlb.req.fire() || sfence)

  tlb.req.ready := !haveOne
  ptw.req.valid := haveOne && !sent
  ptw.req.bits := req

  tlb.resp.bits := resp
  tlb.resp.valid := haveOne && recv
  ptw.resp.ready := !recv

  XSDebug(haveOne, p"haveOne:${haveOne} sent:${sent} recv:${recv} sfence:${sfence} req:${req} resp:${resp}")
  XSDebug(io.tlb.req.valid || io.tlb.resp.valid, p"tlb: ${tlb}\n")
  XSDebug(io.ptw.req.valid || io.ptw.resp.valid, p"ptw: ${ptw}\n")
  assert(!RegNext(recv && io.ptw.resp.valid), "re-receive ptw.resp")
}
