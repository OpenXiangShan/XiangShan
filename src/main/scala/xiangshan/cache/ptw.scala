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
  val ptwl2Replacer = Some("random")
  def ptwl2replace = ReplacementPolicy.fromString(ptwl2Replacer,PtwL2WayNum,PtwL2LineNum)

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
  def ptwl3replace = ReplacementPolicy.fromString(ptwl3Replacer,PtwL3WayNum,PtwL3LineNum)

  // super page, including 1GB and 2MB page
  val SPTagLen = vpnnLen * 2

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

  // def getLeafEntry(vpn: UInt): PtwEntry = {
  //   require(hasPerm)
  //   val e = Wire(new PtwEntry(tagLen, hasPerm, true))
  //   e.tag := tagClip(vpn)
  //   e.ppn := ppns(sectorIdxClip(vpn, level))
  //   e.perm.map(_ := perms(sectorIdxClip(vpn, level)))
  //   e.level.map(_ := level.U)
  //   e
  // }
  // def get(vpn: UInt) = {
  //   val secIdx = sectorIdxClip(vpn, )
  //   (ppns())
  // }

  override def cloneType: this.type = (new PtwEntries(num, tagLen, level, hasPerm)).asInstanceOf[this.type]
  override def toPrintable: Printable = {
    require(num == 4, "if num is not 4, please comment this toPrintable")
    // NOTE: if num is not 4, please comment this toPrintable
    val permsInner = perms.getOrElse(0.U.asTypeOf(Vec(num, new PtePermBundle)))
    p"tag:${Hexadecimal(tag)} ppn(0):${Hexadecimal(ppns(0))} ppn(1):${Hexadecimal(ppns(1))} " +
    p"ppn(2):${Hexadecimal(ppns(2))} ppn(3):${Hexadecimal(ppns(3))} vs:${Binary(vs.asUInt)} " +
    (if (hasPerm) p"perms(0):${permsInner(0)} perms(1):${permsInner(1)} perms(2):${permsInner(2)} perms(3):${permsInner(3)}" else p"")
  }
}

// class L2TlbEntry extends TlbBundle {
//   val tag = UInt(vpnLen.W) // tag is vpn
//   val level = UInt(log2Up(Level).W) // 2 for 4KB, 1 for 2MB, 0 for 1GB
//   val ppn = UInt(ppnLen.W)
//   val perm = new PtePermBundle

//   def hit(vpn: UInt):Bool = {
//     val fullMask = VecInit((Seq.fill(vpnLen)(true.B))).asUInt
//     val maskLevel = VecInit((Level-1 to 0 by -1).map{i => // NOTE: level 2 for 4KB, 1 for 2MB, 0 for 1GB
//       Reverse(VecInit(Seq.fill(vpnLen-i*vpnnLen)(true.B) ++ Seq.fill(i*vpnnLen)(false.B)).asUInt)})
//     val mask = maskLevel(level)
//     (mask&this.tag) === (mask&vpn)
//   }

//   def apply(pte: UInt, level: UInt, vpn: UInt) = {
//     this.tag := vpn
//     this.level := level
//     this.ppn := pte.asTypeOf(pteBundle).ppn
//     this.perm := pte.asTypeOf(pteBundle).perm
//     this
//   }

//   override def toPrintable: Printable = {
//     p"vpn:0x${Hexadecimal(tag)} level:${level} ppn:${Hexadecimal(ppn)} perm:${perm}"
//   }
// }

// class L2TlbEntires(num: Int, tagLen: Int) extends TlbBundle {
//   require(log2Up(num)==log2Down(num))
//   /* vpn can be divide into three part */
//   // vpn: tagPart(17bit) + addrPart(8bit) + cutLenPart(2bit)
//   val cutLen  = log2Up(num)

//   val tag     = UInt(tagLen.W) // NOTE: high part of vpn
//   val ppns    = Vec(num, UInt(ppnLen.W))
//   val perms    = Vec(num, new PtePermBundle)
//   val vs      = Vec(num, Bool())
//   // println(s"L2TlbEntries: tag:1*${tagLen} ppns:${num}*${ppnLen} perms:${num}*${(new PtePermBundle).asUInt.getWidth} vs:${num}*1")

//   def tagClip(vpn: UInt) = { // full vpn => tagLen
//     vpn(vpn.getWidth-1, vpn.getWidth-tagLen)
//   }

//   // NOTE: get insize idx
//   def idxClip(vpn: UInt) = {
//     vpn(cutLen-1, 0)
//   }

//   def hit(vpn: UInt) = {
//     (tag === tagClip(vpn)) && vs(idxClip(vpn))
//   }

//   def genEntries(data: UInt, level: UInt, vpn: UInt): L2TlbEntires = {
//     require((data.getWidth / XLEN) == num,
//       "input data length must be multiple of pte length")
//     assert(level===2.U, "tlb entries only support 4K pages")

//     val ts = Wire(new L2TlbEntires(num, tagLen))
//     ts.tag := tagClip(vpn)
//     for (i <- 0 until num) {
//       val pte = data((i+1)*XLEN-1, i*XLEN).asTypeOf(new PteBundle)
//       ts.ppns(i) := pte.ppn
//       ts.perms(i):= pte.perm // this.perms has no v
//       ts.vs(i)   := !pte.isPf(level) && pte.isLeaf() // legal and leaf, store to l2Tlb
//     }

//     ts
//   }

//   def get(vpn: UInt): L2TlbEntry = {
//     val t = Wire(new L2TlbEntry)
//     val idx = idxClip(vpn)
//     t.tag := vpn // Note: Use input vpn, not vpn in TlbL2
//     t.level := 2.U // L2TlbEntries only support 4k page
//     t.ppn := ppns(idx)
//     t.perm := perms(idx)
//     t
//   }

//   override def cloneType: this.type = (new L2TlbEntires(num, tagLen)).asInstanceOf[this.type]
//   override def toPrintable: Printable = {
//     require(num == 4, "if num is not 4, please comment this toPrintable")
//     // NOTE: if num is not 4, please comment this toPrintable
//     p"tag:${Hexadecimal(tag)} ppn(0):${Hexadecimal(ppns(0))} ppn(1):${Hexadecimal(ppns(1))}" +
//     p"ppn(2):${Hexadecimal(ppns(2))} ppn(3):${Hexadecimal(ppns(3))} " +
//     p"perms(0):${perms(0)} perms(1):${perms(1)} perms(2):${perms(2)} perms(3):${perms(3)} vs:${Binary(vs.asUInt)}"
//   }
// }

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

// class PTWImp(outer: PTW) extends PtwModule(outer){

//   val (mem, edge) = outer.node.out.head
//   require(mem.d.bits.data.getWidth == l1BusDataWidth, "PTW: tilelink width does not match")

//   val io = IO(new PtwIO)

//   val arb = Module(new Arbiter(new PtwReq, PtwWidth))
//   arb.io.in <> VecInit(io.tlb.map(_.req))
//   val arbChosen = RegEnable(arb.io.chosen, arb.io.out.fire())
//   val req = RegEnable(arb.io.out.bits, arb.io.out.fire())
//   val resp  = VecInit(io.tlb.map(_.resp))

//   val valid = ValidHold(arb.io.out.fire(), resp(arbChosen).fire())
//   val validOneCycle = OneCycleValid(arb.io.out.fire())
//   arb.io.out.ready := !valid// || resp(arbChosen).fire()

//   val sfence = io.sfence
//   val csr    = io.csr
//   val satp   = csr.satp
//   val priv   = csr.priv

//   // two level: l2-tlb-cache && pde/pte-cache
//   // l2-tlb-cache is ram-larger-edition tlb
//   // pde/pte-cache is cache of page-table, speeding up ptw
//   val tlbl2 = Module(new SRAMTemplate(
//     new L2TlbEntires(num = PtwL3SectorSize, tagLen = PtwL3TagLen),
//     set = PtwL3LineNum,
//     way = TlbL2WayNum,
//     singlePort = true
//   )) // (total 256, one line is 4 => 64 lines)
//   val tlbv  = RegInit(0.U(PtwL3LineNum.W)) // valid
//   val tlbg  = Reg(UInt(PtwL3LineNum.W)) // global

//   val sp = Reg(Vec(PtwSPEntrySize, new L2TlbEntry)) // (total 16, one is 4M or 1G)
//   val spv = RegInit(0.U(PtwSPEntrySize.W))
//   val spg = Reg(UInt(PtwSPEntrySize.W))

//   val ptwl1 = Reg(Vec(PtwL1EntrySize, new PtwEntry(tagLen = PtwL1TagLen)))
//   val l1v   = RegInit(0.U(PtwL1EntrySize.W)) // valid
//   val l1g   = Reg(UInt(PtwL1EntrySize.W))
//   val ptwl2 = Module(new SRAMTemplate(
//     new PtwEntries(num = PtwL2SectorSize, tagLen = PtwL2TagLen),
//     set = PtwL2LineNum,
//     way = PtwL2WayNum,
//     singlePort = true
//   )) // (total 256, one line is 4 => 64 lines)
//   val l2v   = RegInit(0.U(PtwL2LineNum.W)) // valid
//   val l2g   = Reg(UInt(PtwL2LineNum.W)) // global

//   // mem alias
//   val memRdata = mem.d.bits.data
//   val memSelData = Wire(UInt(XLEN.W))
//   val memPte   = memSelData.asTypeOf(new PteBundle)
//   val memPtes  =(0 until PtwL3SectorSize).map(i => memRdata((i+1)*XLEN-1, i*XLEN).asTypeOf(new PteBundle))
//   val memValid = mem.d.valid
//   val memRespReady = mem.d.ready
//   val memRespFire = mem.d.fire()
//   val memReqReady = mem.a.ready
//   val memReqFire = mem.a.fire()

//   // fsm
//   val s_idle :: s_req :: s_wait_resp :: s_wait_ready :: Nil = Enum(4)
//   val state = RegInit(s_idle)
//   val level = RegInit(0.U(2.W)) // 0/1/2
//   val levelNext = level + 1.U
//   val latch = Reg(new PtwResp)
//   val sfenceLatch = RegEnable(false.B, init = false.B, memValid) // NOTE: store sfence to disable mem.resp.fire(), but not stall other ptw req

//   /*
//    * tlbl2
//    */
//   val (tlbHit, tlbHitData) = {

//     val ridx = genTlbL2Idx(req.vpn)
//     val ridxReg = RegNext(ridx)
//     val vidx = RegEnable(tlbv(ridx), validOneCycle)
//     tlbl2.io.r.req.valid := validOneCycle
//     tlbl2.io.r.req.bits.apply(setIdx = ridx)
//     val ramDatas = tlbl2.io.r.resp.data
//     val hitVec = VecInit(ramDatas.map{wayData => wayData.hit(req.vpn) })
//     val hitWayData = Mux1H(PriorityEncoderOH(hitVec), ramDatas)

//     when(hitVec.asUInt.orR && vidx) {ptwl3replace.access(ridxReg.asUInt,OHToUInt(hitVec))}


//     assert(tlbl2.io.r.req.ready || !tlbl2.io.r.req.valid)
//     XSDebug(tlbl2.io.r.req.valid, p"tlbl2 Read rIdx:${Hexadecimal(ridx)}\n")
//     XSDebug(RegNext(tlbl2.io.r.req.valid), p"tlbl2 hitWayData:${hitWayData}")
//     XSDebug(RegNext(tlbl2.io.r.req.valid), p"tlbl2 v:${vidx} hit:${hitWayData.hit(req.vpn)} tlbPte:${hitWayData.get(req.vpn)}\n")

//     val spHitVec = sp.zipWithIndex.map{ case (a,i) =>
//       RegEnable(a.hit(req.vpn) && spv(i), validOneCycle)
//     }
//     val spHitData = ParallelMux(spHitVec zip sp)
//     val spHit = Cat(spHitVec).orR

//     XSDebug(RegNext(validOneCycle), p"tlbl2 sp: spHit:${spHit} spPte:${spHitData}\n")

//     assert(RegNext(!(hitVec.asUInt.orR && vidx && spHit && RegNext(validOneCycle))), "pages should not be normal page and super page as well")

//     (hitVec.asUInt.orR && vidx || spHit, Mux(spHit, spHitData, hitWayData.get(req.vpn)))
//   }

//   /*
//    * ptwl1
//    */
//   val l1addr = MakeAddr(satp.ppn, getVpnn(req.vpn, 2))
//   val (l1Hit, l1HitData) = {
//     val hitVecT = ptwl1.zipWithIndex.map{case (a,b) => a.hit(l1addr) && l1v(b) }
//     val hitVec  = hitVecT.map(RegEnable(_, validOneCycle))
//     val hitData = ParallelMux(hitVec zip ptwl1)
//     val hit     = ParallelOR(hitVec).asBool
//     (hit, hitData)
//   }

//   /*
//    * ptwl2
//    */
//   val l1MemBack = memRespFire && state===s_wait_resp && level===0.U
//   val l1Res = Mux(l1Hit, l1HitData.ppn, RegEnable(memPte.ppn, l1MemBack))
//   val l2addr = MakeAddr(l1Res, getVpnn(req.vpn, 1))
//   val (l2Hit, l2HitPPN) = {
//     val readRam = (!tlbHit && l1Hit && level===0.U && state===s_req) || (memRespFire && state===s_wait_resp && level===0.U)
//     val ridx = genPtwL2SetIdx(l2addr)
//     val ridxReg = RegNext(ridx)
//     val idx  = RegEnable(l2addr(log2Up(PtwL2SectorSize)+log2Up(XLEN/8)-1, log2Up(XLEN/8)), readRam)
//     val vidx = RegEnable(l2v(ridx), readRam)

//     assert(ptwl2.io.r.req.ready || !readRam)
//     ptwl2.io.r.req.valid := readRam
//     ptwl2.io.r.req.bits.apply(setIdx = ridx)
//     val ramDatas = ptwl2.io.r.resp.data
//     val hitVec = VecInit(ramDatas.map{wayData => wayData.hit(idx, l2addr) })
//     val hitWayData = Mux1H(PriorityEncoderOH(hitVec), ramDatas)

//     when(hitVec.asUInt.orR && vidx) {ptwl2replace.access(ridxReg.asUInt,OHToUInt(hitVec))}

//     XSDebug(ptwl2.io.r.req.valid, p"ptwl2 rIdx:${Hexadecimal(ridx)}\n")
//     XSDebug(RegNext(ptwl2.io.r.req.valid), p"ptwl2 RamData:${hitWayData}\n")
//     XSDebug(RegNext(ptwl2.io.r.req.valid), p"ptwl2 v:${vidx} hit:${hitWayData.hit(idx, l2addr)}\n")
//     (hitVec.asUInt.orR && vidx, hitWayData.get(idx)._2) // TODO: optimize tag
//   }

//   /* ptwl3
//    * ptwl3 has not cache
//    * ptwl3 may be functional conflict with l2-tlb
//    * if l2-tlb does not hit, ptwl3 would not hit (mostly)
//    */
//   val l2MemBack = memRespFire && state===s_wait_resp && level===1.U
//   val l2Res = Mux(l2Hit, l2HitPPN, RegEnable(memPte.ppn, l2MemBack))
//   val l3addr = MakeAddr(l2Res, getVpnn(req.vpn, 0))

//   /*
//    * fsm
//    */
//   assert(!(tlbHit && (mem.a.valid || state===s_wait_resp))) // when tlb hit, should not req/resp.valid

//   val notFound = WireInit(false.B)
//   switch (state) {
//     is (s_idle) {
//       when (valid) {
//         state := s_req
//         level := 0.U
//       }
//     }

//     is (s_req) {
//       when (tlbHit) {
//         when (resp(arbChosen).ready) {
//           state := s_idle
//         }.otherwise {
//           state := s_wait_ready
//         }
//       } .elsewhen (l1Hit && level===0.U || l2Hit && level===1.U) {
//         level := levelNext
//       } .elsewhen (memReqReady && !sfenceLatch) {
//         state := s_wait_resp
//       }
//     }

//     is (s_wait_resp) {
//       when (memRespFire) {
//         when (memPte.isLeaf() || memPte.isPf(level)) {
//           when (resp(arbChosen).ready) {
//             state := s_idle
//           }.otherwise {
//             state := s_wait_ready
//             latch.entry := Wire(new L2TlbEntry()).apply(memRdata, level, req.vpn)
//             latch.pf := memPte.isPf(level)
//           }
//         }.otherwise {
//           level := levelNext
//           when (level=/=2.U) {
//             state := s_req
//           } .otherwise {
//             notFound := true.B
//             when (resp(arbChosen).ready) {
//               state := s_idle
//             } .otherwise {
//               state := s_wait_ready
//             }
//           }
//         }
//       }
//     }

//     is (s_wait_ready) {
//       when (resp(arbChosen).ready) {
//         state := s_idle
//       }
//     }
//   }

//   /*
//    * mem
//    */
//   val memAddr =  Mux(level===0.U, l1addr/*when l1Hit, DontCare, when l1miss, l1addr*/,
//                  Mux(level===1.U, Mux(l2Hit, l3addr, l2addr)/*when l2Hit, l3addr, when l2miss, l2addr*/, l3addr))
//   val pteRead =  edge.Get(
//     fromSource = 0.U/*id*/,
//     // toAddress  = memAddr(log2Up(CacheLineSize / 2 / 8) - 1, 0),
//     toAddress  = Cat(memAddr(PAddrBits - 1, log2Up(l1BusDataWidth/8)), 0.U(log2Up(l1BusDataWidth/8).W)),
//     lgSize     = log2Up(l1BusDataWidth/8).U
//   )._2
//   mem.a.bits  := pteRead
//   mem.a.valid := state === s_req &&
//                ((level===0.U && !tlbHit && !l1Hit) ||
//                 (level===1.U && !l2Hit) ||
//                 (level===2.U)) && !sfenceLatch && !sfence.valid
//   mem.d.ready := state === s_wait_resp || sfenceLatch

//   val memAddrLatch = RegEnable(memAddr, mem.a.valid)
//   memSelData := memRdata.asTypeOf(Vec(MemBandWidth/XLEN, UInt(XLEN.W)))(memAddrLatch(log2Up(l1BusDataWidth/8) - 1, log2Up(XLEN/8)))

//   /*
//    * resp
//    */
//   val ptwFinish = (state===s_req && tlbHit && level===0.U) ||
//                   ((memPte.isLeaf() || memPte.isPf(level) ||
//                   (!memPte.isLeaf() && level===2.U)) && memRespFire && !sfenceLatch) ||
//                   state===s_wait_ready
//   for(i <- 0 until PtwWidth) {
//     resp(i).valid := valid && arbChosen===i.U && ptwFinish // TODO: add resp valid logic
//     resp(i).bits.entry := Mux(tlbHit, tlbHitData,
//       Mux(state===s_wait_ready, latch.entry, Wire(new L2TlbEntry()).apply(memSelData, Mux(level===3.U, 2.U, level), req.vpn)))
//     resp(i).bits.pf  := Mux(level===3.U || notFound, true.B, Mux(tlbHit, false.B, Mux(state===s_wait_ready, latch.pf, memPte.isPf(level))))
//     // TODO: the pf must not be correct, check it
//   }

//   /*
//    * refill
//    */
//   ptwl2.io.w.req <> DontCare
//   tlbl2.io.w.req <> DontCare
//   ptwl2.io.w.req.valid := false.B
//   tlbl2.io.w.req.valid := false.B
//   assert(!memRespFire || (state===s_wait_resp || sfenceLatch))
//   when (memRespFire && !memPte.isPf(level) && !sfenceLatch) {
//     when (level===0.U && !memPte.isLeaf) {
//       val refillIdx = LFSR64()(log2Up(PtwL1EntrySize)-1,0) // TODO: may be LRU
//       val rfOH = UIntToOH(refillIdx)
//       ptwl1(refillIdx).refill(l1addr, memSelData)
//       l1v := l1v | rfOH
//       l1g := (l1g & ~rfOH) | Mux(memPte.perm.g, rfOH, 0.U)
//     }
//     when (level===1.U && !memPte.isLeaf) {
//       val l2addrStore = RegEnable(l2addr, memReqFire && state===s_req && level===1.U)
//       val refillIdx = genPtwL2SetIdx(l2addrStore) //getVpnn(req.vpn, 1)(log2Up(PtwL2EntrySize)-1, 0)
//       val rfOH = UIntToOH(refillIdx)
//       // replacement policy
//       val victimWayOH = UIntToOH(ptwl2replace.way(refillIdx))
//       //TODO: check why the old refillIdx is right

//       assert(ptwl2.io.w.req.ready)
//       val ps = new PtwEntries(PtwL2SectorSize, PtwL2TagLen).genEntries(l2addrStore, memRdata, level)
//       ptwl2.io.w.apply(
//         valid = true.B,
//         setIdx = refillIdx,
//         data = ps,
//         waymask = victimWayOH
//       )
//       l2v := l2v | rfOH
//       l2g := (l2g & ~rfOH) | Mux(Cat(memPtes.map(_.perm.g)).andR, rfOH, 0.U)
//       XSDebug(p"ptwl2 RefillIdx:${Hexadecimal(refillIdx)} ps:${ps}\n")
//     }
//     when (memPte.isLeaf() && (level===2.U)) {
//       val refillIdx = genTlbL2Idx(req.vpn)//getVpnn(req.vpn, 0)(log2Up(PtwL3EntrySize)-1, 0)
//       val rfOH = UIntToOH(refillIdx)
//       // replacement policy
//       val victimWayOH = UIntToOH(ptwl3replace.way(refillIdx))
//       //TODO: check why the old refillIdx is right

//       assert(tlbl2.io.w.req.ready)
//       val ts = new L2TlbEntires(num = PtwL3SectortorSize, tagLen = PtwL3TagLen).genEntries(memRdata, level, req.vpn)
//       tlbl2.io.w.apply(
//         valid = true.B,
//         setIdx = refillIdx,
//         data = ts,
//         //waymask = -1.S.asUInt
//         waymask = victimWayOH
//       )
//       tlbv := tlbv | rfOH
//       tlbg := (tlbg & ~rfOH) | Mux(Cat(memPtes.map(_.perm.g)).andR, rfOH, 0.U)
//       XSDebug(p"tlbl2 refillIdx:${Hexadecimal(refillIdx)} ts:${ts}\n")
//     }
//     when (memPte.isLeaf() && (level===1.U || level===0.U)) {
//       val refillIdx = LFSR64()(log2Up(PtwSPEntrySize)-1,0) // TODO: may be LRU
//       val rfOH = UIntToOH(refillIdx)
//       sp(refillIdx) := Wire(new L2TlbEntry()).apply(memSelData, Mux(level===3.U, 2.U, level), req.vpn)
//       spv := spv | rfOH
//       spg := (spg & ~rfOH) | Mux(memPte.perm.g, rfOH, 0.U)
//     }
//   }

//   /* sfence
//    * for ram is syncReadMem, so could not flush conditionally
//    * l3 may be conflict with l2tlb??, may be we could combine l2-tlb with l3-ptw
//    */
//   when (sfence.valid) { // TODO: flush optionally
//     valid := false.B
//     state := s_idle
//     when (state===s_wait_resp && !memRespFire) {
//       sfenceLatch := true.B // NOTE: every req need a resp
//     }

//     when (sfence.bits.rs1/*va*/) {
//       when (sfence.bits.rs2) {
//         // all va && all asid
//         tlbv := 0.U
//         spv := 0.U
//         // tlbg := 0.U
//         l1v  := 0.U
//         l2v  := 0.U
//         // l2g  := 0.U
//       } .otherwise {
//         // all va && specific asid except global
//         tlbv := tlbv & tlbg
//         spv  := spv  & spg
//         l1v  := l1v  & l1g
//         l2v  := l2v  & l2g
//       }
//     } .otherwise {
//       val sfenceTlbL2IdxOH = UIntToOH(genTlbL2Idx(sfence.bits.addr(sfence.bits.addr.getWidth-1, offLen)))
//       when (sfence.bits.rs2) {
//         // specific leaf of addr && all asid
//         tlbv := tlbv & ~sfenceTlbL2IdxOH
//         tlbg := tlbg & ~sfenceTlbL2IdxOH
//       } .otherwise {
//         // specific leaf of addr && specific asid
//         tlbv := tlbv & (~sfenceTlbL2IdxOH| tlbg)
//       }
//       spv := 0.U
//     }
//   }

//   if (!env.FPGAPlatform) {
//     ExcitingUtils.addSource(validOneCycle, "perfCntPtwReqCnt", Perf)
//     ExcitingUtils.addSource(valid, "perfCntPtwCycleCnt", Perf)
//     ExcitingUtils.addSource(valid && tlbHit && state===s_req && level===0.U, "perfCntPtwL2TlbHit", Perf)
//   }

//   assert(level=/=3.U)

//   def PrintFlag(en: Bool, flag: Bool, nameEnable: String, nameDisable: String): Unit = {
//     when(flag) {
//       XSDebug(false, en, nameEnable)
//     }.otherwise {
//       XSDebug(false, en, nameDisable)
//     }
//   }

//   XSDebug(validOneCycle, "**New Ptw Req from ")
//   PrintFlag(validOneCycle, arbChosen===0.U, "DTLB**:", "ITLB**:")
//   XSDebug(false, validOneCycle, p"(v:${validOneCycle} r:${arb.io.out.ready}) vpn:0x${Hexadecimal(req.vpn)}\n")
//   XSDebug(resp(arbChosen).fire(), "**Ptw Resp to ")
//   PrintFlag(resp(arbChosen).fire(), arbChosen===0.U, "DTLB**:\n", "ITLB**\n")
//   XSDebug(resp(arbChosen).fire(), p"(v:${resp(arbChosen).valid} r:${resp(arbChosen).ready})" +
//     p" entry:${resp(arbChosen).bits.entry} pf:${resp(arbChosen).bits.pf}\n")

//   XSDebug(sfence.valid, p"Sfence: sfence instr here ${sfence.bits}\n")
//   XSDebug(valid, p"CSR: ${csr}\n")

//   XSDebug(valid, p"vpn2:0x${Hexadecimal(getVpnn(req.vpn, 2))} vpn1:0x${Hexadecimal(getVpnn(req.vpn, 1))}" +
//     p" vpn0:0x${Hexadecimal(getVpnn(req.vpn, 0))}\n")
//   XSDebug(valid, p"state:${state} level:${level} tlbHit:${tlbHit} l1addr:0x${Hexadecimal(l1addr)} l1Hit:${l1Hit}" +
//     p" l2addr:0x${Hexadecimal(l2addr)} l2Hit:${l2Hit} l3addr:0x${Hexadecimal(l3addr)} memReq(v:${mem.a.valid} r:${mem.a.ready})\n")

//   XSDebug(memReqFire, p"mem req fire addr:0x${Hexadecimal(memAddr)}\n")
//   XSDebug(memRespFire, p"mem resp fire: \n")
//   for(i <- 0 until (MemBandWidth/XLEN)) {
//     XSDebug(memRespFire, p"            ${i.U}: ${memPtes(i)} isPf:${memPtes(i).isPf(level)} isLeaf:${memPtes(i).isLeaf}\n")
//   }

//   XSDebug(sfenceLatch, p"ptw has a flushed req waiting for resp... " +
//     p"state:${state} mem.a(${mem.a.valid} ${mem.a.ready}) d($memValid} ${memRespReady})\n")

//   // TODO: add ptw perf cnt
// }


class PTWImp(outer: PTW) extends PtwModule(outer) {

  val (mem, edge) = outer.node.out.head
  require(mem.d.bits.data.getWidth == l1BusDataWidth, "PTW: tilelink width does not match")

  val io = IO(new PtwIO)
  val arb = Module(new Arbiter(new PtwReq, PtwWidth))
  arb.io.in <> VecInit(io.tlb.map(_.req))
  val arbChosen = RegEnable(arb.io.chosen, arb.io.out.fire())
  val req = RegEnable(arb.io.out.bits, arb.io.out.fire())
  val resp  = VecInit(io.tlb.map(_.resp))
  val vpn = req.vpn

  val valid = ValidHold(arb.io.out.fire(), resp(arbChosen).fire())
  val validOneCycle = OneCycleValid(arb.io.out.fire())
  arb.io.out.ready := !valid// || resp(arbChosen).fire()

  val sfence = io.sfence
  val csr    = io.csr
  val satp   = csr.satp
  val priv   = csr.priv

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
  val latch = Reg(new PtwResp)
  val sfenceLatch = RegEnable(false.B, init = false.B, memValid) // NOTE: store sfence to disable mem.resp.fire(), but not stall other ptw req


  // l1
  val l1HitReg = Reg(Bool())
  val l1HitPPNReg = Reg(UInt(ppnLen.W))
  val (l1Hit, l1HitPPN) = {
    val hitVecT = l1.zipWithIndex.map { case (e, i) => e.hit(vpn) && l1v(i) }
    val hitVec = hitVecT.map(RegEnable(_, validOneCycle))
    val hitPPN = ParallelPriorityMux(hitVec zip l1.map(_.ppn))
    val hit = ParallelOR(hitVec)
    (hit, hitPPN)
  }

  // l2
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
    val hit = ParallelOR(hitVec)
    val hitWay = ParallelPriorityMux(hitVec zip (0 until PtwL2WayNum).map(_.U))

    when (hit) { ptwl2replace.access(genPtwL2SetIdx(vpn), hitWay) }

    (hit, hitWayData.ppns(genPtwL2SectorIdx(vpn)))
  }

  // l3
  val l3HitReg = Reg(Bool())
  val (l3Hit, l3HitData) = {
    val ridx = genPtwL3SetIdx(vpn)
    val vidx = RegEnable(VecInit(getl3vSet(vpn).asBools), validOneCycle)
    l3.io.r.req.valid := validOneCycle
    l3.io.r.req.bits.apply(setIdx = ridx)
    val ramDatas = l3.io.r.resp.data
    val hitVec = VecInit(ramDatas.zip(vidx).map{ case (wayData, v) => wayData.hit(vpn) && v })
    val hitWayData = ParallelPriorityMux(hitVec zip ramDatas)
    val hit = ParallelOR(hitVec)
    val hitWay = ParallelPriorityMux(hitVec zip (0 until PtwL3WayNum).map(_.U))

    when (hit) { ptwl3replace.access(genPtwL3SetIdx(vpn), hitWay) }

    (hit, hitWayData)
  }
  val l3HitPPN = l3HitData.ppns(genPtwL3SectorIdx(vpn))
  val l3HitPerm = l3HitData.perms.getOrElse(0.U.asTypeOf(Vec(PtwL3SectorSize, new PtePermBundle)))(genPtwL3SectorIdx(vpn))

  // super page
  val spHitReg = Reg(Bool())
  val (spHit, spHitData) = {
    val hitVecT = sp.zipWithIndex.map { case (e, i) => e.hit(vpn) && spv(i) }
    val hitVec = hitVecT.map(RegEnable(_, validOneCycle))
    val hitData = ParallelPriorityMux(hitVec zip sp)
    val hit = ParallelOR(hitVec)
    (hit, hitData)
  }
  val spHitPerm = spHitData.perm.getOrElse(0.U.asTypeOf(new PtePermBundle))
  val spHitLevel = spHitData.level.getOrElse(0.U)

  // default values
  resp.map(_.valid := false.B)
  resp.map(_.bits := DontCare)
  l2.io.w.req <> DontCare
  l3.io.w.req <> DontCare
  l2.io.w.req.valid := false.B
  l3.io.w.req.valid := false.B

  // fsm
  val pteHit = l3Hit || spHit
  val notFound = WireInit(false.B)
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
      when (memReqReady && !sfenceLatch) {
        state := s_resp
      }
    }

    is (s_resp) {
      when (memRespFire) {
        when (memPte.isLeaf() || memPte.isPf(level)) {
          state := s_idle
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
  }

  // refill
  when (memRespFire && !memPte.isPf(level) && !sfenceLatch) {
    when (level === 0.U && !memPte.isLeaf()) {
      val refillIdx = LFSR64()(log2Up(PtwL1EntrySize)-1,0) // TODO: may be LRU
      val rfOH = UIntToOH(refillIdx)
      l1(refillIdx).refill(vpn, memSelData)
      l1v := l1v | rfOH
      l1g := (l1g & ~rfOH) | Mux(memPte.perm.g, rfOH, 0.U)
    }

    when (level === 1.U && !memPte.isLeaf()) {
      val refillIdx = genPtwL2SetIdx(vpn)
      val victimWay = ptwl2replace.way(refillIdx)
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
      l2v := l2v | rfvOH
      l2g := l2g & ~rfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, rfvOH, 0.U)
    }

    when (level === 2.U && memPte.isLeaf()) {
      val refillIdx = genPtwL3SetIdx(vpn)
      val victimWay = ptwl3replace.way(refillIdx)
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
      l3v := l3v | rfvOH
      l3g := l3g & ~rfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, rfvOH, 0.U)
    }

    when ((level === 0.U || level === 1.U) && memPte.isLeaf()) {
      val refillIdx = LFSR64()(log2Up(PtwSPEntrySize)-1,0) // TODO: may be LRU
      val rfOH = UIntToOH(refillIdx)
      sp(refillIdx).refill(vpn, memSelData, level)
      spv := spv | rfOH
      spg := spg & ~rfOH | Mux(memPte.perm.g, rfOH, 0.U)
    }
  }

  // sfence
  when (sfence.valid) {
    valid := false.B
    state := s_idle
    when (state === s_resp && memRespFire) {
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

}