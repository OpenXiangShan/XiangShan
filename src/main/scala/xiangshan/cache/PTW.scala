/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._

/* PTW Graph
 * not yet
 */

trait HasPtwConst extends HasTlbConst with MemoryOpConstants{
  val PtwWidth = 2
  val MemBandWidth  = 256 // TODO: change to IO bandwidth param
  val SramSinglePort = true // NOTE: ptwl2, ptwl3 sram single port or not

  val bPtwWidth = log2Up(PtwWidth)

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

  val MSHRSize = PtwMissQueueSize

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

abstract class PtwBundle(implicit p: Parameters) extends XSBundle with HasPtwConst
abstract class PtwModule(outer: PTW) extends LazyModuleImp(outer)
  with HasXSParameter with HasPtwConst

class PteBundle(implicit p: Parameters) extends PtwBundle{
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
    val pm = Wire(new PtePermBundle)
    pm.d := perm.d
    pm.a := perm.a
    pm.g := perm.g
    pm.u := perm.u
    pm.x := perm.x
    pm.w := perm.w
    pm.r := perm.r
    pm
  }

  override def toPrintable: Printable = {
    p"ppn:0x${Hexadecimal(ppn)} perm:b${Binary(perm.asUInt)}"
  }
}

class PtwEntry(tagLen: Int, hasPerm: Boolean = false, hasLevel: Boolean = false)(implicit p: Parameters) extends PtwBundle {
  val tag = UInt(tagLen.W)
  val ppn = UInt(ppnLen.W)
  val perm = if (hasPerm) Some(new PtePermBundle) else None
  val level = if (hasLevel) Some(UInt(log2Up(Level).W)) else None

  def hit(vpn: UInt, allType: Boolean = false) = {
    require(vpn.getWidth == vpnLen)
    if (allType) {
      require(hasLevel)
      val hit0 = tag(tagLen - 1,    vpnnLen*2) === vpn(tagLen - 1, vpnnLen*2)
      val hit1 = tag(vpnnLen*2 - 1, vpnnLen)   === vpn(vpnnLen*2 - 1,  vpnnLen)
      val hit2 = tag(vpnnLen - 1,     0)         === vpn(vpnnLen - 1, 0)
      Mux(level.getOrElse(0.U) === 2.U, hit2 && hit1 && hit0, Mux(level.getOrElse(0.U) === 1.U, hit1 && hit0, hit0))
    } else if (hasLevel) {
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

class PtwEntries(num: Int, tagLen: Int, level: Int, hasPerm: Boolean)(implicit p: Parameters) extends PtwBundle {
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

class PtwReq(implicit p: Parameters) extends PtwBundle {
  val vpn = UInt(vpnLen.W)

  override def toPrintable: Printable = {
    p"vpn:0x${Hexadecimal(vpn)}"
  }
}

class PtwResp(implicit p: Parameters) extends PtwBundle {
  val entry = new PtwEntry(tagLen = vpnLen, hasPerm = true, hasLevel = true)
  val pf  = Bool()

  override def toPrintable: Printable = {
    p"entry:${entry} pf:${pf}"
  }
}

class PtwIO(implicit p: Parameters) extends PtwBundle {
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

class PTWImp(outer: PTW)(implicit p: Parameters) extends PtwModule(outer) {

  val (mem, edge) = outer.node.out.head
  require(mem.d.bits.data.getWidth == l1BusDataWidth, "PTW: tilelink width does not match")

  val io = IO(new PtwIO)
  val difftestIO = IO(new Bundle() {
    val ptwResp = Output(Bool())
    val ptwAddr = Output(UInt(64.W))
    val ptwData = Output(Vec(4, UInt(64.W)))
  })

  /* Ptw processes multiple requests
   * Divide Ptw procedure into two stages: cache access ; mem access if cache miss
   *           miss queue itlb       dtlb
   *               |       |         |
   *               ------arbiter------
   *                            |
   *                    l1 - l2 - l3 - sp
   *                            |
   *          -------------------------------------------
   *    miss  |  queue                                  | hit
   *    [][][][][][]                                    |
   *          |                                         |
   *    state machine accessing mem                     |
   *          |                                         |
   *          ---------------arbiter---------------------
   *                 |                    |
   *                itlb                 dtlb
   */

  difftestIO <> DontCare

  val sfence = RegNext(io.sfence)
  val csr    = io.csr
  val satp   = csr.satp
  val priv   = csr.priv

  val missQueue = Module(new PtwMissQueue)
  val cache = Module(new PtwCache)
  val fsm = Module(new PtwFsm)
  val arb1 = Module(new Arbiter(new PtwReq, PtwWidth))
  val arb2 = Module(new Arbiter(new Bundle {
    val vpn = UInt(vpnLen.W)
    val source = UInt(bPtwWidth.W)
  }, 2))
  val outArb = (0 until PtwWidth).map(i => Module(new Arbiter(new PtwResp, 2)).io)

  // NOTE: when cache out but miss and fsm doesnt accept,
  val blockNewReq = false.B
  arb1.io.in <> VecInit(io.tlb.map(_.req(0)))
  arb1.io.out.ready := arb2.io.in(1).ready && !blockNewReq

  val blockMissQueue = !fsm.io.req.ready
  block_decoupled(missQueue.io.out, arb2.io.in(0), blockMissQueue)
  arb2.io.in(1).valid := arb1.io.out.valid && !blockNewReq
  arb2.io.in(1).bits.vpn := arb1.io.out.bits.vpn
  arb2.io.in(1).bits.source := arb1.io.chosen
  arb2.io.out.ready := cache.io.req.ready

  cache.io.req.valid := arb2.io.out.valid
  cache.io.req.bits.vpn := arb2.io.out.bits.vpn
  cache.io.req.bits.source := arb2.io.out.bits.source
  cache.io.req.bits.isReplay := arb2.io.chosen === 0.U
  cache.io.refill.valid := mem.d.valid
  cache.io.refill.bits.ptes := mem.d.bits.data
  cache.io.refill.bits.vpn  := fsm.io.refill.vpn
  cache.io.refill.bits.level := fsm.io.refill.level
  cache.io.refill.bits.memAddr := fsm.io.refill.memAddr
  cache.io.sfence := sfence
  cache.io.refuseRefill := fsm.io.sfenceLatch
  cache.io.resp.ready := Mux(cache.io.resp.bits.hit, true.B, missQueue.io.in.ready || fsm.io.req.ready)

  missQueue.io.in.valid := cache.io.resp.valid && !cache.io.resp.bits.hit && !fsm.io.req.ready
  missQueue.io.in.bits.vpn := cache.io.resp.bits.vpn
  missQueue.io.in.bits.source := cache.io.resp.bits.source
  missQueue.io.sfence  := sfence

  // NOTE: missQueue req has higher priority
  fsm.io.req.valid := cache.io.resp.valid && !cache.io.resp.bits.hit
  fsm.io.req.bits.source := cache.io.resp.bits.source
  fsm.io.req.bits.l1Hit := cache.io.resp.bits.toFsm.l1Hit
  fsm.io.req.bits.l2Hit := cache.io.resp.bits.toFsm.l2Hit
  fsm.io.req.bits.ppn := cache.io.resp.bits.toFsm.ppn
  fsm.io.req.bits.vpn := cache.io.resp.bits.vpn
  fsm.io.mem.req.ready := mem.a.ready
  fsm.io.mem.resp.valid := mem.d.valid
  fsm.io.mem.resp.bits.data := mem.d.bits.data
  fsm.io.csr := csr
  fsm.io.sfence := sfence
  fsm.io.resp.ready := MuxLookup(fsm.io.resp.bits.source, false.B,
    (0 until PtwWidth).map(i => i.U -> outArb(i).in(1).ready))

  val memRead =  edge.Get(
    fromSource = 0.U/*id*/,
    // toAddress  = memAddr(log2Up(CacheLineSize / 2 / 8) - 1, 0),
    toAddress  = Cat(fsm.io.mem.req.bits.addr(PAddrBits - 1, log2Up(l1BusDataWidth/8)), 0.U(log2Up(l1BusDataWidth/8).W)),
    lgSize     = log2Up(l1BusDataWidth/8).U
  )._2
  mem.a.bits := memRead
  mem.a.valid := fsm.io.mem.req.valid
  mem.d.ready := true.B

  for (i <- 0 until PtwWidth) {
    outArb(i).in(0).valid := cache.io.resp.valid && cache.io.resp.bits.hit && cache.io.resp.bits.source===i.U
    outArb(i).in(0).bits.entry := cache.io.resp.bits.toTlb
    outArb(i).in(0).bits.pf := false.B
    outArb(i).in(1).valid := fsm.io.resp.valid && fsm.io.resp.bits.source===i.U
    outArb(i).in(1).bits := fsm.io.resp.bits.resp
  }

  // io.tlb.map(_.resp) <> outArb.map(_.out)
  io.tlb.map(_.resp).zip(outArb.map(_.out)).map{
    case (resp, out) => resp <> out
  }
  def block_decoupled[T <: Data](source: DecoupledIO[T], sink: DecoupledIO[T], block_signal: Bool) = {
    sink.valid   := source.valid && !block_signal
    source.ready := sink.ready   && !block_signal
    sink.bits    := source.bits
  }
  // debug info
  for (i <- 0 until PtwWidth) {
    XSDebug(p"[io.tlb(${i.U})] ${io.tlb(i)}\n")
  }
  XSDebug(p"[io.sfence] ${io.sfence}\n")
  XSDebug(p"[io.csr] ${io.csr}\n")

  for (i <- 0 until PtwWidth) {
    XSPerfAccumulate(s"req_count${i}", io.tlb(i).req(0).fire())
    XSPerfAccumulate(s"req_blocked_count_${i}", io.tlb(i).req(0).valid && !io.tlb(i).req(0).ready)
  }
  XSPerfAccumulate(s"req_blocked_by_mq", arb1.io.out.valid && missQueue.io.out.valid)
  XSPerfAccumulate(s"replay_again", cache.io.resp.valid && !cache.io.resp.bits.hit && cache.io.resp.bits.isReplay && !fsm.io.req.ready)
  XSPerfAccumulate(s"into_fsm_no_replay", cache.io.resp.valid && !cache.io.resp.bits.hit && !cache.io.resp.bits.isReplay && fsm.io.req.ready)
}

/* Miss Queue dont care about duplicate req, which is done by PtwFilter
 * PtwMissQueue is just a Queue inside Chisel with flush
 */
class PtwMissQueue(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val vpn = UInt(vpnLen.W)
      val source = UInt(bPtwWidth.W)
    }))
    val sfence = Input(new SfenceBundle)
    val out = Decoupled(new Bundle {
      val vpn = UInt(vpnLen.W)
      val source = UInt(bPtwWidth.W)
    })
    val empty = Output(Bool())
  })

  val vpn = Reg(Vec(MSHRSize, UInt(vpnLen.W))) // request vpn
  val source = Reg(Vec(MSHRSize, UInt(bPtwWidth.W))) // is itlb
  val enqPtr = RegInit(0.U(log2Up(MSHRSize).W))
  val deqPtr = RegInit(0.U(log2Up(MSHRSize).W))

  val mayFull = RegInit(false.B)
  val full = mayFull && enqPtr === deqPtr
  val empty = !mayFull && enqPtr === deqPtr

  val do_enq = io.in.fire()
  val do_deq = io.out.fire()

  when (do_enq) {
    enqPtr := enqPtr + 1.U
    vpn(enqPtr) := io.in.bits.vpn
    source(enqPtr) := io.in.bits.source
  }

  when (do_deq) {
    deqPtr := deqPtr + 1.U
  }

  when (do_enq =/= do_deq) {
    mayFull := do_enq
  }

  when (io.sfence.valid) {
    enqPtr := 0.U
    deqPtr := 0.U
    mayFull := false.B
  }

  io.in.ready := !full
  io.out.valid := !empty
  io.out.bits.vpn := vpn(deqPtr)
  io.out.bits.source := source(deqPtr)
  io.empty := empty

  XSPerfAccumulate("mq_in_count", io.in.fire())
  XSPerfAccumulate("mq_in_block", io.in.valid && !io.in.ready)
  val count = RegInit(0.U(log2Up(MSHRSize+1).W))
  when (do_enq =/= do_deq) {
    count := Mux(do_enq, count + 1.U, count - 1.U)
  }
  when (io.sfence.valid) {
    count := 0.U
  }
  for (i <- 0 until MSHRSize) {
    XSPerfAccumulate(s"numExist${i}", count === i.U)
  }
}

class PTWRepeater(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new Bundle {
    val tlb = Flipped(new TlbPtwIO)
    val ptw = new TlbPtwIO
    val sfence = Input(new SfenceBundle)
  })

  val (tlb, ptw, sfence) = (io.tlb, io.ptw, RegNext(io.sfence.valid))
  val req = RegEnable(tlb.req(0).bits, tlb.req(0).fire())
  val resp = RegEnable(ptw.resp.bits, ptw.resp.fire())
  val haveOne = BoolStopWatch(tlb.req(0).fire(), tlb.resp.fire() || sfence)
  val sent = BoolStopWatch(ptw.req(0).fire(), tlb.req(0).fire() || sfence)
  val recv = BoolStopWatch(ptw.resp.fire(), tlb.req(0).fire() || sfence)

  tlb.req(0).ready := !haveOne
  ptw.req(0).valid := haveOne && !sent
  ptw.req(0).bits := req

  tlb.resp.bits := resp
  tlb.resp.valid := haveOne && recv
  ptw.resp.ready := !recv

  XSPerfAccumulate("req_count", ptw.req(0).fire())
  XSPerfAccumulate("tlb_req_cycle", BoolStopWatch(tlb.req(0).fire(), tlb.resp.fire() || sfence))
  XSPerfAccumulate("ptw_req_cycle", BoolStopWatch(ptw.req(0).fire(), ptw.resp.fire() || sfence))

  XSDebug(haveOne, p"haveOne:${haveOne} sent:${sent} recv:${recv} sfence:${sfence} req:${req} resp:${resp}")
  XSDebug(io.tlb.req(0).valid || io.tlb.resp.valid, p"tlb: ${tlb}\n")
  XSDebug(io.ptw.req(0).valid || io.ptw.resp.valid, p"ptw: ${ptw}\n")
  assert(!RegNext(recv && io.ptw.resp.valid, init = false.B), "re-receive ptw.resp")
}

/* dtlb
 *
 */
class PTWFilter(Width: Int, Size: Int)(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new Bundle {
    val tlb = Flipped(new TlbPtwIO(Width))
    val ptw = new TlbPtwIO
    val sfence = Input(new SfenceBundle)
  })

  require(Size >= Width)

  val v = RegInit(VecInit(Seq.fill(Size)(false.B)))
  val vpn = Reg(Vec(Size, UInt(vpnLen.W)))
  val enqPtr = RegInit(0.U(log2Up(Size).W)) // Enq
  val issPtr = RegInit(0.U(log2Up(Size).W)) // Iss to Ptw
  val deqPtr = RegInit(0.U(log2Up(Size).W)) // Deq
  val mayFullDeq = RegInit(false.B)
  val mayFullIss = RegInit(false.B)
  val counter = RegInit(0.U(log2Up(Size+1).W))

  val sfence = RegNext(io.sfence)
  val ptwResp = RegEnable(io.ptw.resp.bits, io.ptw.resp.fire())
  val ptwResp_valid = RegNext(io.ptw.resp.valid, init = false.B)
  val reqs = filter(io.tlb.req)

  var enqPtr_next = WireInit(deqPtr)
  val isFull = enqPtr === deqPtr && mayFullDeq
  val isEmptyDeq = enqPtr === deqPtr && !mayFullDeq
  val isEmptyIss = enqPtr === issPtr && !mayFullIss
  val accumEnqNum = (0 until Width).map(i => PopCount(reqs.take(i).map(_.valid)))
  val enqPtrVec = VecInit((0 until Width).map(i => enqPtr + accumEnqNum(i)))
  val enqNum = PopCount(reqs.map(_.valid))
  val canEnqueue = counter +& enqNum <= Size.U

  io.tlb.req.map(_.ready := true.B) // NOTE: just drop un-fire reqs
  io.tlb.resp.valid := ptwResp_valid
  io.tlb.resp.bits := ptwResp
  io.ptw.req(0).valid := v(issPtr) && !isEmptyIss && !(ptwResp_valid && ptwResp.entry.hit(io.ptw.req(0).bits.vpn))
  io.ptw.req(0).bits.vpn := vpn(issPtr)
  io.ptw.resp.ready := true.B

  reqs.zipWithIndex.map{
    case (req, i) =>
      when (req.valid && canEnqueue) {
        v(enqPtrVec(i)) := true.B
        vpn(enqPtrVec(i)) := req.bits.vpn
      }
  }

  val do_enq = canEnqueue && Cat(reqs.map(_.valid)).orR
  val do_deq = (!v(deqPtr) && !isEmptyDeq)
  val do_iss = io.ptw.req(0).fire() || (!v(issPtr) && !isEmptyIss)
  when (do_enq) {
    enqPtr := enqPtr + enqNum
  }
  when (do_deq) {
    deqPtr := deqPtr + 1.U
  }
  when (do_iss) {
    issPtr := issPtr + 1.U
  }
  when (do_enq =/= do_deq) {
    mayFullDeq := do_enq
  }
  when (do_enq =/= do_iss) {
    mayFullIss := do_enq
  }

  when (ptwResp_valid) {
    vpn.zip(v).map{case (pi, vi) =>
      when (vi && ptwResp.entry.hit(pi, allType = true)) { vi := false.B }
    }
  }

  counter := counter - do_deq + Mux(do_enq, enqNum, 0.U)
  assert(counter <= Size.U, "counter should be less than Size")
  when (counter === 0.U) {
    assert(!io.ptw.req(0).fire(), "when counter is 0, should not req")
    assert(isEmptyDeq && isEmptyIss, "when counter is 0, should be empty")
  }
  when (counter === Size.U) {
    assert(mayFullDeq, "when counter is Size, should be full")
  }

  when (sfence.valid) {
    v.map(_ := false.B)
    deqPtr := 0.U
    enqPtr := 0.U
    issPtr := 0.U
    ptwResp_valid := false.B
    mayFullDeq := false.B
    mayFullIss := false.B
    counter := 0.U
  }

  def canMerge(vpnReq: UInt, reqs: Seq[DecoupledIO[PtwReq]], index: Int) : Bool = {
    Cat((vpn ++ reqs.take(index).map(_.bits.vpn))
      .zip(v ++ reqs.take(index).map(_.valid))
      .map{case (pi, vi) => vi && pi === vpnReq}
    ).orR || (ptwResp_valid && ptwResp.entry.hit(vpnReq))
  }

  def filter(tlbReq: Vec[DecoupledIO[PtwReq]]) = {
    val reqs =  tlbReq.indices.map{ i =>
      val req = Wire(ValidIO(new PtwReq()))
      req.bits := tlbReq(i).bits
      req.valid := !canMerge(tlbReq(i).bits.vpn, tlbReq, i) && tlbReq(i).valid
      req
    }
    reqs
  }

  // perf
  val inflight_counter = RegInit(0.U(log2Up(Size + 1).W))
  when (io.ptw.req(0).fire() =/= io.ptw.resp.fire()) {
    inflight_counter := Mux(io.ptw.req(0).fire(), inflight_counter + 1.U, inflight_counter - 1.U)
  }
  when (sfence.valid) {
    inflight_counter := 0.U
  }
  XSPerfAccumulate("tlb_req_count", PopCount(Cat(io.tlb.req.map(_.valid))))
  XSPerfAccumulate("tlb_req_count_filtered", Mux(do_enq, accumEnqNum(Width - 1), 0.U))
  XSPerfAccumulate("ptw_req_count", io.ptw.req(0).fire())
  XSPerfAccumulate("ptw_req_cycle", inflight_counter)
  XSPerfAccumulate("tlb_resp_count", io.tlb.resp.fire())
  XSPerfAccumulate("ptw_resp_count", io.ptw.resp.fire())
  XSPerfAccumulate("inflight_cycle", !isEmptyDeq)
  for (i <- 0 until Size + 1) {
    XSPerfAccumulate(s"counter${i}", counter === i.U)
  }

}

/* ptw cache caches the page table of all the three layers
 * ptw cache resp at next cycle
 * the cache should not be blocked
 * when miss queue if full, just block req outside
 */
class PtwCacheIO()(implicit p: Parameters) extends PtwBundle {
  val req = Flipped(DecoupledIO(new Bundle {
    val vpn = UInt(vpnLen.W)
    val source = UInt(bPtwWidth.W)
    val isReplay = Bool()
  }))
  val resp = DecoupledIO(new Bundle {
    val source = UInt(bPtwWidth.W)
    val vpn = UInt(vpnLen.W)
    val isReplay = Bool()
    val hit = Bool()
    val toFsm = new Bundle {
      val l1Hit = Bool()
      val l2Hit = Bool()
      val ppn = UInt(ppnLen.W)
    }
    val toTlb = new PtwEntry(tagLen = vpnLen, hasPerm = true, hasLevel = true)
  })
  val refill = Flipped(ValidIO(new Bundle {
    val ptes = UInt(MemBandWidth.W)
    val vpn = UInt(vpnLen.W)
    val level = UInt(log2Up(Level).W)
    val memAddr = Input(UInt(PAddrBits.W))
  }))
  val sfence = Input(new SfenceBundle)
  val refuseRefill = Input(Bool())
}

class PtwCache()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new PtwCacheIO)

  // TODO: four caches make the codes dirty, think about how to deal with it

  val sfence = io.sfence
  val refuseRefill = io.refuseRefill
  val refill = io.refill.bits

  val first_valid = io.req.valid
  val first_fire = first_valid && io.req.ready
  val first_req = io.req.bits
  val second_ready = Wire(Bool())
  val second_valid = ValidHold(first_fire, io.resp.fire(), sfence.valid)
  val second_req = RegEnable(first_req, first_fire)
  // NOTE: if ptw cache resp may be blocked, hard to handle refill
  // when miss queue is full, please to block itlb and dtlb input

  // when refill, refuce to accept new req
  val rwHarzad = if (SramSinglePort) io.refill.valid else false.B
  io.req.ready := !rwHarzad && (second_ready || io.req.bits.isReplay)
  // NOTE: when write, don't ready, whe
  //       when replay, just come in, out make sure resp.fire()

  // l1: level 0 non-leaf pte
  val l1 = Reg(Vec(PtwL1EntrySize, new PtwEntry(tagLen = PtwL1TagLen)))
  val l1v = RegInit(0.U(PtwL1EntrySize.W))
  val l1g = Reg(UInt(PtwL1EntrySize.W))

  // l2: level 1 non-leaf pte
  val l2 = Module(new SRAMTemplate(
    new PtwEntries(num = PtwL2SectorSize, tagLen = PtwL2TagLen, level = 1, hasPerm = false),
    set = PtwL2LineNum,
    way = PtwL2WayNum,
    singlePort = SramSinglePort
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
    singlePort = SramSinglePort
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
  val (l1Hit, l1HitPPN) = {
    val hitVecT = l1.zipWithIndex.map { case (e, i) => e.hit(first_req.vpn) && l1v(i) }
    val hitVec = hitVecT.map(RegEnable(_, first_fire))
    val hitPPN = ParallelPriorityMux(hitVec zip l1.map(_.ppn))
    val hit = ParallelOR(hitVec) && second_valid

    when (hit) { ptwl1replace.access(OHToUInt(hitVec)) }

    l1AccessPerf.zip(hitVec).map{ case (l, h) => l := h && RegNext(first_fire)}
    for (i <- 0 until PtwL1EntrySize) {
      XSDebug(first_fire, p"[l1] l1(${i.U}) ${l1(i)} hit:${l1(i).hit(first_req.vpn)}\n")
    }
    XSDebug(first_fire, p"[l1] l1v:${Binary(l1v)} hitVecT:${Binary(VecInit(hitVecT).asUInt)}\n")
    XSDebug(second_valid, p"[l1] l1Hit:${hit} l1HitPPN:0x${Hexadecimal(hitPPN)} hitVec:${VecInit(hitVec).asUInt}\n")

    VecInit(hitVecT).suggestName(s"l1_hitVecT")
    VecInit(hitVec).suggestName(s"l1_hitVec")

    (hit, hitPPN)
  }

  // l2
  val ptwl2replace = ReplacementPolicy.fromString(ptwl2Replacer,PtwL2WayNum,PtwL2LineNum)
  val (l2Hit, l2HitPPN) = {
    val ridx = genPtwL2SetIdx(first_req.vpn)
    val vidx = RegEnable(VecInit(getl2vSet(first_req.vpn).asBools), first_fire)
    l2.io.r.req.valid := first_fire
    l2.io.r.req.bits.apply(setIdx = ridx)
    val ramDatas = l2.io.r.resp.data
    // val hitVec = VecInit(ramDatas.map{wayData => wayData.hit(first_req.vpn) })
    val hitVec = VecInit(ramDatas.zip(vidx).map { case (wayData, v) => wayData.hit(second_req.vpn) && v })
    val hitWayData = ParallelPriorityMux(hitVec zip ramDatas)
    val hit = ParallelOR(hitVec) && second_valid
    val hitWay = ParallelPriorityMux(hitVec zip (0 until PtwL2WayNum).map(_.U))

    ridx.suggestName(s"l2_ridx")
    vidx.suggestName(s"l2_vidx")
    ramDatas.suggestName(s"l2_ramDatas")
    hitVec.suggestName(s"l2_hitVec")
    hitWayData.suggestName(s"l2_hitWayData")
    hitWay.suggestName(s"l2_hitWay")

    when (hit) { ptwl2replace.access(genPtwL2SetIdx(second_req.vpn), hitWay) }

    l2AccessPerf.zip(hitVec).map{ case (l, h) => l := h && RegNext(first_fire) }
    XSDebug(first_fire, p"[l2] ridx:0x${Hexadecimal(ridx)}\n")
    for (i <- 0 until PtwL2WayNum) {
      XSDebug(RegNext(first_fire), p"[l2] ramDatas(${i.U}) ${ramDatas(i)}  l2v:${vidx(i)}  hit:${ramDatas(i).hit(second_req.vpn)}\n")
    }
    XSDebug(second_valid, p"[l2] l2Hit:${hit} l2HitPPN:0x${Hexadecimal(hitWayData.ppns(genPtwL2SectorIdx(second_req.vpn)))} hitVec:${Binary(hitVec.asUInt)} hitWay:${hitWay} vidx:${Binary(vidx.asUInt)}\n")

    (hit, hitWayData.ppns(genPtwL2SectorIdx(second_req.vpn)))
  }

  // l3
  val ptwl3replace = ReplacementPolicy.fromString(ptwl3Replacer,PtwL3WayNum,PtwL3LineNum)
  val (l3Hit, l3HitData) = {
    val ridx = genPtwL3SetIdx(first_req.vpn)
    val vidx = RegEnable(VecInit(getl3vSet(first_req.vpn).asBools), first_fire)
    l3.io.r.req.valid := first_fire
    l3.io.r.req.bits.apply(setIdx = ridx)
    val ramDatas = l3.io.r.resp.data
    val hitVec = VecInit(ramDatas.zip(vidx).map{ case (wayData, v) => wayData.hit(second_req.vpn) && v })
    val hitWayData = ParallelPriorityMux(hitVec zip ramDatas)
    val hit = ParallelOR(hitVec) && second_valid
    val hitWay = ParallelPriorityMux(hitVec zip (0 until PtwL3WayNum).map(_.U))

    when (hit) { ptwl3replace.access(genPtwL3SetIdx(second_req.vpn), hitWay) }

    l3AccessPerf.zip(hitVec).map{ case (l, h) => l := h && RegNext(first_fire) }
    XSDebug(first_fire, p"[l3] ridx:0x${Hexadecimal(ridx)}\n")
    for (i <- 0 until PtwL3WayNum) {
      XSDebug(RegNext(first_fire), p"[l3] ramDatas(${i.U}) ${ramDatas(i)}  l3v:${vidx(i)}  hit:${ramDatas(i).hit(second_req.vpn)}\n")
    }
    XSDebug(second_valid, p"[l3] l3Hit:${hit} l3HitData:${hitWayData} hitVec:${Binary(hitVec.asUInt)} hitWay:${hitWay} vidx:${Binary(vidx.asUInt)}\n")

    ridx.suggestName(s"l3_ridx")
    vidx.suggestName(s"l3_vidx")
    ramDatas.suggestName(s"l3_ramDatas")
    hitVec.suggestName(s"l3_hitVec")
    hitWay.suggestName(s"l3_hitWay")

    (hit, hitWayData)
  }
  val l3HitPPN = l3HitData.ppns(genPtwL3SectorIdx(second_req.vpn))
  val l3HitPerm = l3HitData.perms.getOrElse(0.U.asTypeOf(Vec(PtwL3SectorSize, new PtePermBundle)))(genPtwL3SectorIdx(second_req.vpn))

  // super page
  val spreplace = ReplacementPolicy.fromString(spReplacer, PtwSPEntrySize)
  val (spHit, spHitData) = {
    val hitVecT = sp.zipWithIndex.map { case (e, i) => e.hit(first_req.vpn) && spv(i) }
    val hitVec = hitVecT.map(RegEnable(_, first_fire))
    val hitData = ParallelPriorityMux(hitVec zip sp)
    val hit = ParallelOR(hitVec) && second_valid

    when (hit) { spreplace.access(OHToUInt(hitVec)) }

    spAccessPerf.zip(hitVec).map{ case (s, h) => s := h && RegNext(first_fire) }
    for (i <- 0 until PtwSPEntrySize) {
      XSDebug(first_fire, p"[sp] sp(${i.U}) ${sp(i)} hit:${sp(i).hit(first_req.vpn)} spv:${spv(i)}\n")
    }
    XSDebug(second_valid, p"[sp] spHit:${hit} spHitData:${hitData} hitVec:${Binary(VecInit(hitVec).asUInt)}\n")

    VecInit(hitVecT).suggestName(s"sp_hitVecT")
    VecInit(hitVec).suggestName(s"sp_hitVec")

    (hit, hitData)
  }
  val spHitPerm = spHitData.perm.getOrElse(0.U.asTypeOf(new PtePermBundle))
  val spHitLevel = spHitData.level.getOrElse(0.U)

  val resp = Wire(io.resp.bits.cloneType)
  val resp_latch = RegEnable(resp, io.resp.valid && !io.resp.ready)
  val resp_latch_valid = ValidHold(io.resp.valid && !io.resp.ready, io.resp.ready, sfence.valid)
  second_ready := !(second_valid || resp_latch_valid) || io.resp.fire()
  resp.source   := second_req.source
  resp.vpn      := second_req.vpn
  resp.isReplay := second_req.isReplay
  resp.hit      := l3Hit || spHit
  resp.toFsm.l1Hit := l1Hit
  resp.toFsm.l2Hit := l2Hit
  resp.toFsm.ppn   := Mux(l2Hit, l2HitPPN, l1HitPPN)
  resp.toTlb.tag   := second_req.vpn
  resp.toTlb.ppn   := Mux(l3Hit, l3HitPPN, spHitData.ppn)
  resp.toTlb.perm.map(_ := Mux(l3Hit, l3HitPerm, spHitPerm))
  resp.toTlb.level.map(_ := Mux(l3Hit, 2.U, spHitLevel))

  io.resp.valid := second_valid
  io.resp.bits := Mux(resp_latch_valid, resp_latch, resp)
  assert(!(l3Hit && spHit), "normal page and super page both hit")

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
  l2.io.w.req <> DontCare
  l3.io.w.req <> DontCare
  l2.io.w.req.valid := false.B
  l3.io.w.req.valid := false.B

  val memRdata = refill.ptes
  val memSelData = memRdata.asTypeOf(Vec(MemBandWidth/XLEN, UInt(XLEN.W)))(refill.memAddr(log2Up(l1BusDataWidth/8) - 1, log2Up(XLEN/8)))
  val memPtes = (0 until PtwL3SectorSize).map(i => memRdata((i+1)*XLEN-1, i*XLEN).asTypeOf(new PteBundle))
  val memPte = memSelData.asTypeOf(new PteBundle)

  // TODO: handle sfenceLatch outsize
  when (io.refill.valid && !memPte.isPf(refill.level) && !(sfence.valid || refuseRefill)) {
    when (refill.level === 0.U && !memPte.isLeaf()) {
      // val refillIdx = LFSR64()(log2Up(PtwL1EntrySize)-1,0) // TODO: may be LRU
      val refillIdx = replaceWrapper(l1v, ptwl1replace.way)
      refillIdx.suggestName(s"PtwL1RefillIdx")
      val rfOH = UIntToOH(refillIdx)
      l1(refillIdx).refill(refill.vpn, memSelData)
      ptwl1replace.access(refillIdx)
      l1v := l1v | rfOH
      l1g := (l1g & ~rfOH) | Mux(memPte.perm.g, rfOH, 0.U)

      for (i <- 0 until PtwL1EntrySize) {
        l1RefillPerf(i) := i.U === refillIdx
      }

      XSDebug(p"[l1 refill] refillIdx:${refillIdx} refillEntry:${l1(refillIdx).genPtwEntry(refill.vpn, memSelData)}\n")
      XSDebug(p"[l1 refill] l1v:${Binary(l1v)}->${Binary(l1v | rfOH)} l1g:${Binary(l1g)}->${Binary((l1g & ~rfOH) | Mux(memPte.perm.g, rfOH, 0.U))}\n")

      refillIdx.suggestName(s"l1_refillIdx")
      rfOH.suggestName(s"l1_rfOH")
    }

    when (refill.level === 1.U && !memPte.isLeaf()) {
      val refillIdx = genPtwL2SetIdx(refill.vpn)
      val victimWay = replaceWrapper(RegEnable(VecInit(getl2vSet(refill.vpn).asBools).asUInt, first_fire), ptwl2replace.way(refillIdx))
      val victimWayOH = UIntToOH(victimWay)
      val rfvOH = UIntToOH(Cat(refillIdx, victimWay))
      l2.io.w.apply(
        valid = true.B,
        setIdx = refillIdx,
        data = (new PtwEntries(num = PtwL2SectorSize, tagLen = PtwL2TagLen, level = 1, hasPerm = false)).genEntries(
          vpn = refill.vpn, data = memRdata, levelUInt = 1.U
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
          vpn = refill.vpn, data = memRdata, levelUInt = 1.U)
      }\n")
      XSDebug(p"[l2 refill] l2v:${Binary(l2v)} -> ${Binary(l2v | rfvOH)}\n")
      XSDebug(p"[l2 refill] l2g:${Binary(l2g)} -> ${Binary(l2g & ~rfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, rfvOH, 0.U))}\n")

      refillIdx.suggestName(s"l2_refillIdx")
      victimWay.suggestName(s"l2_victimWay")
      victimWayOH.suggestName(s"l2_victimWayOH")
      rfvOH.suggestName(s"l2_rfvOH")
    }

    when (refill.level === 2.U && memPte.isLeaf()) {
      val refillIdx = genPtwL3SetIdx(refill.vpn)
      val victimWay = replaceWrapper(RegEnable(VecInit(getl3vSet(refill.vpn).asBools).asUInt, first_fire), ptwl3replace.way(refillIdx))
      val victimWayOH = UIntToOH(victimWay)
      val rfvOH = UIntToOH(Cat(refillIdx, victimWay))
      l3.io.w.apply(
        valid = true.B,
        setIdx = refillIdx,
        data = (new PtwEntries(num = PtwL3SectorSize, tagLen = PtwL3TagLen, level = 2, hasPerm = true)).genEntries(
          vpn = refill.vpn, data = memRdata, levelUInt = 2.U
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
          vpn = refill.vpn, data = memRdata, levelUInt = 2.U)
      }\n")
      XSDebug(p"[l3 refill] l3v:${Binary(l3v)} -> ${Binary(l3v | rfvOH)}\n")
      XSDebug(p"[l3 refill] l3g:${Binary(l3g)} -> ${Binary(l3g & ~rfvOH | Mux(Cat(memPtes.map(_.perm.g)).andR, rfvOH, 0.U))}\n")

      refillIdx.suggestName(s"l3_refillIdx")
      victimWay.suggestName(s"l3_victimWay")
      victimWayOH.suggestName(s"l3_victimWayOH")
      rfvOH.suggestName(s"l3_rfvOH")
    }

    when ((refill.level === 0.U || refill.level === 1.U) && memPte.isLeaf()) {
      val refillIdx = spreplace.way// LFSR64()(log2Up(PtwSPEntrySize)-1,0) // TODO: may be LRU
      val rfOH = UIntToOH(refillIdx)
      sp(refillIdx).refill(refill.vpn, memSelData, refill.level)
      spreplace.access(refillIdx)
      spv := spv | rfOH
      spg := spg & ~rfOH | Mux(memPte.perm.g, rfOH, 0.U)

      for (i <- 0 until PtwSPEntrySize) {
        spRefillPerf(i) := i.U === refillIdx
      }

      XSDebug(p"[sp refill] refillIdx:${refillIdx} refillEntry:${sp(refillIdx).genPtwEntry(refill.vpn, memSelData, refill.level)}\n")
      XSDebug(p"[sp refill] spv:${Binary(spv)}->${Binary(spv | rfOH)} spg:${Binary(spg)}->${Binary(spg & ~rfOH | Mux(memPte.perm.g, rfOH, 0.U))}\n")

      refillIdx.suggestName(s"sp_refillIdx")
      rfOH.suggestName(s"sp_rfOH")
    }
  }

  // sfence
  when (sfence.valid) {
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
  XSPerfAccumulate("access", second_valid)
  XSPerfAccumulate("l1_hit", l1Hit)
  XSPerfAccumulate("l2_hit", l2Hit)
  XSPerfAccumulate("l3_hit", l3Hit)
  XSPerfAccumulate("sp_hit", spHit)
  XSPerfAccumulate("pte_hit", l3Hit || spHit)
  XSPerfAccumulate("rwHarzad", io.req.valid && !io.req.ready)
  XSPerfAccumulate("out_blocked", io.resp.valid && !io.resp.ready)
  l1AccessPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"L1AccessIndex${i}", l) }
  l2AccessPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"L2AccessIndex${i}", l) }
  l3AccessPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"L3AccessIndex${i}", l) }
  spAccessPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"SPAccessIndex${i}", l) }
  l1RefillPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"L1RefillIndex${i}", l) }
  l2RefillPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"L2RefillIndex${i}", l) }
  l3RefillPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"L3RefillIndex${i}", l) }
  spRefillPerf.zipWithIndex.map{ case (l, i) => XSPerfAccumulate(s"SPRefillIndex${i}", l) }

  // debug
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
    val resp = Flipped(ValidIO(new Bundle {
      val data = UInt(MemBandWidth.W)
    }))
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

  val memRdata = mem.resp.bits.data
  val memSelData = memRdata.asTypeOf(Vec(MemBandWidth/XLEN, UInt(XLEN.W)))(memAddrReg(log2Up(l1BusDataWidth/8) - 1, log2Up(XLEN/8)))
  val memPtes = (0 until PtwL3SectorSize).map(i => memRdata((i+1)*XLEN-1, i*XLEN).asTypeOf(new PteBundle))
  val memPte = memSelData.asTypeOf(new PteBundle)
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
  val resp = Reg(io.resp.bits.cloneType)
  when (finish && !sfenceLatch) {
    resp.source := RegEnable(io.req.bits.source, io.req.fire())
    resp.resp.pf := level === 3.U || notFound
    resp.resp.entry.tag := vpn
    resp.resp.entry.ppn := memPte.ppn
    resp.resp.entry.perm.map(_ := memPte.getPerm())
    resp.resp.entry.level.map(_ := level)
  }
  io.resp.valid := state === s_resp
  io.resp.bits := resp
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

class PTEHelper() extends BlackBox {
  val io = IO(new Bundle {
    val clock  = Input(Clock())
    val enable = Input(Bool())
    val satp   = Input(UInt(64.W))
    val vpn    = Input(UInt(64.W))
    val pte    = Output(UInt(64.W))
    val level  = Output(UInt(8.W))
    val pf     = Output(UInt(8.W))
  })
}

class FakePTW()(implicit p: Parameters) extends XSModule with HasPtwConst {
  val io = IO(new PtwIO)

  for (i <- 0 until PtwWidth) {
    io.tlb(i).req(0).ready := true.B

    val helper = Module(new PTEHelper())
    helper.io.clock := clock
    helper.io.enable := io.tlb(i).req(0).valid
    helper.io.satp := io.csr.satp.ppn
    helper.io.vpn := io.tlb(i).req(0).bits.vpn
    val pte = helper.io.pte.asTypeOf(new PteBundle)
    val level = helper.io.level
    val pf = helper.io.pf

    io.tlb(i).resp.valid := RegNext(io.tlb(i).req(0).valid)
    assert(!io.tlb(i).resp.valid || io.tlb(i).resp.ready)
    io.tlb(i).resp.bits.entry.tag := RegNext(io.tlb(i).req(0).bits.vpn)
    io.tlb(i).resp.bits.entry.ppn := pte.ppn
    io.tlb(i).resp.bits.entry.perm.map(_ := pte.getPerm())
    io.tlb(i).resp.bits.entry.level.map(_ := level)
    io.tlb(i).resp.bits.pf := pf
  }
}

class PTWWrapper()(implicit p: Parameters) extends LazyModule with HasDCacheParameters {
  val node = if (!useFakePTW) TLIdentityNode() else null
  val ptw = if (!useFakePTW) LazyModule(new PTW()) else null
  if (!useFakePTW) {
    node := ptw.node
  }

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new PtwIO)
    if (useFakePTW) {
      val fake_ptw = Module(new FakePTW())
      io <> fake_ptw.io
    }
    else {
      io <> ptw.module.io
    }
  }
}
