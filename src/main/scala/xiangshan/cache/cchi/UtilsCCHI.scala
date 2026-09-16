package xiangshan.cache

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.AddressSet
import oceanus.compactchi._

object CCHICheckTypeEquality {
  def apply(x: Data, y: Data): Boolean = {
    val kx = portClass(x)
    val ky = portClass(y)
    kx.nonEmpty && kx == ky
  }

  private def portClass(d: Data): Option[Class[_]] = d match {
    case _: CCHIType1Port => Some(classOf[CCHIType1Port])
    case _: CCHIType3Port => Some(classOf[CCHIType3Port])
    case _: CCHIType4Port => Some(classOf[CCHIType4Port])
    case _                => None
  }
}

object CCHIBuffer {
  // upper = requester / inner; down = completer / outer.
  // up* : upper -> Queue -> down;  dn* : down -> Queue -> upper.
  def apply[T <: Data](upper: T, down: T, nStages: Int = 1): Unit = {
    require(CCHICheckTypeEquality(upper, down), "CCHI port types must match")
    require(nStages >= 1, "nStages must be >= 1")
    val upperChans = channels(upper)
    val downChans = channels(down)
    upperChans.foreach { case (name, u) =>
      val d = downChans(name)
      if (name.startsWith("up")) {
        d <> QueueBuffer(u, nStages)
      } else if (name.startsWith("dn")) {
        u <> QueueBuffer(d, nStages)
      } else {
        require(false, s"CCHI channel must be up* or dn*: $name")
      }
    }
  }

  private def channels(p: Data): collection.Map[String, DecoupledIO[Data]] = {
    p.asInstanceOf[Record].elements.map { case (name, ch) =>
      require(ch.isInstanceOf[DecoupledIO[_]], s"CCHI field $name is not DecoupledIO")
      name -> ch.asInstanceOf[DecoupledIO[Data]]
    }
  }

  private def QueueBuffer[T <: Data](x: DecoupledIO[T], nStages: Int): DecoupledIO[T] = {
    (0 until nStages).foldLeft(x)((ch, _) => Queue(ch, 2))
  }
}

object CCHIXbar {
  def rrArb[T <: Data](out: DecoupledIO[T], ins: Seq[DecoupledIO[T]]): Unit = {
    if (ins.length == 1) {
      out <> ins.head
    } else {
      val arb = Module(new RRArbiter(chiselTypeOf(out.bits), ins.length))
      arb.io.in.zip(ins).foreach { case (a, i) => a <> i }
      out <> arb.io.out
    }
  }

  def addrHitOH(addr: UInt, addrSets: Seq[Seq[AddressSet]]): UInt = {
    val hits = addrSets.map { sets =>
      if (sets.isEmpty) false.B
      else sets.map(_.contains(addr)).reduce(_ || _)
    }
    val anyHit = hits.reduce(_ || _)
    VecInit(addrSets.zip(hits).map { case (sets, h) =>
      if (sets.isEmpty) !anyHit else h
    }).asUInt
  }
}

/*
 * MMIO Type3 xbar (TL mmio_xbar).
 *
 * Up: typically d_mmio (UncacheSrcId) and i_mmio (InstrUncacheSrcId).
 * Down: address-decoded completer ports; empty Seq[AddressSet] is the default (must be last).
 * upDAT has no Addr: use upDatAddr (Uncache.txdatAddr).
 */
class CCHIType3Xbar(
  val nUp: Int,
  val downAddrSets: Seq[Seq[AddressSet]],
  val upSrcIds: Seq[Int]
) extends Module {
  val nDown: Int = downAddrSets.length
  require(nUp >= 1 && nDown >= 1)
  require(upSrcIds.length == nUp, "upSrcIds length")
  require(downAddrSets.count(_.isEmpty) <= 1, "at most one default down port")
  require(downAddrSets.indexWhere(_.isEmpty) < 0 || downAddrSets.last.isEmpty, "default down port must be last")
  for (i <- downAddrSets.indices; j <- i + 1 until nDown) {
    if (downAddrSets(i).nonEmpty && downAddrSets(j).nonEmpty) {
      require(!downAddrSets(i).exists(a => downAddrSets(j).exists(_.overlaps(a))),
        s"CCHIType3Xbar down $i and $j overlap")
    }
  }

  val io = IO(new Bundle {
    val up = Vec(nUp, Flipped(new CCHIType3Port))
    val upDatAddr = Input(Vec(nUp, UInt(48.W)))
    val down = Vec(nDown, new CCHIType3Port)
  })

  private val uReq = Seq.fill(nUp)(Wire(Vec(nDown, Decoupled(new FlitREQ))))
  private val uDat = Seq.fill(nUp)(Wire(Vec(nDown, Decoupled(new FlitUpDAT64))))

  for (u <- 0 until nUp) {
    val reqOH = CCHIXbar.addrHitOH(io.up(u).upREQ.bits.Addr, downAddrSets)
    val datOH = CCHIXbar.addrHitOH(io.upDatAddr(u), downAddrSets)
    for (d <- 0 until nDown) {
      uReq(u)(d).valid := io.up(u).upREQ.valid && reqOH(d)
      uReq(u)(d).bits := io.up(u).upREQ.bits
      uDat(u)(d).valid := io.up(u).upDAT.valid && datOH(d)
      uDat(u)(d).bits := io.up(u).upDAT.bits
    }
    io.up(u).upREQ.ready := Mux1H(reqOH, (0 until nDown).map(uReq(u)(_).ready))
    io.up(u).upDAT.ready := Mux1H(datOH, (0 until nDown).map(uDat(u)(_).ready))
    when (io.up(u).upREQ.valid) {
      assert(PopCount(reqOH) === 1.U, "CCHIType3Xbar: upREQ must hit exactly one down port")
    }
    when (io.up(u).upDAT.valid) {
      assert(PopCount(datOH) === 1.U, "CCHIType3Xbar: upDAT must hit exactly one down port")
    }
  }
  for (d <- 0 until nDown) {
    CCHIXbar.rrArb(io.down(d).upREQ, (0 until nUp).map(uReq(_)(d)))
    CCHIXbar.rrArb(io.down(d).upDAT, (0 until nUp).map(uDat(_)(d)))
  }

  private val rspToU = Seq.fill(nUp)(Wire(Vec(nDown, Decoupled(new FlitDnRSP))))
  private val datToU = Seq.fill(nUp)(Wire(Vec(nDown, Decoupled(new FlitDnDAT64))))
  for (d <- 0 until nDown) {
    val tgtR = io.down(d).dnRSP.bits.TgtID
    val tgtD = io.down(d).dnDAT.bits.TgtID
    val rspOH = VecInit(upSrcIds.map(id => tgtR === id.U)).asUInt
    val datOH = VecInit(upSrcIds.map(id => tgtD === id.U)).asUInt
    val rspBits = WireInit(io.down(d).dnRSP.bits)
    val datBits = WireInit(io.down(d).dnDAT.bits)
    rspBits.SrcID := d.U(8.W)
    datBits.SrcID := d.U(8.W)
    for (u <- 0 until nUp) {
      rspToU(u)(d).valid := io.down(d).dnRSP.valid && rspOH(u)
      rspToU(u)(d).bits := rspBits
      datToU(u)(d).valid := io.down(d).dnDAT.valid && datOH(u)
      datToU(u)(d).bits := datBits
    }
    io.down(d).dnRSP.ready := Mux1H(rspOH, (0 until nUp).map(rspToU(_)(d).ready))
    io.down(d).dnDAT.ready := Mux1H(datOH, (0 until nUp).map(datToU(_)(d).ready))
  }
  for (u <- 0 until nUp) {
    CCHIXbar.rrArb(io.up(u).dnRSP, (0 until nDown).map(rspToU(u)(_)))
    CCHIXbar.rrArb(io.up(u).dnDAT, (0 until nDown).map(datToU(u)(_)))
  }
}

object CCHIType3Xbar {
  def apply(
    nUp: Int,
    downAddrSets: Seq[Seq[AddressSet]],
    upSrcIds: Seq[Int]
  ): CCHIType3Xbar = Module(new CCHIType3Xbar(nUp, downAddrSets, upSrcIds))
}
