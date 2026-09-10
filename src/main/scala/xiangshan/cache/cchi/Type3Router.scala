package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import oceanus.compactchi._

/*
 * 1 upstream Type3 requester (Uncache) -> N downstream ports by address decode.
 * TXREQ/TXDAT: demux by Addr (txdatAddr from Uncache entry, same as txreq Addr).
 * RXRSP/RXDAT: RR arbiter when downstream respond.
 * downCtrl(0) = D$ CtrlUnit, downCtrl(1) = I$ CtrlUnit.
 */
class Type3Router(implicit val p: Parameters) extends Module with HasDCacheParameters {
  val io = IO(new Bundle {
    val up = Flipped(new CCHIType3Port)
    val txdatAddr = Input(UInt(48.W))
    val downL2 = new CCHIType3Port
    val downCtrl = Vec(2, new CCHIType3Port)
  })

  private def isDCacheCtrlAddr(addr: UInt): Bool =
    dcacheParameters.cacheCtrlAddressOpt.map(_.contains(addr)).getOrElse(false.B)

  private def isICacheCtrlAddr(addr: UInt): Bool =
    Option.when(icacheCtrlEnabled)(icacheCtrlAddress.contains(addr)).getOrElse(false.B)

  private def isCtrlAddr(addr: UInt): Bool = isDCacheCtrlAddr(addr) || isICacheCtrlAddr(addr)
  private def ctrlPortOH(addr: UInt): UInt = Cat(isICacheCtrlAddr(addr), isDCacheCtrlAddr(addr))

  private def demuxTxReq(up: DecoupledIO[FlitREQ], downCtrl: Seq[DecoupledIO[FlitREQ]], downL2: DecoupledIO[FlitREQ]): Unit = {
    val isCtrl = up.valid && isCtrlAddr(up.bits.Addr)
    val ctrlOH = ctrlPortOH(up.bits.Addr)
    downCtrl.zipWithIndex.foreach { case (downctrl, i) =>
      downctrl.valid := isCtrl && ctrlOH(i)
      downctrl.bits := up.bits
    }
    downL2.valid := up.valid && !isCtrl
    downL2.bits := up.bits
    up.ready := Mux(isCtrl, Mux1H(ctrlOH, downCtrl.map(_.ready)), downL2.ready)
  }

  private def demuxTxDat(up: DecoupledIO[FlitUpDAT64], addr: UInt, downCtrl: Seq[DecoupledIO[FlitUpDAT64]], downL2: DecoupledIO[FlitUpDAT64]): Unit = {
    val isCtrl = up.valid && isCtrlAddr(addr)
    val ctrlOH = ctrlPortOH(addr)
    downCtrl.zipWithIndex.foreach { case (downctrl, i) =>
      downctrl.valid := isCtrl && ctrlOH(i)
      downctrl.bits := up.bits
    }
    downL2.valid := up.valid && !isCtrl
    downL2.bits := up.bits
    up.ready := Mux(isCtrl, Mux1H(ctrlOH, downCtrl.map(_.ready)), downL2.ready)
  }

  demuxTxReq(io.up.txreq, io.downCtrl.map(_.txreq), io.downL2.txreq)
  demuxTxDat(io.up.txdat, io.txdatAddr, io.downCtrl.map(_.txdat), io.downL2.txdat)
  assert(PopCount(io.downCtrl.map(_.txreq.valid) :+ io.downL2.txreq.valid) <= 1.U, "Type3Router: at most one to-downstream txreq valid")

  // RXRSP/RXDAT: RR arbiter (downL2 = in(0), downCtrl(i) = in(i + 1))
  private def arbDownRx[T <: Data](up: DecoupledIO[T], downL2: DecoupledIO[T], downCtrl: Seq[DecoupledIO[T]]): Unit = {
    val arb = Module(new RRArbiter(chiselTypeOf(up.bits), 1 + downCtrl.length))
    arb.io.in(0).valid := downL2.valid
    arb.io.in(0).bits := downL2.bits
    downCtrl.zipWithIndex.foreach { case (port, i) =>
      arb.io.in(i + 1).valid := port.valid
      arb.io.in(i + 1).bits := port.bits
    }
    up <> arb.io.out
    downL2.ready := arb.io.in(0).ready
    downCtrl.zipWithIndex.foreach { case (port, i) =>
      port.ready := arb.io.in(i + 1).ready
    }
  }

  arbDownRx(io.up.rxrsp, io.downL2.rxrsp, io.downCtrl.map(_.rxrsp))
  arbDownRx(io.up.rxdat, io.downL2.rxdat, io.downCtrl.map(_.rxdat))
}
