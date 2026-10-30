package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.cache._

// Exercise the production demux, lowestArb, and complete CCHIBuffer payload.
class PfBTrainRouteHarness(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val in = Flipped(Vec(3, Decoupled(DCacheREQ())))
    val lane = Input(Vec(3, UInt(1.W)))
    val out = Vec(2, Decoupled(DCacheREQ()))
  })
  val demux = io.in.zip(io.lane).map { case (in, ch) => demuxByCCHIChannel(in, ch, 2) }
  for (lane <- 0 until 2) {
    val upper = Wire(new CCHIType1Port)
    val down = Wire(new CCHIType1Port)
    lowestArb(upper.upREQ, demux.map(_(lane)))
    CCHIBuffer(upper, down, nStages = 2)
    io.out(lane) <> down.upREQ
    upper.upEVT.valid := false.B
    upper.upEVT.bits := 0.U.asTypeOf(upper.upEVT.bits)
    upper.upRSP.valid := false.B
    upper.upRSP.bits := 0.U.asTypeOf(upper.upRSP.bits)
    upper.upDAT.valid := false.B
    upper.upDAT.bits := 0.U.asTypeOf(upper.upDAT.bits)
    down.upEVT.ready := true.B
    down.upRSP.ready := true.B
    down.upDAT.ready := true.B
    down.dnSNP.valid := false.B
    down.dnSNP.bits := 0.U.asTypeOf(down.dnSNP.bits)
    down.dnRSP.valid := false.B
    down.dnRSP.bits := 0.U.asTypeOf(down.dnRSP.bits)
    down.dnDAT.valid := false.B
    down.dnDAT.bits := 0.U.asTypeOf(down.dnDAT.bits)
    upper.dnSNP.ready := true.B
    upper.dnRSP.ready := true.B
    upper.dnDAT.ready := true.B
  }
}

class PfBTrainRouteTest extends AnyFlatSpec with ChiselSim {
  "Type-1 training payload" should "stay paired across two independently stalled lanes" in {
    val base = new top.PfBAllConfig
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head
    }
    simulate(new PfBTrainRouteHarness) { dut =>
      val expected = Array.fill(2)(scala.collection.mutable.Queue.empty[(BigInt, BigInt, Int, Boolean)])
      val ids = Array(1, 2, 3)
      var accepted = 0
      var completed = 0
      for (cycle <- 0 until 650) {
        for (lane <- 0 until 2) dut.io.out(lane).ready.poke((cycle >= 500 || (cycle + lane * 3) % (7 + lane) >= 3).B)
        for (source <- 0 until 3) {
          val port = dut.io.in(source)
          val id = ids(source)
          val lane = (id + source) & 1
          val cmo = source == 0
          val va = if (cmo) BigInt(0) else (BigInt(id) << 17) | BigInt(0x155 + source)
          val reqSource = if (cmo) 0 else if (source == 1) 2 else 6
          // Initialize the complete flit so random unobserved fields cannot mask errors.
          port.bits.elements.filterNot(_._1 == "train").values.foreach {
            case b: Bool => b.poke(false.B)
            case u: UInt => u.poke(0.U)
            case _ => ()
          }
          port.bits.Addr.poke((BigInt(id) << 6).U)
          val train = DCacheREQ.train(port.bits).get
          train.vaddrBlock.poke(va.U)
          train.reqSource.poke(reqSource.U)
          train.trainEnable.poke((!cmo).B)
          dut.io.lane(source).poke(lane.U)
          port.valid.poke((cycle < 500 && (cycle + source) % 5 != 0).B)
        }
        for (source <- 0 until 3) {
          val port = dut.io.in(source)
          if (port.valid.peek().litToBoolean && port.ready.peek().litToBoolean) {
            val lane = dut.io.lane(source).peek().litValue.toInt
            val train = DCacheREQ.train(port.bits).get
            expected(lane).enqueue((port.bits.Addr.peek().litValue, train.vaddrBlock.peek().litValue,
              train.reqSource.peek().litValue.toInt, train.trainEnable.peek().litToBoolean))
            ids(source) += 3
            accepted += 1
          }
        }
        for (lane <- 0 until 2) {
          val port = dut.io.out(lane)
          if (port.valid.peek().litToBoolean && port.ready.peek().litToBoolean) {
            assert(expected(lane).nonEmpty)
            val (pa, va, source, trainEn) = expected(lane).dequeue()
            val train = DCacheREQ.train(port.bits).get
            port.bits.Addr.expect(pa.U)
            train.vaddrBlock.expect(va.U)
            train.reqSource.expect(source.U)
            train.trainEnable.expect(trainEn.B)
            completed += 1
          }
        }
        dut.clock.step()
      }
      assert(accepted > 150 && completed == accepted && expected.forall(_.isEmpty))
      println(s"pf-b paired payloads accepted=$accepted completed=$completed; lanes=2, queueStages=2")
    }
  }
}
