package device

import chipsalliance.rocketchip.config._
import chisel3._
import chiseltest._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import utils.DebugIdentityNode

class TLTimerTestTop()(implicit p: Parameters) extends LazyModule {


  val addressSet = AddressSet(0x38000000L, 0x0000ffffL)

  val timer = LazyModule(new TLTimer(Seq(addressSet), sim = true))
  val ident = LazyModule(new DebugIdentityNode)
  val fuzz = LazyModule(new TLFuzzer(
    nOperations = 10,
    overrideAddress = Some(addressSet)
  ))

  timer.node := ident.node := TLDelayer(0.1) := fuzz.node

  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle() {
      val finished = Output(Bool())
    })
    io.finished := fuzz.module.io.finished
  }
}

class TLTimerTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  it should "run" in {
    implicit val p = Parameters.empty
    test(LazyModule(new TLTimerTestTop()).module){ c =>
      while (!c.io.finished.peek().litToBoolean){
        c.clock.step(1)
      }
    }
  }

}
