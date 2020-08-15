package device

import chipsalliance.rocketchip.config._
import chisel3._
import chiseltest._
import freechips.rocketchip.amba.axi4.{AXI4Deinterleaver, AXI4UserYanker}
import org.scalatest.{FlatSpec, Matchers}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import utils.DebugIdentityNode

class AXI4RamFuzzTest()(implicit p: Parameters) extends LazyModule {

  val addressSet = AddressSet(0x38000000L, 0x0000ffffL)

  val fuzz = LazyModule(new TLFuzzer(
    nOperations = 10,
    overrideAddress = Some(addressSet),
    inFlight = 1
  ))
  val ident = LazyModule(new DebugIdentityNode())
  val axiRam = LazyModule(new AXI4RAM(addressSet))

  axiRam.node :=
    AXI4UserYanker() :=
    TLToAXI4() :=
    TLFragmenter(8, 8) :=
    ident.node :=
    fuzz.node

  lazy val module = new LazyModuleImp(this){
    val finished = IO(Output(Bool()))
    finished := fuzz.module.io.finished
  }
}

class AXI4RamBurstTest()(implicit p: Parameters) extends LazyModule {

  val addressSet = AddressSet(0x38000000L, 0x0000ffffL)
  val burst = LazyModule(new AXI4BurstMaster(startAddr = addressSet.base.toLong, nOp = 3))
  val axiRam = LazyModule(new AXI4RAM(addressSet))

  axiRam.node := burst.node

  lazy val module = new LazyModuleImp(this){
    val finished = IO(Output(Bool()))
    finished := burst.module.io.finished
  }

}

class AXI4RamTLBurstTest()(implicit p: Parameters) extends LazyModule {

  val addressSet = AddressSet(0x38000000L, 0x0000ffffL)

  val tlburst = LazyModule(new TLBurstMaster(startAddr = addressSet.base.toLong, nOp = 3))
  val ident = LazyModule(new DebugIdentityNode())
  val axiRam = LazyModule(new AXI4RAM(addressSet))

  axiRam.node :=
    AXI4UserYanker() :=
    TLToAXI4() :=
    TLFragmenter(8, 8) :=
    ident.node :=
    tlburst.node

  lazy val module = new LazyModuleImp(this){
    val finished = IO(Output(Bool()))
    finished := tlburst.module.io.finished
  }
}

class AXI4RamTest extends FlatSpec with ChiselScalatestTester with Matchers {
  it should "run with fuzz" in {
    implicit val p = Parameters.empty
    test(LazyModule(new AXI4RamFuzzTest()).module){ c =>
      while (!c.finished.peek().litToBoolean){
        c.clock.step(1)
      }
    }
  }

  it should "run in burst mode with axi master" in {
    implicit val p = Parameters.empty
    test(LazyModule(new AXI4RamBurstTest()).module){c =>
      while (!c.finished.peek().litToBoolean){
        c.clock.step(1)
      }
    }
  }

  it should "run in burst mode with tilelink master" in {
    implicit val p = Parameters.empty
    test(LazyModule(new AXI4RamTLBurstTest()).module){c =>
      while (!c.finished.peek().litToBoolean){
        c.clock.step(1)
      }
    }
  }
}
