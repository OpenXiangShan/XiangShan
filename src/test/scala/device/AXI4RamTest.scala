/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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

package device

import org.chipsalliance.cde.config._
import chisel3._
import chiseltest._
import freechips.rocketchip.amba.axi4.{AXI4Deinterleaver, AXI4UserYanker}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import utils.DebugIdentityNode

class AXI4RamFuzzTest()(implicit p: Parameters) extends LazyModule {

  val addressSet = AddressSet(0x38000000L, 0x0000ffffL)

  val fuzz = LazyModule(new TLFuzzer(
    nOperations = 10,
    overrideAddress = Some(addressSet),
    inFlight = 10
  ))
  val ident = LazyModule(new DebugIdentityNode())
  val axiRam = LazyModule(new AXI4RAM(Seq(addressSet), memByte = 1024))

  axiRam.node :=
    AXI4UserYanker() :=
    TLToAXI4() :=
    TLFragmenter(8, 8) :=
    ident.node :=
    fuzz.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this){
    val finished = IO(Output(Bool()))
    finished := fuzz.module.io.finished
  }
}

class AXI4RamBurstTest()(implicit p: Parameters) extends LazyModule {

  val addressSet = AddressSet(0x38000000L, 0x0000ffffL)
  val burst = LazyModule(new AXI4BurstMaster(startAddr = addressSet.base.toLong, nOp = 3))
  val axiRam = LazyModule(new AXI4RAM(Seq(addressSet), memByte = 1024))

  axiRam.node := burst.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this){
    val finished = IO(Output(Bool()))
    finished := burst.module.io.finished
  }

}

class AXI4RamTLBurstTest()(implicit p: Parameters) extends LazyModule {

  val addressSet = AddressSet(0x38000000L, 0x0000ffffL)

  val tlburst = LazyModule(new TLBurstMaster(startAddr = addressSet.base.toLong, nOp = 1, burstLen = 32))
  val ident = LazyModule(new DebugIdentityNode())
  val axiRam = LazyModule(new AXI4RAM(Seq(addressSet), memByte = 1024))

  axiRam.node :=
    AXI4UserYanker() :=
    TLToAXI4() :=
    TLFragmenter(8, 32 * 8, holdFirstDeny = true) :=
    ident.node :=
    tlburst.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this){
    val finished = IO(Output(Bool()))
    finished := tlburst.module.io.finished
  }
}

class AXI4RamTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
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
