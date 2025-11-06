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
import freechips.rocketchip.amba.axi4.{AXI4Deinterleaver, AXI4UserYanker, AXI4Xbar}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import top.SimMMIO
import utils.DebugIdentityNode

class SimMMIOTestTop()(implicit p: Parameters) extends LazyModule {

  val addressSet = AddressSet(0x40600000L, 0xf)

  val fuzz = LazyModule(new TLFuzzer(nOperations = 10, inFlight = 1, overrideAddress = Some(addressSet)))
  val simMMIO = LazyModule(new SimMMIO())

  simMMIO.axiBus :=
    AXI4UserYanker() :=
    TLToAXI4() :=
    DebugIdentityNode() :=
    fuzz.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this){
    val finished = IO(Output(Bool()))
    finished := fuzz.module.io.finished
  }
}

class SimMMIOTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  it should "run" in {
    implicit val p = Parameters.empty
    test(LazyModule(new SimMMIOTestTop()).module){c =>
      while (!c.finished.peek().litToBoolean){
        c.clock.step(1)
      }
    }
  }
}
