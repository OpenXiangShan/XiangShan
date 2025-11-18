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

package xiangshan.testutils

import chisel3._
import chisel3.util._
import chiseltest._

class PartialDecoupledDriver[T <: Data](x: DecoupledIO[T]) extends DecoupledDriver(x){
  val sourceClock = x.getSourceClock()
  val sinkClock = x.getSinkClock()

  def enqueuePartialNow(data: T): Unit = timescope {
    // TODO: check for init
    x.bits.asInstanceOf[Record].pokePartial(data.asInstanceOf[Record])
    x.valid.poke(true.B)
    fork.withRegion(Monitor) {
      x.ready.expect(true.B)
    }.joinAndStep(sourceClock)
  }

  def enqueuePartial(data: T): Unit = timescope {
    // TODO: check for init
    x.bits.asInstanceOf[Record].pokePartial(data.asInstanceOf[Record])
    x.valid.poke(true.B)
    fork.withRegion(Monitor) {
      while (!x.ready.peek().litToBoolean) {
        sourceClock.step(1)
      }
    }.joinAndStep(sourceClock)
  }

  def enqueuePartialSeq(data: Seq[T]): Unit = timescope {
    for (elt <- data) {
      enqueuePartial(elt)
    }
  }


  def expectDequeuePartialNow(data: T): Unit = timescope {
    // TODO: check for init
    x.ready.poke(true.B)
    fork.withRegion(Monitor) {
      x.valid.expect(true.B)
      x.bits.asInstanceOf[Record].expectPartial(data.asInstanceOf[Record])
    }.joinAndStep(sinkClock)
  }

  def expectDequeuePartial(data: T): Unit = timescope {
    // TODO: check for init
    x.ready.poke(true.B)
    fork.withRegion(Monitor) {
      waitForValid()
      x.valid.expect(true.B)
      x.bits.asInstanceOf[Record].expectPartial(data.asInstanceOf[Record])
    }.joinAndStep(sinkClock)
  }

  def expectDequeuePartialSeq(data: Seq[T]): Unit = timescope {
    for (elt <- data) {
      expectDequeuePartial(elt)
    }
  }


}

trait HasPartialDecoupledDriver {
  implicit def partialDecoupledDriver[T <: Data](x: DecoupledIO[T]) = new PartialDecoupledDriver[T](x)
}
