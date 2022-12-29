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

import chisel3._
import chisel3.util._
import chipsalliance.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.AddressSet
import utils._
import utility._

// we support 256 interrupt bits by default
class IntrGenIO extends Bundle {
  val intrVec = Output(UInt(64.W))
}

class AXI4IntrGenerator
(
  address: Seq[AddressSet]
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable = false, _extra = new IntrGenIO)
{

  override lazy val module = new AXI4SlaveModuleImp(this){

    val intrGenRegs = RegInit(VecInit(Seq.fill(8)(0.U(32.W))))

    // 0x0 - 0x8
    val intrReg = VecInit(intrGenRegs.take(2))
    // 0x8 - 0x10
    val randEnable = VecInit(intrGenRegs.slice(2, 4))
    // 0x10
    val randMask = intrGenRegs(4)
    val randCounter = intrGenRegs(5)
    val randThres = intrGenRegs(6)

    val randomPosition = LFSR64()(5, 0)
    val randomCondition = randCounter === randThres && randEnable(randomPosition(5))(randomPosition(4, 0))
    randCounter := randCounter + 1.U
    when (randomCondition) {
      intrGenRegs(randomPosition(5)) := intrReg(randomPosition(5)) | UIntToOH(randomPosition(4, 0))
    }

    io.extra.get.intrVec := Cat(intrReg.reverse)

    // Delay the intr gen for 1000 cycles.
    val delayCycles = 1000
    var w_fire = in.w.fire && in.w.bits.data =/= 0.U
    for (i <- 0 until delayCycles) {
      w_fire = RegNext(w_fire, init=false.B)
    }
    val w_data = DelayN(in.w.bits.data(31, 0), delayCycles)
    when (w_fire) {
      intrGenRegs(DelayN(waddr(4, 2), delayCycles)) := w_data
    }
    // Clear takes effect immediately
    when (in.w.fire && in.w.bits.data === 0.U) {
      intrGenRegs(waddr(4, 2)) := 0.U
    }
    // write resets the threshold and counter
    when (in.w.fire && in.w.bits.data === 0.U || w_fire) {
      randThres := LFSR64() & randMask
      randCounter := 0.U
    }

    in.r.bits.data := intrReg(raddr)
  }
}
