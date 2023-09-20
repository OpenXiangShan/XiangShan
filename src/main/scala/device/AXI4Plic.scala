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
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import utils.{XSDebug, HasTLDump}
import utility.{RegMap, MaskExpand}

/*  base + 0x000000: Reserved (interrupt source 0 does not exist)
    base + 0x000004: Interrupt source 1 priority
    base + 0x000008: Interrupt source 2 priority
    ...
    base + 0x000FFC: Interrupt source 1023 priority
    base + 0x001000: Interrupt Pending bit 0-31
    base + 0x00107C: Interrupt Pending bit 992-1023
    ...
    base + 0x002000: Enable bits for sources 0-31 on context 0
    base + 0x002004: Enable bits for sources 32-63 on context 0
    ...
    base + 0x00207F: Enable bits for sources 992-1023 on context 0
    base + 0x002080: Enable bits for sources 0-31 on context 1
    base + 0x002084: Enable bits for sources 32-63 on context 1
    ...
    base + 0x0020FF: Enable bits for sources 992-1023 on context 1
    base + 0x002100: Enable bits for sources 0-31 on context 2
    base + 0x002104: Enable bits for sources 32-63 on context 2
    ...
    base + 0x00217F: Enable bits for sources 992-1023 on context 2
    ...
    base + 0x1F1F80: Enable bits for sources 0-31 on context 15871
    base + 0x1F1F84: Enable bits for sources 32-63 on context 15871
    base + 0x1F1FFF: Enable bits for sources 992-1023 on context 15871
    ...
    base + 0x1FFFFC: Reserved
    base + 0x200000: Priority threshold for context 0
    base + 0x200004: Claim/complete for context 0
    base + 0x200008: Reserved
    ...
    base + 0x200FFC: Reserved
    base + 0x201000: Priority threshold for context 1
    base + 0x201004: Claim/complete for context 1
    ...
    base + 0x3FFE000: Priority threshold for context 15871
    base + 0x3FFE004: Claim/complete for context 15871
    base + 0x3FFE008: Reserved
    ...
    base + 0x3FFFFFC: Reserved  */

object PLICConsts
{
  def maxDevices = 1023
  def maxHarts = 15872
  def priorityBase = 0x0
  def pendingBase = 0x1000
  def enableBase = 0x2000
  def hartBase = 0x200000

  def claimOffset = 4
  def priorityBytes = 4

  def enableOffset(i: Int) = i * ((maxDevices+7)/8)
  def hartOffset(i: Int) = i * 0x1000
  def enableBase(i: Int):Int = enableOffset(i) + enableBase
  def hartBase(i: Int):Int = hartOffset(i) + hartBase

  def size(maxHarts: Int): Int = {
    require(maxHarts > 0 && maxHarts <= maxHarts, s"Must be: maxHarts=$maxHarts > 0 && maxHarts <= PLICConsts.maxHarts=${PLICConsts.maxHarts}")
    1 << log2Ceil(hartBase(maxHarts))
  }

  require(hartBase >= enableBase(maxHarts))
}

class PlicIO(val numCores: Int, val numExtIntrs: Int) extends Bundle{
  val intrVec = Input(UInt(numExtIntrs.W))
  val meip = Output(Vec(numCores, Bool()))
}

class AXI4Plic
(
  address: Seq[AddressSet],
  numCores: Int,
  numExtIntrs: Int,
  sim: Boolean = false
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable = false, _extra = new PlicIO(numCores, numExtIntrs))
{
  override lazy val module = new AXI4SlaveModuleImp[PlicIO](this) {
    require(numCores <= PLICConsts.maxDevices)
    require(numExtIntrs <= PLICConsts.maxHarts)
    val addressSpaceSize = 0x4000000
    val addressBits = log2Up(addressSpaceSize)

    def getOffset(addr: UInt) = addr(addressBits - 1, 0)

    val priority = List.fill(numExtIntrs)(Reg(UInt(32.W)))
    val priorityMap = priority.zipWithIndex.map { case (r, intr) => RegMap((intr + 1) * 4, r) }.toMap

    val nrIntrWord = (numExtIntrs + 31) / 32 // roundup
    // pending bits are updated in the unit of bit by PLIC,
    // so define it as vectors of bits, instead of UInt(32.W)
    val pending = List.fill(nrIntrWord)(RegInit(0.U.asTypeOf(Vec(32, Bool()))))
    val pendingMap = pending.zipWithIndex.map { case (r, intrWord) =>
      RegMap(0x1000 + intrWord * 4, Cat(r.reverse), RegMap.Unwritable)
    }.toMap

    val enable = List.fill(numCores)(List.fill(nrIntrWord)(RegInit(0.U(32.W))))
    val enableMap = enable.zipWithIndex.map { case (l, hart) =>
      l.zipWithIndex.map { case (r, intrWord) => RegMap(0x2000 + hart * 0x80 + intrWord * 4, r) }
    }.reduce(_ ++ _).toMap

    val threshold = List.fill(numCores)(Reg(UInt(32.W)))
    val thresholdMap = threshold.zipWithIndex.map {
      case (r, hart) => RegMap(0x200000 + hart * 0x1000, r)
    }.toMap

    val inHandle = RegInit(0.U.asTypeOf(Vec(numExtIntrs + 1, Bool())))

    def completionFn(wdata: UInt) = {
      inHandle(wdata(31, 0)) := false.B
      0.U
    }

    val claimCompletion = List.fill(numCores)(Reg(UInt(32.W)))
    val claimCompletionMap = claimCompletion.zipWithIndex.map {
      case (r, hart) => {
        val addr = 0x200004 + hart * 0x1000
        when(in.r.fire && (getOffset(raddr) === addr.U)) {
          inHandle(r) := true.B
        }
        RegMap(addr, r, completionFn)
      }
    }.toMap

    val intrVecReg = Wire(UInt(numExtIntrs.W))
    intrVecReg := RegNext(RegNext(RegNext(io.extra.get.intrVec)))
    intrVecReg.asBools.zipWithIndex.map { case (intr, i) => {
      val id = i + 1
      when(intr) {
        pending(id / 32)(id % 32) := true.B
      }
      when(inHandle(id)) {
        pending(id / 32)(id % 32) := false.B
      }
    }
    }

    val pendingVec = Cat(pending.map(x => Cat(x.reverse)))
    claimCompletion.zipWithIndex.map { case (r, hart) => {
      val takenVec = pendingVec & Cat(enable(hart))
      r := Mux(takenVec === 0.U, 0.U, PriorityEncoder(takenVec))
    }
    }

    val mapping = priorityMap ++ pendingMap ++ enableMap ++ thresholdMap ++ claimCompletionMap

    val rdata = Wire(UInt(32.W))
    RegMap.generate(mapping, getOffset(raddr), rdata,
      getOffset(waddr), in.w.fire, in.w.bits.data, MaskExpand(in.w.bits.strb >> waddr(2, 0)))
    // narrow read
    in.r.bits.data := Fill(2, rdata)

    io.extra.get.meip.zipWithIndex.map { case (ip, hart) => ip := claimCompletion(hart) =/= 0.U }
  }
}
