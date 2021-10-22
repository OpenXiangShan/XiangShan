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

package xiangshan.backend.pubs

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._

class ConfTabEntry(tagWidth: Int, counterWidth: Int)(implicit p: Parameters) extends XSBundle {
  // If the counter value in the corresponding entry is the maximum value,
  // the prediction of the associated branch is confident; otherwise, it is unconfident.
  val lowConfThreshold = 1 << counterWidth - 2

  val valid = Bool()
  val tag = UInt(tagWidth.W)
  val counter = UInt(counterWidth.W)

  def inc(): Unit = { counter := Mux(counter.andR, counter, counter + 1.U) }
  def reset(): Unit = { counter := 0.U }
  def allocate(): Unit = { counter := (1 << counterWidth - 1).U }
  def isLowConf(): Bool = valid && counter <= lowConfThreshold.U
  override def cloneType: ConfTabEntry.this.type =
    new ConfTabEntry(tagWidth, counterWidth).asInstanceOf[this.type]
}

class ConfTable(implicit p: Parameters) extends XSModule {
  val confTableSet = 128
  val confTableWay = 8

  val setWidth = log2Up(confTableSet)
  val tagWidth = 4
  val counterWidth = 6
  def pcSetIdx(pc: UInt): UInt = pc(setWidth - 1, 0)
  def pcTag(pc: UInt): UInt = XORFold(pc(VAddrBits - 1, setWidth), tagWidth)

  val io = IO(new Bundle {
    val read = Vec(RenameWidth, new Bundle {
      val enable = Input(Bool())
      val address = Input(UInt(VAddrBits.W))
      val data = Output(new ConfTabEntry(tagWidth, counterWidth))
    })
  })

  val bpRight = Wire(Vec(PredictWidth, Bool()))
  val bpWrong = Wire(Vec(PredictWidth, Bool()))
  val bpPc = Wire(Vec(PredictWidth, UInt(VAddrBits.W)))
  bpRight := DontCare
  bpWrong := DontCare
  bpPc := DontCare
  for (i <- 0 until PredictWidth) {
    ExcitingUtils.addSink(bpRight(i), s"bpRight_$i")
    ExcitingUtils.addSink(bpWrong(i), s"bpWrong_$i")
    ExcitingUtils.addSink(bpPc(i), s"bpPc_$i")
  }

  val data = Reg(Vec(confTableSet, Vec(confTableWay, new ConfTabEntry(tagWidth, counterWidth))))
  val lru = ReplacementPolicy.fromString("setlru", confTableWay, confTableSet)

  for (i <- 0 until RenameWidth) {
    val row = data(pcSetIdx(io.read(i).address))
    val hitVec = VecInit(row.map(e => e.valid && e.tag === pcTag(io.read(i).address)))
    val hit = hitVec.asUInt.orR
    val entry = row(OHToUInt(hitVec))
    io.read(i).data := entry
    io.read(i).data.valid := entry.valid && hit
    when (hit && io.read(i).enable) {
      lru.access(pcSetIdx(io.read(i).address), OHToUInt(hitVec))
    }
  }

  for (i <- 0 until PredictWidth) {
    val row = data(pcSetIdx(bpPc(i)))
    val hitVec = VecInit(row.map(e => e.valid && e.tag === pcTag(bpPc(i))))
    val hit = hitVec.asUInt.orR
    val allocate = lru.way(pcSetIdx(bpPc(i)))
    when (bpRight(i) || bpWrong(i)) {
      when (hit) {
        when (bpRight(i)) {
          row(OHToUInt(hitVec)).inc()
        }.otherwise {
          row(OHToUInt(hitVec)).reset()
        }
      }.otherwise {
        row(allocate).valid := true.B
        row(allocate).tag := pcTag(bpPc(i))
        when (bpRight(i)) {
          row(allocate).allocate()
        }.otherwise {
          row(allocate).allocate()
        }
        lru.access(pcSetIdx(bpPc(i)), allocate)
      }
    }
  }
  when (reset.asBool()) {
    data.map(_.map(_.valid := false.B))
  }

  XSPerfAccumulate("bpWrong", PopCount(bpWrong))
  XSPerfAccumulate("bpRight", PopCount(bpRight))
}
