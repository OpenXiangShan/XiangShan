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

package xiangshan.cache

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.{XSPerfAccumulate, ClockGate}
import utility.mbist.MbistPipeline
import utility.sram.SRAMTemplate

class TagReadReq(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
}

class TagWriteReq(implicit p: Parameters) extends TagReadReq {
  val vaddr = UInt(vtagBits.W)
  val tag = UInt(tagBits.W)
  val ecc = UInt(tagECCBits.W)

  def asECCTag() = {
    if (EnableTagEcc) {
      Cat(ecc, tag)
    } else {
      tag
    }
  }
}

class TagEccWriteReq(implicit p: Parameters) extends TagReadReq {
  val ecc = UInt(tagECCBits.W)
}

case object HasTagEccParam

abstract class AbstractTagArray(implicit p: Parameters) extends DCacheModule {
  val TagEccParam = if(EnableTagEcc) Some(HasTagEccParam) else None
}

class TagSRAMBank(index: Int, setBankIndex: Int)(implicit p: Parameters) extends AbstractTagArray {
  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new TagReadReq {
      override val way_en = UInt(DCacheWayDiv.W)
    }))
    val resp = Output(Vec(DCacheWayDiv, UInt(encTagBits.W)))
    val write = Flipped(DecoupledIO(new TagWriteReq {
      override val way_en = UInt(DCacheWayDiv.W)
    }))
  })

  private val setsPerBank = nSets / DCacheTagBanks
  private val bankSelectedForRead = set_to_tag_bank(io.read.bits.idx) === setBankIndex.U
  private val bankSelectedForWrite = set_to_tag_bank(io.write.bits.idx) === setBankIndex.U

  // TODO: reset is unnecessary?
  val rst_cnt = RegInit(0.U(log2Up(setsPerBank + 1).W))
  val rst = rst_cnt < setsPerBank.U
  val rstVal = 0.U
  val waddr = Mux(rst, rst_cnt, set_to_tag_bank_set(io.write.bits.idx))
  val wdata = Mux(rst, rstVal, io.write.bits.asECCTag())
  val wmask = Mux(rst || (DCacheWayDiv == 1).B, (-1).asSInt, io.write.bits.way_en.asSInt).asBools
  when (rst) {
    rst_cnt := rst_cnt + 1.U
  }

  val tag_array = Module(new SRAMTemplate(UInt(encTagBits.W), set = setsPerBank, way = DCacheWayDiv,
    shouldReset = false, holdRead = false, singlePort = true, withClockGate = true,
    hasMbist = hasMbist,  hasSramCtl = hasSramCtl, suffix = Some("dcsh_tag")))

  val wen = rst || (io.write.valid && bankSelectedForWrite)
  io.write.ready := !rst
  tag_array.io.w.req.valid := wen
  tag_array.io.w.req.bits.apply(
    setIdx = waddr,
    data = wdata,
    waymask = VecInit(wmask).asUInt
  )

  // tag read
  val ren = io.read.fire && bankSelectedForRead
  io.read.ready := !wen || !bankSelectedForRead
  tag_array.io.r.req.valid := ren
  tag_array.io.r.req.bits.apply(setIdx = set_to_tag_bank_set(io.read.bits.idx))
  io.resp := tag_array.io.r.resp.data

  XSPerfAccumulate("part_tag_read_counter_" + index, tag_array.io.r.req.valid)
}

class TagArray(implicit p: Parameters) extends AbstractTagArray {
  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new TagReadReq))
    val resp = Output(Vec(nWays, UInt(encTagBits.W)))
    val write = Flipped(DecoupledIO(new TagWriteReq))
  })

  val wayDivs = nWays / DCacheWayDiv
  val readBankSel = set_to_tag_bank(io.read.bits.idx)
  val writeBankSel = set_to_tag_bank(io.write.bits.idx)
  val readBankSelReg = RegInit(0.U((DCacheTagBankBits max 1).W))
  when (io.read.fire) {
    readBankSelReg := readBankSel
  }

  val tag_arrays = Seq.tabulate(wayDivs, DCacheTagBanks) { (wayDivIdx, setBankIdx) =>
    Module(new TagSRAMBank(wayDivIdx * DCacheTagBanks + setBankIdx, setBankIdx))
  }

  tag_arrays.zipWithIndex.foreach { case (setBankArrays, wayDivIdx) =>
    setBankArrays.foreach { tag_array =>
      tag_array.io.read.valid := io.read.valid
      tag_array.io.read.bits.idx := io.read.bits.idx
      tag_array.io.read.bits.way_en := io.read.bits.way_en((wayDivIdx + 1) * DCacheWayDiv - 1, wayDivIdx * DCacheWayDiv)

      tag_array.io.write.valid := io.write.valid
      tag_array.io.write.bits.idx := io.write.bits.idx
      tag_array.io.write.bits.way_en := io.write.bits.way_en((wayDivIdx + 1) * DCacheWayDiv - 1, wayDivIdx * DCacheWayDiv)
      tag_array.io.write.bits.vaddr := io.write.bits.vaddr
      tag_array.io.write.bits.tag := io.write.bits.tag
      tag_array.io.write.bits.ecc := io.write.bits.ecc
    }
  }

  io.read.ready := VecInit(tag_arrays.head.map(_.io.read.ready))(readBankSel)
  io.write.ready := VecInit(tag_arrays.head.map(_.io.write.ready))(writeBankSel)

  val bankedResp = Wire(Vec(wayDivs, Vec(DCacheWayDiv, UInt(encTagBits.W))))
  tag_arrays.zipWithIndex.foreach { case (setBankArrays, wayDivIdx) =>
    bankedResp(wayDivIdx) := VecInit(setBankArrays.map(_.io.resp))(readBankSelReg)
  }
  io.resp.zip(bankedResp.flatten).foreach {
    case (resp, bankResp) =>
      resp := bankResp
  }
}

class DuplicatedTagArray(readPorts: Int)(implicit p: Parameters) extends AbstractTagArray {
  val io = IO(new Bundle() {
    val read = Vec(readPorts, Flipped(DecoupledIO(new TagReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, UInt(encTagBits.W))))
    val write = Flipped(DecoupledIO(new TagWriteReq))
  })

  val array = Seq.fill(readPorts) { Module(new TagArray) }
  val mbistPl = MbistPipeline.PlaceMbistPipeline(1, s"MbistPipeDcacheTag", hasMbist)

  def getECCFromEncTag(encTag: UInt) = {
    if (EnableTagEcc) {
      require(encTag.getWidth == encTagBits, s"encTag=$encTag != encTagBits=$encTagBits!")
      encTag(encTagBits - 1, tagBits)
    } else {
      0.U
    }
  }

  val tag_read_oh = WireInit(VecInit(Seq.fill(readPorts)(0.U(XLEN.W))))
  for (i <- 0 until readPorts) {
    // normal read / write
    array(i).io.write.valid := io.write.valid
    array(i).io.write.bits.idx := io.write.bits.idx
    array(i).io.write.bits.way_en := io.write.bits.way_en
    array(i).io.write.bits.vaddr := io.write.bits.vaddr
    array(i).io.write.bits.tag := io.write.bits.tag
    array(i).io.write.bits.ecc := getECCFromEncTag(cacheParams.tagCode.encode(io.write.bits.tag))

    array(i).io.read <> io.read(i)
    io.read(i).ready := array(i).io.read.ready
    io.resp(i) := array(i).io.resp
    tag_read_oh(i) := PopCount(array(i).io.read.fire)
  }
  io.write.ready := array.head.io.write.ready

  XSPerfAccumulate("tag_read_counter", tag_read_oh.reduce(_ + _))
}
