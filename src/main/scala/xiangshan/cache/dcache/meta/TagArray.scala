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
  val wayEn = UInt(nWays.W)
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

class TagSRAMBank(index: Int)(implicit p: Parameters) extends AbstractTagArray {
  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new TagReadReq {
      override val wayEn = UInt(DCacheWayDiv.W)
    }))
    val resp = Output(Vec(DCacheWayDiv, UInt(encTagBits.W)))
    val write = Flipped(DecoupledIO(new TagWriteReq {
      override val wayEn = UInt(DCacheWayDiv.W)
    }))
  })
  // TODO: reset is unnecessary?
  val rstCnt = RegInit(0.U(log2Up(nSets + 1).W))
  val rst = rstCnt < nSets.U
  val rstVal = 0.U
  val waddr = Mux(rst, rstCnt, io.write.bits.idx)
  val wdata = Mux(rst, rstVal, io.write.bits.asECCTag())
  val wmask = Mux(rst || (DCacheWayDiv == 1).B, (-1).asSInt, io.write.bits.wayEn.asSInt).asBools
  val rmask = Mux(rst || (DCacheWayDiv == 1).B, (-1).asSInt, io.read.bits.wayEn.asSInt).asBools
  when (rst) {
    rstCnt := rstCnt + 1.U
  }

  val tagArray = Module(new SRAMTemplate(UInt(encTagBits.W), set = nSets, way = DCacheWayDiv,
    shouldReset = false, holdRead = false, singlePort = true, withClockGate = true,
    hasMbist = hasMbist,  hasSramCtl = hasSramCtl, suffix = Some("dcsh_tag")))

  val wen = rst || io.write.valid
  io.write.ready := !rst
  tagArray.io.w.req.valid := wen
  tagArray.io.w.req.bits.apply(
    setIdx = waddr,
    data = wdata,
    waymask = VecInit(wmask).asUInt
  )

  // tag read
  val ren = io.read.fire
  io.read.ready := !wen
  tagArray.io.r.req.valid := ren
  tagArray.io.r.req.bits.apply(setIdx = io.read.bits.idx)
  io.resp := tagArray.io.r.resp.data

  XSPerfAccumulate("part_tag_read_counter_" + index, tagArray.io.r.req.valid)
}

class TagArray(implicit p: Parameters) extends AbstractTagArray {
  val io = IO(new Bundle() {
    val read = Flipped(DecoupledIO(new TagReadReq))
    val resp = Output(Vec(nWays, UInt(encTagBits.W)))
    val write = Flipped(DecoupledIO(new TagWriteReq))
  })

  val tagArrays = List.tabulate(nWays / DCacheWayDiv)(i => Module(new TagSRAMBank(i)))
  tagArrays.zipWithIndex.foreach { case (tagArray, i) =>
    tagArray.io.read <> io.read
    tagArray.io.read.bits.wayEn := io.read.bits.wayEn((i + 1) * DCacheWayDiv - 1, i * DCacheWayDiv)
    tagArray.io.write <> io.write
    tagArray.io.write.bits.wayEn := io.write.bits.wayEn((i + 1) * DCacheWayDiv - 1, i * DCacheWayDiv)
  }
  io.resp.zip(tagArrays.map(_.io.resp).flatten).foreach {
    case (resp, bank_resp) =>
      resp := bank_resp
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

  val tagReadOh = WireInit(VecInit(Seq.fill(readPorts)(0.U(XLEN.W))))
  for (i <- 0 until readPorts) {
    // normal read / write
    array(i).io.write.valid := io.write.valid
    array(i).io.write.bits.idx := io.write.bits.idx
    array(i).io.write.bits.wayEn := io.write.bits.wayEn
    array(i).io.write.bits.vaddr := io.write.bits.vaddr
    array(i).io.write.bits.tag := io.write.bits.tag
    array(i).io.write.bits.ecc := getECCFromEncTag(cacheParams.tagCode.encode(io.write.bits.tag))
    io.write.ready := true.B

    array(i).io.read <> io.read(i)
    io.read(i).ready := array(i).io.read.ready
    io.resp(i) := array(i).io.resp
    tagReadOh(i) := PopCount(array(i).io.read.fire)
  }

  XSPerfAccumulate("tag_read_counter", tagReadOh.reduce(_ + _))
}
