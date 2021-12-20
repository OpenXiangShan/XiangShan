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

import freechips.rocketchip.tilelink.ClientMetadata
import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.L1CacheErrorInfo
import xiangshan.cache.CacheInstrucion._

class Meta(implicit p: Parameters) extends DCacheBundle {
  val coh = new ClientMetadata
}

object Meta {
  def apply(meta: UInt)(implicit p: Parameters) = {
    val m = Wire(new Meta)
    m.coh := meta.asTypeOf(new ClientMetadata)
    m
  }
}

class MetaAndTag(implicit p: Parameters) extends DCacheBundle {
  val meta = new Meta
  val tag = UInt(tagBits.W)
}

object MetaAndTag {
  def apply(coh: ClientMetadata, tag: UInt)(implicit p: Parameters) = {
    val x = Wire(new MetaAndTag)
    x.meta.coh := coh
    x.tag := tag
    x
  }
}

class MetaReadReq(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
}

class MetaWriteReq(implicit p: Parameters) extends MetaReadReq {
  val meta = new Meta
  val tag = UInt(tagBits.W) // used to calculate ecc
}

class ECCReadReq(implicit p: Parameters) extends MetaReadReq
class ECCWriteReq(implicit p: Parameters) extends ECCReadReq {
  val ecc = UInt()
}

class ErrorWriteReq(implicit p: Parameters) extends MetaReadReq {
  val error = Bool()
}

class AsynchronousECCArray(readPorts: Int, writePorts: Int)(implicit p: Parameters) extends DCacheModule {
  def metaAndTagOnReset = MetaAndTag(ClientMetadata.onReset, 0.U)

  // enc bits encode both tag and meta, but is saved in meta array
  val metaAndTagBits = metaAndTagOnReset.getWidth
  val encMetaAndTagBits = cacheParams.tagCode.width(metaAndTagBits)
  val encMetaBits = encMetaAndTagBits - tagBits
  val encBits = encMetaAndTagBits - metaAndTagBits

  val io = IO(new Bundle() {
    val read = Vec(readPorts, Flipped(ValidIO(new ECCReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, UInt(encBits.W))))
    val write = Vec(writePorts, Flipped(ValidIO(new ECCWriteReq)))
    val cacheOp = Flipped(new L1CacheInnerOpIO)
  })

  val ecc_array = Reg(Vec(nSets, Vec(nWays, UInt(encBits.W))))
  when (reset.asBool()) {
    ecc_array := 0.U.asTypeOf(ecc_array.cloneType)
  }

  io.read.zip(io.resp).foreach {
    case (read, resp) =>
      resp := RegEnable(ecc_array(read.bits.idx), read.valid)
  }

  io.write.foreach {
    case write =>
      write.bits.way_en.asBools.zipWithIndex.foreach {
        case (wen, i) =>
          when (write.valid && wen) {
            ecc_array(write.bits.idx)(i) := write.bits.ecc
          }
      }
  }

  // deal with customized cache op
  val cacheOpShouldResp = WireInit(false.B)
  when (io.cacheOp.req.valid) {
    when (isReadTagECC(io.cacheOp.req.bits.opCode)) {
      cacheOpShouldResp := true.B
    }
    when (isWriteTagECC(io.cacheOp.req.bits.opCode)) {
      ecc_array(io.cacheOp.req.bits.index)(io.cacheOp.req.bits.wayNum(4, 0)) :=
        io.cacheOp.req.bits.write_tag_ecc
      cacheOpShouldResp := true.B
    }
  }
  io.cacheOp.resp.valid := RegNext(io.cacheOp.req.valid && cacheOpShouldResp)
  io.cacheOp.resp.bits := DontCare
  io.cacheOp.resp.bits.read_tag_ecc := Mux(
    io.cacheOp.resp.valid,
    RegNext(ecc_array(io.cacheOp.req.bits.index)(io.cacheOp.req.bits.wayNum(4, 0))),
    0.U
  )
}

class AsynchronousMetaArray(readPorts: Int, writePorts: Int)(implicit p: Parameters) extends DCacheModule {
  def metaAndTagOnReset = MetaAndTag(ClientMetadata.onReset, 0.U)

  // enc bits encode both tag and meta, but is saved in meta array
  val metaAndTagBits = metaAndTagOnReset.getWidth
  val encMetaAndTagBits = cacheParams.tagCode.width(metaAndTagBits)
  val encMetaBits = encMetaAndTagBits - tagBits
  val encBits = encMetaAndTagBits - metaAndTagBits

  val io = IO(new Bundle() {
    val read = Vec(readPorts, Flipped(DecoupledIO(new MetaReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, UInt(encMetaBits.W))))
    val write = Vec(writePorts, Flipped(DecoupledIO(new MetaWriteReq)))
    // customized cache op port 
    val cacheOp = Flipped(new L1CacheInnerOpIO)
  })

  val meta_array = Reg(Vec(nSets, Vec(nWays, new Meta)))
  val ecc_array = Module(new AsynchronousECCArray(readPorts, writePorts))
  when (reset.asBool()) {
    meta_array := 0.U.asTypeOf(meta_array.cloneType)
  }

  (io.read.zip(io.resp)).zipWithIndex.foreach {
    case ((read, resp), i) =>
      read.ready := true.B
      ecc_array.io.read(i).valid := read.fire()
      ecc_array.io.read(i).bits := read.bits
      resp := VecInit(RegEnable(meta_array(read.bits.idx), read.valid).zip(
        ecc_array.io.resp(i)
      ).map { case (m, ecc) => Cat(ecc, m.asUInt) })
  }

  io.write.zip(ecc_array.io.write).foreach {
    case (write, ecc_write) =>
      write.ready := true.B
      val ecc = cacheParams.tagCode.encode(MetaAndTag(write.bits.meta.coh, write.bits.tag).asUInt)(encMetaAndTagBits - 1, metaAndTagBits)
      ecc_write.valid := write.fire()
      ecc_write.bits.idx := write.bits.idx
      ecc_write.bits.way_en := write.bits.way_en
      ecc_write.bits.ecc := ecc
      write.bits.way_en.asBools.zipWithIndex.foreach {
        case (wen, i) =>
          when (write.valid && wen) {
            meta_array(write.bits.idx)(i) := write.bits.meta
          }
      }
  }

  ecc_array.io.cacheOp <> io.cacheOp
}

class ErrorArray(readPorts: Int, writePorts: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val read = Vec(readPorts, Flipped(DecoupledIO(new MetaReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, Bool())))
    val write = Vec(writePorts, Flipped(DecoupledIO(new ErrorWriteReq)))
    // customized cache op port 
    // val cacheOp = Flipped(new L1CacheInnerOpIO)
  })

  val meta_array = Reg(Vec(nSets, Vec(nWays, Bool())))
  when (reset.asBool()) {
    meta_array := 0.U.asTypeOf(meta_array.cloneType)
  }

  io.read.zip(io.resp).foreach {
    case (read, resp) =>
      read.ready := true.B
      resp := RegEnable(meta_array(read.bits.idx), read.valid)
  }

  io.write.foreach {
    case write =>
      write.ready := true.B
      write.bits.way_en.asBools.zipWithIndex.foreach {
        case (wen, i) =>
          when (write.valid && wen) {
            meta_array(write.bits.idx)(i) := write.bits.error
          }
      }
  }
}
