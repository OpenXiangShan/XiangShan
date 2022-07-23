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

class MetaReadReq(implicit p: Parameters) extends DCacheBundle {
  val idx = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
}

class MetaWriteReq(implicit p: Parameters) extends MetaReadReq {
  val meta = new Meta
}

class ErrorWriteReq(implicit p: Parameters) extends MetaReadReq {
  val error = Bool()
}

class AsynchronousMetaArray(readPorts: Int, writePorts: Int)(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle() {
    val read = Vec(readPorts, Flipped(DecoupledIO(new MetaReadReq)))
    val resp = Output(Vec(readPorts, Vec(nWays, new Meta)))
    val write = Vec(writePorts, Flipped(DecoupledIO(new MetaWriteReq)))
  })

  val meta_array = Reg(Vec(nSets, Vec(nWays, new Meta)))
  when (reset.asBool()) {
    meta_array := 0.U.asTypeOf(meta_array.cloneType)
  }

  (io.read.zip(io.resp)).zipWithIndex.foreach {
    case ((read, resp), i) =>
      read.ready := true.B
      resp := RegEnable(meta_array(read.bits.idx), read.valid)
  }

  io.write.foreach {
    case write =>
      write.ready := true.B
      write.bits.way_en.asBools.zipWithIndex.foreach {
        case (wen, i) =>
          when (write.valid && wen) {
            meta_array(write.bits.idx)(i) := write.bits.meta
          }
      }
  }
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
      // resp := RegEnable(meta_array(read.bits.idx), read.valid)
      resp := meta_array(RegEnable(read.bits.idx, read.valid))
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
