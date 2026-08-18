/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 ***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.sram.SRAMTemplate

class L1DBPSampleEntry(implicit p: Parameters) extends DCacheBundle {
  val valid = Bool()
  val payload = UInt(l1dbpPcIndexWidth.W)
}

class L1DBPPredictionEntry(implicit p: Parameters) extends DCacheBundle {
  val valid = Bool()
  val dead = Bool()
}

class L1DBPPredictionWrite(implicit p: Parameters) extends DCacheBundle {
  val set = UInt((idxBits - l1dbpSampleBits).W)
  val wayEn = UInt(nWays.W)
  val entry = new L1DBPPredictionEntry
}

class L1DSampledPCRead(implicit p: Parameters) extends DCacheBundle {
  val set = UInt((idxBits - l1dbpSampleBits).W)
}

class L1DSampledPCWrite(implicit p: Parameters) extends DCacheBundle {
  val set = UInt((idxBits - l1dbpSampleBits).W)
  val wayEn = UInt(nWays.W)
  val entry = new L1DBPSampleEntry
}

class L1DBPPredictionArray(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val readSet = Input(UInt((idxBits - l1dbpSampleBits).W))
    val resp = Output(Vec(nWays, new L1DBPPredictionEntry))
    val write = Flipped(ValidIO(new L1DBPPredictionWrite))
  })

  val array = RegInit(VecInit(Seq.fill(l1dbpNumSampleSets)(
    VecInit(Seq.fill(nWays)(0.U.asTypeOf(new L1DBPPredictionEntry)))
  )))

  io.resp := array(io.readSet)
  when (io.write.valid) {
    for (way <- 0 until nWays) {
      when (io.write.bits.wayEn(way)) {
        array(io.write.bits.set)(way).valid := io.write.bits.entry.valid
        array(io.write.bits.set)(way).dead := io.write.bits.entry.dead
      }
    }
  }
}

class L1DSampledPCArray(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val read = Flipped(DecoupledIO(new L1DSampledPCRead))
    val resp = Output(Vec(nWays, new L1DBPSampleEntry))
    val write = Flipped(ValidIO(new L1DSampledPCWrite))
  })

  val array = Module(new SRAMTemplate(
    new L1DBPSampleEntry,
    set = l1dbpNumSampleSets,
    way = nWays,
    shouldReset = false,
    holdRead = false,
    singlePort = true,
    withClockGate = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl
  ))

  io.resp := array.io.r(io.read.fire, io.read.bits.set).resp.data
  array.io.w(
    io.write.valid,
    io.write.bits.entry,
    io.write.bits.set,
    io.write.bits.wayEn
  )
  io.read.ready := !io.write.valid
}
