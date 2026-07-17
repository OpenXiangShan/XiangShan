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

class L1DBPSampleWrite(params: L1DBPParams)(implicit p: Parameters) extends DCacheBundle {
  val set = UInt((idxBits - params.sampleBits).W)
  val wayEn = UInt(nWays.W)
  val entry = new L1DBPSampleEntry(params)
}

class L1DBPSampleArray(val l1dbpParams: L1DBPParams)(implicit p: Parameters)
  extends DCacheModule with HasL1DBPParameters {
  val io = IO(new Bundle {
    val read = Flipped(ValidIO(UInt((idxBits - l1dbpParams.sampleBits).W)))
    val resp = Output(Vec(nWays, new L1DBPSampleEntry(l1dbpParams)))
    val write = Flipped(ValidIO(new L1DBPSampleWrite(l1dbpParams)))
  })

  val array = Module(new SRAMTemplate(
    new L1DBPSampleEntry(l1dbpParams),
    set = l1dbpNumSampleSets,
    way = nWays,
    shouldReset = false,
    holdRead = false,
    singlePort = true,
    withClockGate = true,
    hasMbist = hasMbist,
    hasSramCtl = hasSramCtl,
    suffix = Some("dcsh_l1dbp")
  ))

  array.io.r.req.valid := io.read.valid
  array.io.r.req.bits.apply(setIdx = io.read.bits)
  array.io.w.req.valid := io.write.valid
  array.io.w.req.bits.apply(
    data = io.write.bits.entry,
    setIdx = io.write.bits.set,
    waymask = io.write.bits.wayEn
  )
  io.resp := array.io.r.resp.data
}
