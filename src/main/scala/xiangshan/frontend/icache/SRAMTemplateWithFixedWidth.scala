// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.icache

import chisel3._
import utility.sram.SRAMReadBus
import utility.sram.SRAMTemplate
import utility.sram.SRAMWriteBus

// FIXME: should move to utility

// Automatically partition the SRAM based on the width of the data and the desired width.
// final SRAM width = width * way
class SRAMTemplateWithFixedWidth[T <: Data](
    gen:           T,
    set:           Int,
    width:         Int,
    way:           Int = 1,
    shouldReset:   Boolean = false,
    holdRead:      Boolean = false,
    singlePort:    Boolean = false,
    bypassWrite:   Boolean = false,
    withClockGate: Boolean = false,
    hasMbist:      Boolean = false
) extends Module {
  class SRAMTemplateWithFixedWidthIO[TT <: Data](gen: TT, set: Int, way: Int) extends Bundle {
    val r: SRAMReadBus[TT]  = Flipped(new SRAMReadBus(gen, set, way))
    val w: SRAMWriteBus[TT] = Flipped(new SRAMWriteBus(gen, set, way))
  }
  val io: SRAMTemplateWithFixedWidthIO[T] = IO(new SRAMTemplateWithFixedWidthIO(gen, set, way))

  private def dataBits  = gen.getWidth
  private def bankNum   = math.ceil(dataBits.toDouble / width.toDouble).toInt
  private def totalBits = bankNum * width

  private val wordType = UInt(width.W)
  private val writeDatas = (0 until bankNum).map { bank =>
    VecInit((0 until way).map { i =>
      io.w.req.bits.data(i).asTypeOf(UInt(totalBits.W)).asTypeOf(Vec(bankNum, wordType))(bank)
    })
  }

  private val srams = (0 until bankNum) map { bank =>
    val sramBank = Module(new SRAMTemplate(
      wordType,
      set = set,
      way = way,
      shouldReset = shouldReset,
      holdRead = holdRead,
      singlePort = singlePort,
      bypassWrite = bypassWrite,
      withClockGate = withClockGate,
      hasMbist = hasMbist
    ))
    // read req
    sramBank.io.r.req.valid       := io.r.req.valid
    sramBank.io.r.req.bits.setIdx := io.r.req.bits.setIdx

    // write req
    sramBank.io.w.req.valid       := io.w.req.valid
    sramBank.io.w.req.bits.setIdx := io.w.req.bits.setIdx
    sramBank.io.w.req.bits.data   := writeDatas(bank)
    sramBank.io.w.req.bits.waymask.foreach(_ := io.w.req.bits.waymask.get)

    sramBank
  }

  io.r.req.ready := !io.w.req.valid
  (0 until way).foreach { i =>
    io.r.resp.data(i) := VecInit((0 until bankNum).map(bank =>
      srams(bank).io.r.resp.data(i)
    )).asTypeOf(UInt(totalBits.W))(dataBits - 1, 0).asTypeOf(gen.cloneType)
  }

  io.r.req.ready := srams.head.io.r.req.ready
  io.w.req.ready := srams.head.io.w.req.ready
}
