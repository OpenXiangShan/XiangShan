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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.{ClientMetadata, TLClientParameters, TLEdgeOut}
import utility.{Code, ParallelOR, ReplacementPolicy, SRAMTemplate}
import utils.XSDebug

import scala.math.max

class DuplicatedDataArray(implicit p: Parameters) extends AbstractDataArray {
  val singlePort = true
  val readHighPriority = false

  def getECCFromEncWord(encWord: UInt) = {
    require(encWord.getWidth == encWordBits)
    encWord(encWordBits - 1, wordBits)
  }

  def getECCFromRow(row: UInt) = {
    require(row.getWidth == rowBits)
    VecInit((0 until rowWords).map { w =>
      val word = row(wordBits * (w + 1) - 1, wordBits * w)
      getECCFromEncWord(cacheParams.dataCode.encode(word))
    })
  }

  val waddr = (io.write.bits.addr >> blockOffBits).asUInt()
  val raddrs = io.read.map(r => (r.bits.addr >> blockOffBits).asUInt)
  io.write.ready := (if (readHighPriority) {
    if (singlePort) {
      !VecInit(io.read.map(_.valid)).asUInt.orR
    } else {
      !(Cat(io.read.zipWithIndex.map { case (r, i) => r.valid && raddrs(i) === waddr }).orR)
    }
  } else {
    true.B
  })

  // wrap a data row and a ecc row
  class DataSRAMGroup extends Module {
    val io = IO(new Bundle() {
      val wen, ren = Input(Bool())
      val waddr, raddr = Input(UInt())
      val wdata = Input(UInt(rowBits.W))
      val w_way_en, r_way_en = Input(UInt(nWays.W))
      val rdata = Output(UInt())
    })

    val r_way_en_reg = RegNext(io.r_way_en)
    val data_array = Array.fill(nWays) {
      Module(new SRAMTemplate(
        Bits(rowBits.W),
        set = nSets,
        way = 1,
        shouldReset = false,
        holdRead = false,
        singlePort = singlePort
      ))
    }

    for (w <- 0 until nWays) {
      val wen = io.wen && io.w_way_en(w)
      data_array(w).io.w.req.valid := wen
      data_array(w).io.w.req.bits.apply(
        setIdx = io.waddr,
        data = io.wdata,
        waymask = 1.U
      )
      data_array(w).io.r.req.valid := io.ren
      data_array(w).io.r.req.bits.apply(setIdx = io.raddr)
    }

    val half = nWays / 2
    val data_read = data_array.map(_.io.r.resp.data(0))
    val data_left = Mux1H(r_way_en_reg.tail(half), data_read.take(half))
    val data_right = Mux1H(r_way_en_reg.head(half), data_read.drop(half))

    val sel_low = r_way_en_reg.tail(half).orR()
    val row_data = Mux(sel_low, data_left, data_right)

    io.rdata := row_data
  }

  for (j <- 0 until 3) {
    val raddr = raddrs(j)
    val rmask = io.read(j).bits.rmask

    // for single port SRAM, do not allow read and write in the same cycle
    // for dual port SRAM, raddr === waddr is undefined behavior
    val rwhazard = if (singlePort) io.write.valid else io.write.valid && waddr === raddr
    io.read(j).ready := (if (readHighPriority) true.B else !rwhazard)

    // use way_en to select a way after data read out
    assert(!(RegNext(io.read(j).fire() && PopCount(io.read(j).bits.way_en) > 1.U)))
    val way_en = RegNext(io.read(j).bits.way_en)

    val row_error = Wire(Vec(blockRows, Vec(rowWords, Bool())))
    for (r <- 0 until blockRows) {
      val ecc_array = Module(new SRAMTemplate(
        Vec(rowWords, Bits(eccBits.W)),
        set = nSets,
        way = nWays,
        shouldReset = false,
        holdRead = false,
        singlePort = singlePort
      ))
      ecc_array.io.w.req.valid := io.write.valid && io.write.bits.wmask(r)
      ecc_array.io.w.req.bits.apply(
        setIdx = waddr,
        data = getECCFromRow(io.write.bits.data(r)),
        waymask = io.write.bits.way_en
      )
      when(ecc_array.io.w.req.valid) {
        XSDebug(p"write in ecc sram ${j.U} row ${r.U}: setIdx=${Hexadecimal(ecc_array.io.w.req.bits.setIdx)} ecc(0)=${Hexadecimal(getECCFromRow(io.write.bits.data(r))(0))} ecc(1)=${Hexadecimal(getECCFromRow(io.write.bits.data(r))(1))} waymask=${Hexadecimal(io.write.bits.way_en)}\n")
      }
      ecc_array.io.r.req.valid := io.read(j).valid && rmask(r)
      ecc_array.io.r.req.bits.apply(setIdx = raddr)

      val dataGroup = Module(new DataSRAMGroup)
      dataGroup.io.wen := io.write.valid && io.write.bits.wmask(r)
      dataGroup.io.w_way_en := io.write.bits.way_en
      dataGroup.io.waddr := waddr
      dataGroup.io.wdata := io.write.bits.data(r)
      dataGroup.io.ren := io.read(j).valid && io.read(j).bits.rmask(r)
      dataGroup.io.r_way_en := io.read(j).bits.way_en
      dataGroup.io.raddr := raddr

      val ecc_resp = Wire(Vec(rowWords, Vec(nWays, Bits(eccBits.W))))
      for(w <- 0 until nWays){
        for(k <- 0 until rowWords){
          ecc_resp(k)(w) := ecc_array.io.r.resp.data(w)(k)
        }
      }
      val ecc_resp_chosen = Wire(Vec(rowWords, Bits(eccBits.W)))
      val data_resp_chosen = Wire(Vec(rowWords, Bits(wordBits.W)))
      data_resp_chosen := dataGroup.io.rdata.asTypeOf(data_resp_chosen)
      for (k <- 0 until rowWords) {
        ecc_resp_chosen(k) := Mux1H(way_en, ecc_resp(k))
      }
      io.resp(j)(r) := Cat((0 until rowWords) reverseMap {
        k => {
          val data = Cat(ecc_resp_chosen(k), data_resp_chosen(k))
          row_error(r)(k) := dcacheParameters.dataCode.decode(data).error && RegNext(rmask(r))
          data
        }
      })
      io.errors(j).report_to_beu := RegNext(io.read(j).fire()) && Cat(row_error.flatten).orR()
      io.errors(j).paddr := RegNext(io.read(j).bits.addr)
    }

    io.nacks(j) := false.B
  }
}
