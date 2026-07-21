/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
import freechips.rocketchip.tilelink.{ClientMetadata, TLClientParameters, TLEdgeOut}
import utility.{Code, ParallelOR, ReplacementPolicy, XSDebug}
import utility.sram.SRAMTemplate

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

  val waddr = (io.write.bits.addr >> blockOffBits).asUInt
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
      val wWayEn, rWayEn = Input(UInt(nWays.W))
      val rdata = Output(UInt())
    })

    val rWayEnReg = RegEnable(io.rWayEn, io.ren)
    val dataArray = Seq.fill(nWays) {
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
      val wen = io.wen && io.wWayEn(w)
      dataArray(w).io.w.req.valid := wen
      dataArray(w).io.w.req.bits.apply(
        setIdx = io.waddr,
        data = io.wdata,
        waymask = 1.U
      )
      dataArray(w).io.r.req.valid := io.ren
      dataArray(w).io.r.req.bits.apply(setIdx = io.raddr)
    }

    val half = nWays / 2
    val dataRead = dataArray.map(_.io.r.resp.data(0))
    val dataLeft = Mux1H(rWayEnReg.tail(half), dataRead.take(half))
    val dataRight = Mux1H(rWayEnReg.head(half), dataRead.drop(half))

    val selLow = rWayEnReg.tail(half).orR
    val rowData = Mux(selLow, dataLeft, dataRight)

    io.rdata := rowData
  }

  for (j <- 0 until 3) {
    val raddr = raddrs(j)
    val rmask = io.read(j).bits.rmask

    // for single port SRAM, do not allow read and write in the same cycle
    // for dual port SRAM, raddr === waddr is undefined behavior
    val rwhazard = if (singlePort) io.write.valid else io.write.valid && waddr === raddr
    io.read(j).ready := (if (readHighPriority) true.B else !rwhazard)

    // use wayEn to select a way after data read out
    assert(!(RegNext(io.read(j).fire && PopCount(io.read(j).bits.wayEn) > 1.U)))
    val wayEn = RegEnable(io.read(j).bits.wayEn, io.read(j).fire)

    val rowError = Wire(Vec(blockRows, Vec(rowWords, Bool())))
    for (r <- 0 until blockRows) {
      val eccArray = Module(new SRAMTemplate(
        Vec(rowWords, Bits(eccBits.W)),
        set = nSets,
        way = nWays,
        shouldReset = false,
        holdRead = false,
        singlePort = singlePort
      ))
      eccArray.io.w.req.valid := io.write.valid && io.write.bits.wmask(r)
      eccArray.io.w.req.bits.apply(
        setIdx = waddr,
        data = getECCFromRow(io.write.bits.data(r)),
        waymask = io.write.bits.wayEn
      )
      XSDebug(eccArray.io.w.req.valid,
        p"write in ecc sram ${j.U} row ${r.U}: setIdx=${Hexadecimal(eccArray.io.w.req.bits.setIdx)} ecc(0)=${Hexadecimal(getECCFromRow(io.write.bits.data(r))(0))} ecc(1)=${Hexadecimal(getECCFromRow(io.write.bits.data(r))(1))} waymask=${Hexadecimal(io.write.bits.wayEn)}\n")
      eccArray.io.r.req.valid := io.read(j).valid && rmask(r)
      eccArray.io.r.req.bits.apply(setIdx = raddr)

      val dataGroup = Module(new DataSRAMGroup)
      dataGroup.io.wen := io.write.valid && io.write.bits.wmask(r)
      dataGroup.io.wWayEn := io.write.bits.wayEn
      dataGroup.io.waddr := waddr
      dataGroup.io.wdata := io.write.bits.data(r)
      dataGroup.io.ren := io.read(j).valid && io.read(j).bits.rmask(r)
      dataGroup.io.rWayEn := io.read(j).bits.wayEn
      dataGroup.io.raddr := raddr

      val eccResp = Wire(Vec(rowWords, Vec(nWays, Bits(eccBits.W))))
      for(w <- 0 until nWays){
        for(k <- 0 until rowWords){
          eccResp(k)(w) := eccArray.io.r.resp.data(w)(k)
        }
      }
      val eccRespChosen = Wire(Vec(rowWords, Bits(eccBits.W)))
      val dataRespChosen = Wire(Vec(rowWords, Bits(wordBits.W)))
      dataRespChosen := dataGroup.io.rdata.asTypeOf(dataRespChosen)
      for (k <- 0 until rowWords) {
        eccRespChosen(k) := Mux1H(wayEn, eccResp(k))
      }
      io.resp(j)(r) := Cat((0 until rowWords).reverseIterator.map {
        k => {
          val data = Cat(eccRespChosen(k), dataRespChosen(k))
          rowError(r)(k) := dcacheParameters.dataCode.decode(data).error && RegNext(rmask(r))
          data
        }
      }.toSeq)
      io.errors(j).bits.report_to_beu := RegNext(io.read(j).fire) && Cat(rowError.flatten).orR
      io.errors(j).bits.paddr := RegEnable(io.read(j).bits.addr, io.read(j).fire)
    }

    io.nacks(j) := false.B
  }
}
