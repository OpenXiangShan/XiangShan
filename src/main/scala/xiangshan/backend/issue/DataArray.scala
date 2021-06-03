/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._

class DataArrayReadIO(numEntries: Int, numSrc: Int, dataBits: Int)(implicit p: Parameters) extends XSBundle {
  val addr = Input(UInt(numEntries.W))
  val data = Vec(numSrc, Output(UInt(dataBits.W)))

  override def cloneType: DataArrayReadIO.this.type =
    new DataArrayReadIO(numEntries, numSrc, dataBits).asInstanceOf[this.type]
}

class DataArrayWriteIO(numEntries: Int, numSrc: Int, dataBits: Int)(implicit p: Parameters) extends XSBundle {
  val enable = Input(Bool())
  val mask   = Vec(numSrc, Input(Bool()))
  val addr   = Input(UInt(numEntries.W))
  val data   = Vec(numSrc, Input(UInt(dataBits.W)))

  override def cloneType: DataArrayWriteIO.this.type =
    new DataArrayWriteIO(numEntries, numSrc, dataBits).asInstanceOf[this.type]
}

class DataArrayMultiWriteIO(numEntries: Int, numSrc: Int, dataBits: Int)(implicit p: Parameters) extends XSBundle {
  val enable = Input(Bool())
  val addr   = Vec(numSrc, Input(UInt(numEntries.W)))
  val data   = Input(UInt(dataBits.W))

  override def cloneType: DataArrayMultiWriteIO.this.type =
    new DataArrayMultiWriteIO(numEntries, numSrc, dataBits).asInstanceOf[this.type]
}

class DataArrayIO(config: RSConfig)(implicit p: Parameters) extends XSBundle {
  val read = Vec(config.numDeq, new DataArrayReadIO(config.numEntries, config.numSrc, config.dataBits))
  val write = Vec(config.numEnq, new DataArrayWriteIO(config.numEntries, config.numSrc, config.dataBits))
  val multiWrite = Vec(config.numValueBroadCast, new DataArrayMultiWriteIO(config.numEntries, config.numSrc, config.dataBits))
  val delayedWrite = if (config.delayedRf) Vec(config.numEnq, Flipped(ValidIO(UInt(config.dataBits.W)))) else null

  override def cloneType: DataArrayIO.this.type =
    new DataArrayIO(config).asInstanceOf[this.type]
}

class DataArray(config: RSConfig)(implicit p: Parameters) extends XSModule {
  val io = IO(new DataArrayIO(config))

  // single array for each source
  def genSingleArray(raddr: Seq[UInt], wen: Seq[Bool], waddr: Seq[UInt], wdata: Seq[UInt]) = {
    val dataArray = Reg(Vec(config.numEntries, UInt(config.dataBits.W)))

    // write
    for (((en, addr), wdata) <- wen.zip(waddr).zip(wdata)) {
      dataArray.zipWithIndex.map { case (entry, i) =>
        when (en && addr(i)) {
          entry := wdata
        }
      }

      XSDebug(en, p"write ${Hexadecimal(wdata)} to address ${OHToUInt(addr)}\n")
    }

    // read
    val rdata = VecInit(raddr.map{ addr =>
      XSError(PopCount(addr) > 1.U, p"addr ${Binary(addr)} should be one-hot")
      Mux1H(addr, dataArray)
    })

    rdata
  }

  for (i <- 0 until config.numSrc) {
    val delayedWen = if (i == 1 && config.delayedRf) io.delayedWrite.map(_.valid) else Seq()
    val delayedWaddr = if (i == 1 && config.delayedRf) RegNext(VecInit(io.write.map(_.addr))) else Seq()
    val delayedWdata = if (i == 1 && config.delayedRf) io.delayedWrite.map(_.bits) else Seq()

    val wen = io.write.map(w => w.enable && w.mask(i)) ++ io.multiWrite.map(_.enable) ++ delayedWen
    val waddr = io.write.map(_.addr) ++ io.multiWrite.map(_.addr(i)) ++ delayedWaddr
    val wdata = io.write.map(_.data(i)) ++ io.multiWrite.map(_.data) ++ delayedWdata

    val rdata = genSingleArray(io.read.map(_.addr), wen, waddr, wdata)
    io.read.zip(rdata).map{ case (rport, data) => rport.data(i) := data }
  }

}
