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

class DataArrayIO(numEntries: Int, numSrc: Int, numRead: Int, numWrite: Int, numMultiWrite: Int, dataBits: Int)(implicit p: Parameters) extends XSBundle {
  val read = Vec(numRead, new DataArrayReadIO(numEntries, numSrc, dataBits))
  val write = Vec(numWrite, new DataArrayWriteIO(numEntries, numSrc, dataBits))
  val multiWrite = Vec(numMultiWrite, new DataArrayMultiWriteIO(numEntries, numSrc, dataBits))

  override def cloneType: DataArrayIO.this.type =
    new DataArrayIO(numEntries, numSrc, numRead, numWrite, numMultiWrite, dataBits).asInstanceOf[this.type]
}

class DataArray(numEntries: Int, numSrc: Int, numRead: Int, numWrite: Int, numMultiWrite: Int, dataBits: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new DataArrayIO(numEntries, numSrc, numRead, numWrite, numMultiWrite, dataBits))

  // single array for each source
  def genSingleArray(raddr: Seq[UInt], wen: Seq[Bool], waddr: Seq[UInt], wdata: Seq[UInt]) = {
    val dataArray = Reg(Vec(numEntries, UInt(dataBits.W)))

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

  for (i <- 0 until numSrc) {
    val wen = io.write.map(w => w.enable && w.mask(i)) ++ io.multiWrite.map(_.enable)
    val waddr = io.write.map(_.addr) ++ io.multiWrite.map(_.addr(i))
    val wdata = io.write.map(_.data(i)) ++ io.multiWrite.map(_.data)

    val rdata = genSingleArray(io.read.map(_.addr), wen, waddr, wdata)
    io.read.zip(rdata).map{ case (rport, data) => rport.data(i) := data }
  }

}
