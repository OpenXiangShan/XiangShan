package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.AsyncRawDataModuleTemplate
import utils.{XSDebug, XSError}
import xiangshan.XSModule

class OHReadBundle[T <: Data](addrLen: Int, gen: T) extends Bundle {
  val addr = Input(UInt(addrLen.W))
  val data = Output(gen)
}

class OHWriteBundle[T <: Data](addrLen: Int, gen: T) extends Bundle {
  val en = Input(Bool())
  val addr = Input(UInt(addrLen.W))
  val data = Input(gen)
}

class DataArrayIO[T <: Data](gen: T, numRead: Int, numWrite: Int, numEntries: Int) extends Bundle {
  val read = Vec(numRead, new OHReadBundle(numEntries, gen))
  val write = Vec(numWrite, new OHWriteBundle(numEntries, gen))
}

class DataArray[T <: Data](gen: T, numRead: Int, numWrite: Int, numEntries: Int)
  (implicit p: Parameters)
  extends XSModule {

  val io = IO(new DataArrayIO(gen, numRead, numWrite, numEntries))

  private val dataModule = Module(new AsyncRawDataModuleTemplate(gen, numEntries, io.read.length, io.write.length))

  dataModule.io.rvec  := VecInit(io.read.map(_.addr))
  io.read.zip(dataModule.io.rdata).foreach { case (l, r) => l.data := r}

  dataModule.io.wvec  := VecInit(io.write.map(_.addr))
  dataModule.io.wen   := VecInit(io.write.map(_.en))
  dataModule.io.wdata := VecInit(io.write.map(_.data))

  // check if one entry wroten by multi bundles
  for (i <- 0 until numEntries) {
    val wCnt = VecInit(io.write.indices.map(j => dataModule.io.wen(j) && dataModule.io.wvec(j)(i)))
    XSError(RegNext(PopCount(wCnt) > 1.U), s"why not OH $i?")
    when(PopCount(wCnt) > 1.U) {
      XSDebug("ERROR: IssueQueue DataArray write overlap!\n")
    }
  }
}
