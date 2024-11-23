package xiangshan.backend

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import utility.SyncDataModuleTemplate
import xiangshan.HasXSParameter
import xiangshan.frontend.{FtqPtr, IfuToBackendIO}

class GPAMem(implicit p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false

  lazy val module = new GPAMemImp(this)
}

class GPAMemImp(override val wrapper: GPAMem)(implicit p: Parameters) extends LazyModuleImp(wrapper) with HasXSParameter {
  val io = IO(new GPAMemIO)

  private val mem = Module (new SyncDataModuleTemplate(new GPAMemEntry, FtqSize, numRead = 1, numWrite = 1, hasRen = true))

  mem.io.wen.head := io.fromIFU.gpaddrMem_wen
  mem.io.waddr.head := io.fromIFU.gpaddrMem_waddr
  mem.io.wdata.head := io.fromIFU.gpaddrMem_wdata

  mem.io.ren.get.head := io.exceptionReadAddr.valid
  mem.io.raddr.head := io.exceptionReadAddr.bits.ftqPtr.value

  private val ftqOffset = RegEnable(io.exceptionReadAddr.bits.ftqOffset, io.exceptionReadAddr.valid)

  private val gpabase = mem.io.rdata.head.gpaddr
  private val gpa = gpabase + Cat(ftqOffset, 0.U(instOffsetBits.W))

  io.exceptionReadData.gpaddr := gpa
  io.exceptionReadData.isForVSnonLeafPTE := mem.io.rdata.head.isForVSnonLeafPTE

  def getGPAPage(vaddr: UInt): UInt = {
    require(vaddr.getWidth == GPAddrBits, s"The width of gpa should be $GPAddrBits")
    vaddr(GPAddrBits - 1, PageOffsetWidth)
  }
}

class GPAMemEntry(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val gpaddr = UInt(PAddrBitsMax.W)
  val isForVSnonLeafPTE = Bool()
}

class GPAMemIO(implicit val p: Parameters) extends Bundle with HasXSParameter {
  val fromIFU = Flipped(new IfuToBackendIO())

  val exceptionReadAddr = Input(ValidIO(new Bundle {
    val ftqPtr = new FtqPtr()
    val ftqOffset = UInt(log2Up(PredictWidth).W)
  }))
  val exceptionReadData = Output(new GPAMemEntry)
}
