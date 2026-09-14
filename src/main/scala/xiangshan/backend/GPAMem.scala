package xiangshan.backend

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import org.chipsalliance.cde.config.Parameters
import utility._
import utility.SyncDataModuleTemplate
import xiangshan.HasXSParameter
import xiangshan.frontend.{IfuToBackendIO}
import xiangshan.frontend.ftq.FtqPtr

class GPAMem(implicit p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false

  lazy val module = new GPAMemImp(this)
}

class GPAMemImp(override val wrapper: GPAMem)(implicit p: Parameters) extends LazyModuleImp(wrapper) with HasXSParameter {
  val io = IO(new GPAMemIO)

  private val mem = Module (new SyncDataModuleTemplate(new GPAMemEntry, FtqSize, numRead = 1, numWrite = 1, hasRen = true))

  mem.io.wen.head := io.fromIFU.gpAddrMem.wen
  mem.io.waddr.head := io.fromIFU.gpAddrMem.waddr
  mem.io.wdata.head := io.fromIFU.gpAddrMem.wdata

  mem.io.ren.get.head := io.exceptionReadAddr.valid
  mem.io.raddr.head := io.exceptionReadAddr.bits.ftqPtr.value

  private val ftqOffset = RegEnable(io.exceptionReadAddr.bits.ftqOffset, io.exceptionReadAddr.valid)
  private val isRVC     = RegEnable(io.exceptionReadAddr.bits.isRVC, io.exceptionReadAddr.valid)

  private val gpaFtqOffset = (ftqOffset << instOffsetBits).asUInt
  private val gpaRvcOffset = Mux(isRVC, 0.U, 2.U)
  private val gpaOffset    = SignExt(gpaFtqOffset -& gpaRvcOffset, PAddrBitsMax)

  private val gpabase = mem.io.rdata.head.gpaddr
  private val gpa = gpabase + gpaOffset

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
    val ftqOffset = UInt(FetchBlockInstOffsetWidth.W)
    val isRVC = Bool()
  }))
  val exceptionReadData = Output(new GPAMemEntry)
}
