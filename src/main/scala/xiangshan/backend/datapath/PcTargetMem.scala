package xiangshan.backend

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility._
import xiangshan._
import xiangshan.backend.datapath.DataConfig.VAddrData
import xiangshan.frontend.{FtqPtr, FtqToCtrlIO, Ftq_RF_Components}

class PcTargetMem(params: BackendParams)(implicit p: Parameters) extends LazyModule {
  override def shouldBeInlined: Boolean = false

  lazy val module = new PcTargetMemImp(this)(p, params)
}

class PcTargetMemImp(override val wrapper: PcTargetMem)(implicit p: Parameters, params: BackendParams) extends LazyModuleImp(wrapper) with HasXSParameter {

  private val numTargetMemRead = params.numTargetReadPort + params.numPcMemReadPort
  val io = IO(new PcTargetMemIO())

  private def hasRen: Boolean = true
  private val targetMem = Module(new SyncDataModuleTemplate(new Ftq_RF_Components, FtqSize, numTargetMemRead, 1, hasRen = hasRen))
  private val jumpTargetReadVec : Vec[UInt] = Wire(Vec(params.numTargetReadPort, UInt(VAddrData().dataWidth.W)))
  private val jumpTargetVec     : Vec[UInt] = Wire(Vec(params.numTargetReadPort, UInt(VAddrData().dataWidth.W)))

  targetMem.io.wen.head := RegNext(io.fromFrontendFtq.pc_mem_wen)
  targetMem.io.waddr.head := RegEnable(io.fromFrontendFtq.pc_mem_waddr, io.fromFrontendFtq.pc_mem_wen)
  targetMem.io.wdata.head := RegEnable(io.fromFrontendFtq.pc_mem_wdata, io.fromFrontendFtq.pc_mem_wen)

  private val newestEn: Bool = io.fromFrontendFtq.newest_entry_en
  private val newestTarget: UInt = io.fromFrontendFtq.newest_entry_target
  for (i <- 0 until params.numTargetReadPort) {
    val targetVld = io.fromDataPathVld(i)
    val targetPtr = io.fromDataPathFtq(i)
    // target pc stored in next entry
    targetMem.io.ren.get(i) := targetVld
    targetMem.io.raddr(i) := (targetPtr + 1.U).value
    jumpTargetReadVec(i) := targetMem.io.rdata(i).startAddr
    val needNewestTarget = RegNext(targetPtr === io.fromFrontendFtq.newest_entry_ptr)
    jumpTargetVec(i) := Mux(
      needNewestTarget,
      RegEnable(newestTarget, newestEn),
      jumpTargetReadVec(i)
    )
  }
  private val pcReadVec = Wire(Vec(params.numPcMemReadPort, UInt(VAddrData().dataWidth.W)))
  private val pcVec = Wire(Vec(params.numPcMemReadPort, UInt(VAddrData().dataWidth.W)))
  for (i <- 0 until params.numPcMemReadPort) {
    val vld = io.pcToDataPath.fromDataPathFtqVld(i)
    val pcAddr = io.pcToDataPath.fromDataPathFtqPtr(i)
    // pc stored in this entry
    val offset = io.pcToDataPath.fromDataPathFtqOffset(i)
    targetMem.io.ren.get(i + params.numTargetReadPort) := vld
    targetMem.io.raddr(i + params.numTargetReadPort) := pcAddr.value
    pcReadVec(i) := targetMem.io.rdata(i + params.numTargetReadPort).getPc(RegNext(offset))
    pcVec(i) := pcReadVec(i)
  }
  io.pcToDataPath.toDataPathPC := pcVec
  io.toExus := jumpTargetVec

}

class PcToDataPathIO(params: BackendParams)(implicit p: Parameters) extends XSBundle {
  val toDataPathPC = Output(Vec(params.numPcMemReadPort, UInt(VAddrData().dataWidth.W)))
  val fromDataPathFtqVld = Input(Vec(params.numPcMemReadPort, Bool()))
  val fromDataPathFtqPtr = Input(Vec(params.numPcMemReadPort, new FtqPtr))
  val fromDataPathFtqOffset = Input(Vec(params.numPcMemReadPort, UInt(log2Up(PredictWidth).W)))
}

class PcTargetMemIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
  //input
  val fromFrontendFtq = Flipped(new FtqToCtrlIO)
  val fromDataPathVld = Input(Vec(params.numTargetReadPort, Bool()))
  val fromDataPathFtq = Input(Vec(params.numTargetReadPort, new FtqPtr))
  //output
  val toExus = Output(Vec(params.numTargetReadPort, UInt(VAddrData().dataWidth.W)))
  val pcToDataPath = new PcToDataPathIO(params)
}