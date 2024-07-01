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

  require(params.numTargetReadPort == params.numPcMemReadPort, "The EXUs which need PC must be the same as the EXUs which need Target PC.")
  private val numTargetMemRead = params.numTargetReadPort + params.numPcMemReadPort

  val io = IO(new PcTargetMemIO())
  private val readValid = io.toDataPath.fromDataPathValid

  private def hasRen: Boolean = true
  private val targetMem = Module(new SyncDataModuleTemplate(new Ftq_RF_Components, FtqSize, numTargetMemRead, 1, hasRen = hasRen))
  private val targetPCVec : Vec[UInt] = Wire(Vec(params.numTargetReadPort, UInt(VAddrData().dataWidth.W)))
  private val pcVec       : Vec[UInt] = Wire(Vec(params.numPcMemReadPort, UInt(VAddrData().dataWidth.W)))

  targetMem.io.wen.head := GatedValidRegNext(io.fromFrontendFtq.pc_mem_wen)
  targetMem.io.waddr.head := RegEnable(io.fromFrontendFtq.pc_mem_waddr, io.fromFrontendFtq.pc_mem_wen)
  targetMem.io.wdata.head := RegEnable(io.fromFrontendFtq.pc_mem_wdata, io.fromFrontendFtq.pc_mem_wen)

  private val newestEn: Bool = io.fromFrontendFtq.newest_entry_en
  private val newestTarget: UInt = io.fromFrontendFtq.newest_entry_target
  for (i <- 0 until params.numTargetReadPort) {
    val targetPtr = io.toDataPath.fromDataPathFtqPtr(i)
    // target pc stored in next entry
    targetMem.io.ren.get(i) := readValid(i)
    targetMem.io.raddr(i) := (targetPtr + 1.U).value

    val needNewestTarget = RegEnable(targetPtr === io.fromFrontendFtq.newest_entry_ptr, false.B, readValid(i))
    targetPCVec(i) := Mux(
      needNewestTarget,
      RegEnable(newestTarget, newestEn),
      targetMem.io.rdata(i).startAddr
    )
  }

  for (i <- 0 until params.numPcMemReadPort) {
    val pcAddr = io.toDataPath.fromDataPathFtqPtr(i)
    val offset = io.toDataPath.fromDataPathFtqOffset(i)
    // pc stored in this entry
    targetMem.io.ren.get(i + params.numTargetReadPort) := readValid(i)
    targetMem.io.raddr(i + params.numTargetReadPort) := pcAddr.value
    pcVec(i) := targetMem.io.rdata(i + params.numTargetReadPort).getPc(RegEnable(offset, readValid(i)))
  }

  io.toDataPath.toDataPathTargetPC := targetPCVec
  io.toDataPath.toDataPathPC := pcVec
}

class PcToDataPathIO(params: BackendParams)(implicit p: Parameters) extends XSBundle {
  //Ftq
  val fromDataPathValid = Input(Vec(params.numPcMemReadPort, Bool()))
  val fromDataPathFtqPtr = Input(Vec(params.numPcMemReadPort, new FtqPtr))
  val fromDataPathFtqOffset = Input(Vec(params.numPcMemReadPort, UInt(log2Up(PredictWidth).W)))
  //Target PC
  val toDataPathTargetPC = Output(Vec(params.numTargetReadPort, UInt(VAddrData().dataWidth.W)))
  //PC
  val toDataPathPC = Output(Vec(params.numPcMemReadPort, UInt(VAddrData().dataWidth.W)))
}

class PcTargetMemIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
  //from frontend
  val fromFrontendFtq = Flipped(new FtqToCtrlIO)
  //to backend
  val toDataPath = new PcToDataPathIO(params)
}