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
  private val targetMem = Module(new SyncDataModuleTemplate(new PcTargetMemEntry, FtqSize, numTargetMemRead, 1, hasRen = hasRen))
  private val targetPCVec : Vec[UInt] = Wire(Vec(params.numTargetReadPort, UInt(VAddrData().dataWidth.W)))
  private val pcVec       : Vec[UInt] = Wire(Vec(params.numPcMemReadPort, UInt(VAddrData().dataWidth.W)))

  private val wdata = Wire(new PcTargetMemEntry)
  wdata.addr := io.fromFrontendFtq.pc_mem_wdata
  wdata.flag := io.fromFrontendFtq.pc_mem_waddr.flag

  targetMem.io.wen.head := GatedValidRegNext(io.fromFrontendFtq.pc_mem_wen)
  targetMem.io.waddr.head := RegEnable(io.fromFrontendFtq.pc_mem_waddr.value, io.fromFrontendFtq.pc_mem_wen)
  targetMem.io.wdata.head := RegEnable(wdata, io.fromFrontendFtq.pc_mem_wen)
  private val rdataVec = targetMem.io.rdata

  private val newestEn: Bool = io.fromFrontendFtq.newest_entry_en
  private val newestTarget: UInt = io.fromFrontendFtq.newest_entry_target

  // The FtqPtr is used to compare with read ptr since its arrival(T), so it needs to be holded and bypassed.
  private val currentNewestFtqPtr = DataHoldBypass(io.fromFrontendFtq.newest_entry_ptr, newestEn)
  // The newest target will be used at T+1, so there is no need to bypass it.
  private val currentNewestTarget = RegEnable(io.fromFrontendFtq.newest_entry_target, newestEn)
  private val targetReadPtrVec = 0 until params.numTargetReadPort map { i =>
    RegEnable(io.toDataPath.fromDataPathFtqPtr(i), io.toDataPath.fromDataPathValid(i))
  }

  private val canUpdate = WireInit(false.B)
  private val isJalTarget = io.fromFrontendFtq.newest_entry_is_jal_target
  private val jalTargetFtqPtr = RegEnable(io.fromFrontendFtq.newest_entry_ptr, canUpdate)
  private val jalTargetReg = RegEnable(io.fromFrontendFtq.newest_entry_target, canUpdate)
  canUpdate :=  isJalTarget || (io.fromFrontendFtq.newest_entry_ptr === jalTargetFtqPtr && newestEn)

  for (i <- 0 until params.numTargetReadPort) {
    val targetPtr = io.toDataPath.fromDataPathFtqPtr(i)
    // target pc stored in next entry
    targetMem.io.ren.get(i) := readValid(i)
    targetMem.io.raddr(i) := (targetPtr + 1.U).value

    val hitNewestFtqPtr = RegEnable(targetPtr === currentNewestFtqPtr, false.B, readValid(i))
    val useJalTarget = RegEnable(targetPtr === jalTargetFtqPtr, false.B, readValid(i))
    targetPCVec(i) := MuxCase(
      default = Fill(VAddrBits, 1.U(1.W)), // use all 1s as invalid predict jump target
      mapping = Seq(
        useJalTarget -> jalTargetReg,
        hitNewestFtqPtr -> currentNewestTarget,
        (rdataVec(i).flag === targetReadPtrVec(i).flag) -> rdataVec(i).addr.startAddr,
      )
    )
  }

  for (i <- 0 until params.numPcMemReadPort) {
    val pcAddr = io.toDataPath.fromDataPathFtqPtr(i)
    val offset = io.toDataPath.fromDataPathFtqOffset(i)
    // pc stored in this entry
    targetMem.io.ren.get(i + params.numTargetReadPort) := readValid(i)
    targetMem.io.raddr(i + params.numTargetReadPort) := pcAddr.value
    pcVec(i) := targetMem.io.rdata(i + params.numTargetReadPort).addr.getPc(RegEnable(offset, readValid(i)))
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

class PcTargetMemEntry(implicit p: Parameters) extends XSBundle {
  val addr = new Ftq_RF_Components
  val flag = Bool()
}
