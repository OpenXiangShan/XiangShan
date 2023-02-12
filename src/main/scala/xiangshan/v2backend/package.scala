package xiangshan

import chisel3._
import chisel3.util._
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.{CertainLatency, HasFuLatency, UncertainLatency}

// Todo: split it into other config files
package object v2backend {
  sealed trait WBSource
  case class WBFromInt() extends WBSource
  case class WBFromMem() extends WBSource
  case class WBFromVec() extends WBSource
  case class WBFromFp()  extends WBSource

  sealed trait WBSink
  case class WBToInt() extends WBSink
  case class WBToFp()  extends WBSink
  case class WBToVec() extends WBSink

  case class WriteBackConfig(
    wbSource: SchedulerType,
    wbPortCfg: WBPortConfig,
  ) {
    
  }

  object FuType {
    private def OHInt(n: Int) = {
      require(n < 63)
      1 << n
    }
    val jmp = OHInt(0)
    val brh = OHInt(1)
    val i2f = OHInt(2)
    val csr = OHInt(3)
    val alu = OHInt(4)
    val mul = OHInt(5)
    val div = OHInt(6)
    val fence = OHInt(7)
    val bku = OHInt(8)
    val vset = OHInt(9)
    val fmac = OHInt(10)
    val fmisc = OHInt(11)
    val fDivSqrt = OHInt(12)
    val ldu = OHInt(13)
    val stu = OHInt(14)
    val mou = OHInt(15)
    val vipu = OHInt(16)
    val vfpu = OHInt(17)
    val vldu = OHInt(18)
    val vstu = OHInt(19)

    def X = BitPat.dontCare(num)

    def num = 20

    def apply() = UInt(num.W)

    val functionNameMap = Map(
      jmp -> "jmp",
      i2f -> "int_to_float",
      csr -> "csr",
      alu -> "alu",
      mul -> "mul",
      div -> "div",
      fence -> "fence",
      bku -> "bku",
      fmac -> "fmac",
      fmisc -> "fmisc",
      fDivSqrt -> "fdiv_fsqrt",
      ldu -> "load",
      stu -> "store",
      mou -> "mou"
    )
  }

  case class DataConfig (
    name: String,
    dataWidth: Int,
  ) {
    override def toString: String = name
  }

  val IntData = DataConfig("int", 64)
  val FpData = DataConfig("fp", 64)
  val VecData = DataConfig("vec", 128)
  val ImmData = DataConfig("imm", 64)
  val AddrData = DataConfig("addr", 39)
  val VCfgData = DataConfig("vcfg", 2 + 3 + 1 + 1 + 1 + log2Up(VecData.dataWidth)) // 16: vsew + vlmul + vma + vta + vill + vl
  val MaskSrcData = DataConfig("masksrc", VecData.dataWidth) // 128
  val MaskDstData = DataConfig("maskdst", VecData.dataWidth / 8) // 16
  val RegSrcDatas = Set(IntData, FpData, VecData, MaskSrcData)

  case class FuConfig (
    fuType: Int,
    srcData: Seq[Seq[DataConfig]],
    writeIntRf: Boolean,
    writeFpRf: Boolean,
    writeVecRf: Boolean = false,
    writeFflags: Boolean = false,
    dataBits: Int = 64,
    hasRedirect: Boolean = false,
    latency: HasFuLatency = CertainLatency(0),
    hasInputBuffer: (Boolean, Int, Boolean) = (false, 0, false),
    exceptionOut: Seq[Int] = Seq(),
    hasLoadError: Boolean = false,
    flushPipe: Boolean = false,
    replayInst: Boolean = false,
    trigger: Boolean = false,
  ) {
    def numIntSrc: Int = srcData.map(_.count(_ == IntData)).max
    def numFpSrc: Int = srcData.map(_.count(_ == FpData)).max
    def numVecSrc: Int = srcData.map(_.count(x => x == VecData || x == MaskSrcData)).max
    def numSrcMax: Int = srcData.map(_.length).max

    /**
      * params(i): data type set of the ith src port
      * @return
      */
    def getRfReadDataCfgSet: Seq[Set[DataConfig]] = {
      val numSrcMax = srcData.map(_.length).max
      // make srcData is uniform sized to avoid exception when transpose
      val alignedSrcData: Seq[Seq[DataConfig]] = srcData.map(x => x ++ Seq.fill(numSrcMax - x.length)(null))
      alignedSrcData.transpose.map(_.toSet.intersect(RegSrcDatas))
    }
  }

  def JmpCfg: FuConfig = FuConfig (
    fuType = FuType.jmp,
    srcData = Seq(
      Seq(IntData, ImmData), // jalr
      Seq(IntData, AddrData),// auipc
    ),
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = true,
  )

  def BrhCfg = FuConfig (
    fuType = FuType.brh,
    srcData = Seq(
      Seq(IntData, IntData),
    ),
    writeIntRf = false,
    writeFpRf = false,
    hasRedirect = true,
  )

  def I2fCfg = FuConfig (
    FuType.i2f,
    srcData = Seq(
      Seq(IntData),
    ),
    writeIntRf = false,
    writeFpRf = true,
    writeFflags = true,
    latency = CertainLatency(2),
  )

  def CsrCfg = FuConfig (
    fuType = FuType.csr,
    srcData = Seq(
      Seq(IntData),
    ),
    writeIntRf = true,
    writeFpRf = false,
    exceptionOut = Seq(illegalInstr, breakPoint, ecallU, ecallS, ecallM),
    flushPipe = true
  )

  def AluCfg: FuConfig = FuConfig (
    fuType = FuType.alu,
    srcData = Seq(
      Seq(IntData, IntData),
    ),
    writeIntRf = true,
    writeFpRf = false,
  )

  def MulCfg = FuConfig (
    fuType = FuType.mul,
    srcData = Seq(
      Seq(IntData, IntData),
    ),
    writeIntRf = true,
    writeFpRf = false,
    latency = CertainLatency(2),
  )

  def DivCfg = FuConfig (
    fuType = FuType.div,
    srcData = Seq(
      Seq(IntData, IntData),
    ),
    writeIntRf = true,
    writeFpRf = false,
    latency = UncertainLatency(),
    hasInputBuffer = (true, 4, true)
  )

  def FenceCfg: FuConfig = FuConfig (
    FuType.fence,
    srcData = Seq(
      Seq(IntData, IntData),
    ),
    writeIntRf = false,
    writeFpRf = false,
    latency = UncertainLatency(),
    exceptionOut = Seq(illegalInstr),
    flushPipe = true
  )

  // Todo: split it to simple bitmap exu and complex bku
  def BkuCfg = FuConfig (
    fuType = FuType.bku,
    srcData = Seq(
      Seq(IntData, IntData),
    ),
    writeIntRf = true,
    writeFpRf = false,
    latency = CertainLatency(1),
  )

  def VsetCfg = FuConfig (
    fuType = FuType.vset,
    srcData = Seq(
      Seq(IntData, IntData),
    ),
    writeIntRf = true,
    writeFpRf = false,
    latency = CertainLatency(0)
  )

  def FmacCfg = FuConfig (
    fuType = FuType.fmac,
    srcData = Seq(
      Seq(FpData, FpData),
      Seq(FpData, FpData, FpData),
    ),
    writeIntRf = false,
    writeFpRf = true,
    writeFflags = true,
    latency = UncertainLatency(),
  )

  def F2iCfg = FuConfig (
    fuType = FuType.fmisc,
    srcData = Seq(
      Seq(FpData),
    ),
    writeIntRf = true,
    writeFpRf = false,
    writeFflags = true,
    latency = CertainLatency(2),
  )

  def F2fCfg = FuConfig (
    fuType = FuType.fDivSqrt,
    srcData = Seq(
      Seq(FpData),
    ),
    writeIntRf = false,
    writeFpRf = true,
    writeFflags = true,
    latency = UncertainLatency(),
    hasInputBuffer = (true, 8, true)
  )

  def FDivSqrtCfg = FuConfig (
    fuType = FuType.fDivSqrt,
    srcData = Seq(
      Seq(FpData, FpData),
    ),
    writeIntRf = false,
    writeFpRf = true,
    writeFflags = true,
    latency = UncertainLatency(),
    hasInputBuffer = (true, 8, true)
  )

  def LduCfg = FuConfig (
    fuType = FuType.ldu,
    srcData = Seq(
      Seq(IntData),
    ),
    writeIntRf = true,
    writeFpRf = true,
    latency = UncertainLatency(),
    exceptionOut = Seq(loadAddrMisaligned, loadAccessFault, loadPageFault),
    flushPipe = true,
    replayInst = true,
    hasLoadError = true
  )

  def StaCfg = FuConfig (
    fuType = FuType.stu,
    srcData = Seq(
      Seq(IntData),
    ),
    writeIntRf = false,
    writeFpRf = false,
    latency = UncertainLatency(),
    exceptionOut = Seq(storeAddrMisaligned, storeAccessFault, storePageFault)
  )

  def StdCfg = FuConfig (
    fuType = FuType.stu,
    srcData = Seq(
      Seq(IntData),
      Seq(FpData),
    ),
    writeIntRf = false,
    writeFpRf = false,
    latency = CertainLatency(1),
    exceptionOut = Seq(storeAddrMisaligned, storeAccessFault, storePageFault)
  )

  def MouCfg = FuConfig (
    fuType = FuType.mou,
    srcData = Seq(
      Seq(IntData),
    ),
    writeIntRf = false,
    writeFpRf = false,
    latency = UncertainLatency(),
    exceptionOut = (LduCfg.exceptionOut ++ StaCfg.exceptionOut ++ StdCfg.exceptionOut).distinct
  )

  def MoudCfg = FuConfig (
    fuType = FuType.mou,
    srcData = Seq(
      Seq(IntData),
    ),
    writeIntRf = false,
    writeFpRf = false,
    latency = UncertainLatency()
  )

  def VipuCfg = FuConfig (
    fuType = FuType.vipu,
    srcData = Seq(
      Seq(VecData, VecData, VecData, MaskSrcData),  // vs1, vs2, vd_old, v0
      Seq(VecData, VecData, VecData, VecData),      // vs1_1, vs2, vs1_2, vs2_2 // no mask and vta
    ),
    writeIntRf = true,
    writeFpRf = false,
    writeVecRf = true,
    latency = UncertainLatency(),
  )

  def VfpuCfg = FuConfig (
    fuType = FuType.vfpu,
    srcData = Seq(
      Seq(VecData, VecData, VecData, MaskSrcData),  // vs1, vs2, vd_old, v0
      Seq(VecData, VecData, VecData, VecData),      // vs1_1, vs2, vs1_2, vs2_2 // no mask and vta
      Seq(FpData, VecData, VecData, MaskSrcData),   // f[rs1], vs2, vd_old, v0
    ),
    writeIntRf = true,
    writeFpRf = false,
    writeVecRf = true,
    latency = UncertainLatency(),
  )
  // Todo
  // def VlduCfg = FuConfig ()
  // def VstuCfg = FuConfig ()
}
