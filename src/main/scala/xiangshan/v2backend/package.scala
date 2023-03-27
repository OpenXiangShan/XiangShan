package xiangshan

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.fpu.IntToFP
import xiangshan.backend.fu.{CertainLatency, HasFuLatency, UncertainLatency, CSR, Fence, Bku}
import xiangshan.v2backend.fu._

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

  abstract class WriteBackConfig() {
    val port: Int
    def dataCfg: DataConfig
    def numPreg: Int = 0
    def dataWidth: Int = dataCfg.dataWidth

    def pregIdxWidth = log2Up(numPreg)

    def writeInt = dataCfg == IntData()
    def writeFp = dataCfg == FpData()
    def writeVec = dataCfg == VecData()
  }

  abstract class ExuWB extends WriteBackConfig

  abstract class PregWB extends ExuWB {
    val priority: Int
  }

  case class IntWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {
    def dataCfg: DataConfig = IntData()
    override def numPreg: Int = 160
  }

  case class FpWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {
    def dataCfg: DataConfig = FpData()
    override def numPreg: Int = 160
  }

  case class VecWB(
    port    : Int = -1,
    priority: Int = Int.MaxValue,
  ) extends PregWB {
    def dataCfg: DataConfig = VecData()
    override def numPreg: Int = 160
  }

  case class CtrlWB(
    port: Int = -1,
  ) extends WriteBackConfig {
    val priority: Int = Int.MaxValue
    override def dataCfg: DataConfig = NoData()
  }

  abstract class StdWB extends WriteBackConfig

  case class StdIntWB(override val port: Int) extends StdWB {
    override def dataCfg: DataConfig = IntData()
  }

  case class StdFpWB(override val port: Int) extends StdWB {
    override def dataCfg: DataConfig = FpData()
  }

  case class StdVecWB(override val port: Int) extends StdWB {
    override def dataCfg: DataConfig = VecData()
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

    def X = BitPat.N(num) // Todo: Don't Care

    def num = 20

    def width = num

    def apply() = UInt(num.W)

    def isInt(fuType: UInt): Bool = fuType(9, 0).orR // from jmp to vset

    def isJump(fuType: UInt): Bool = fuType(0)

    def isFp(fuType: UInt): Bool = fuType(12, 10).orR

    def isMem(fuType: UInt): Bool = fuType(15, 13).orR

    def isLoadStore(fuType: UInt): Bool = fuType(14, 13).orR

    def isLoad(fuType: UInt): Bool = fuType(13)

    def isStore(fuType: UInt): Bool = fuType(14).orR

    def isAMO(fuType: UInt): Bool = fuType(15).orR

    def isFence(fuType: UInt): Bool = fuType(7)

    def isVpu(fuType: UInt): Bool = fuType(17, 16).orR

    def storeIsAMO(fuType: UInt): Bool = fuType(15)

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

  abstract class DataConfig (
    val name: String,
    val dataWidth: Int,
  ) {
    override def toString: String = name
  }

  case class IntData() extends DataConfig("int", 64)
  case class FpData() extends DataConfig("fp", 64)
  case class VecData() extends DataConfig("vec", 128)
  case class ImmData(len: Int) extends DataConfig("int", len)
  case class VAddrData() extends DataConfig("vaddr", 39) // Todo: associate it with the width of vaddr
  case class MaskSrcData() extends DataConfig("masksrc", VecData().dataWidth) // 128
  case class MaskDstData() extends DataConfig("maskdst", VecData().dataWidth / 8) // 16
  case class VConfigData() extends DataConfig("vconfig", VecData().dataWidth) // Todo: use 16 bit instead
  case class NoData() extends DataConfig("nodata", 0)

  def RegSrcDataSet: Set[DataConfig] = Set(IntData(), FpData(), VecData(), MaskSrcData(), VConfigData())

  case class FuConfig (
    name: String,
    fuType: Int,
    fuGen: (Parameters, FuConfig) => FuncUnit,
    srcData: Seq[Seq[DataConfig]],
    writeIntRf: Boolean,
    writeFpRf: Boolean,
    writeVecRf: Boolean = false,
    writeFflags: Boolean = false,
    dataBits: Int = 64,
    latency: HasFuLatency = CertainLatency(0),
    hasInputBuffer: (Boolean, Int, Boolean) = (false, 0, false),
    exceptionOut: Seq[Int] = Seq(),
    hasLoadError: Boolean = false,
    flushPipe: Boolean = false,
    replayInst: Boolean = false,
    trigger: Boolean = false,
    needSrcFrm: Boolean = false,
    immType: Set[UInt] = Set(),
  ) {
    def numIntSrc: Int = srcData.map(_.count(_ == IntData())).max
    def numFpSrc: Int = srcData.map(_.count(_ == FpData())).max
    def numVecSrc: Int = srcData.map(_.count(x => x == VecData() || x == MaskSrcData() || x == VConfigData())).max
    def numSrcMax: Int = srcData.map(_.length).max
    def readFp: Boolean = numFpSrc > 0

    def fuSel(fuType: UInt): Bool = fuType === this.fuType.U
    /**
      * params(i): data type set of the ith src port
      * @return
      */
    def getRfReadDataCfgSet: Seq[Set[DataConfig]] = {
      val numSrcMax = srcData.map(_.length).max
      // make srcData is uniform sized to avoid exception when transpose
      val alignedSrcData: Seq[Seq[DataConfig]] = srcData.map(x => x ++ Seq.fill(numSrcMax - x.length)(null))
      alignedSrcData.transpose.map(_.toSet.intersect(RegSrcDataSet))
    }

    def hasNoDataWB: Boolean = {
      !(writeIntRf || writeFpRf || writeVecRf)
    }

    def getSrcMaxWidthVec = {
      getRfReadDataCfgSet.map(_.map(_.dataWidth).max)
    }

    def genSrcDataVec: Seq[UInt] = {
      getSrcMaxWidthVec.map(w => UInt(w.W))
    }

    // csr's redirect is in its exception bundle
    def hasRedirect: Boolean = Seq(FuType.jmp, FuType.brh).contains(fuType)

    def hasPredecode: Boolean = Seq(FuType.jmp, FuType.brh, FuType.csr).contains(fuType)

    def needPc: Boolean = Seq(FuType.jmp, FuType.brh, FuType.csr, FuType.fence).contains(fuType)

    def needFPUCtrl: Boolean = {
      import FuType._
      Set(fmac, fDivSqrt, fmisc, i2f).contains(fuType)
    }

    def isMul: Boolean = fuType == FuType.mul

    def isDiv: Boolean = fuType == FuType.div

    def isCsr: Boolean = fuType == FuType.csr

    def isFence: Boolean = fuType == FuType.fence
  }

  val JmpCfg: FuConfig = FuConfig (
    name = "jmp",
    fuType = FuType.jmp,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new JumpUnit(cfg)(p)).suggestName("jmp"),
    srcData = Seq(
      Seq(IntData()), // jal
    ),
    writeIntRf = true,
    writeFpRf = false,
    immType = Set(SelImm.IMM_I, SelImm.IMM_UJ, SelImm.IMM_U),
  )

  val BrhCfg: FuConfig = FuConfig (
    name = "brh",
    fuType = FuType.brh,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new BranchUnit(cfg)(p).suggestName("brh")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    writeIntRf = false,
    writeFpRf = false,
    immType = Set(SelImm.IMM_SB),
  )

  val I2fCfg: FuConfig = FuConfig (
    name = "i2f",
    FuType.i2f,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new IntToFP(cfg)(p).suggestName("i2f")),
    srcData = Seq(
      Seq(IntData()),
    ),
    writeIntRf = false,
    writeFpRf = true,
    writeFflags = true,
    latency = CertainLatency(2),
    needSrcFrm = true,
  )

  val CsrCfg: FuConfig = FuConfig (
    name = "csr",
    fuType = FuType.csr,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new CSR(cfg)(p).suggestName("csr")),
    srcData = Seq(
      Seq(IntData()),
    ),
    writeIntRf = true,
    writeFpRf = false,
    exceptionOut = Seq(illegalInstr, breakPoint, ecallU, ecallS, ecallM),
    flushPipe = true
  )

  val AluCfg: FuConfig = FuConfig (
    name = "alu",
    fuType = FuType.alu,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new Alu(cfg)(p).suggestName("Alu")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    writeIntRf = true,
    writeFpRf = false,
    immType = Set(SelImm.IMM_I, SelImm.IMM_U),
  )

  val MulCfg: FuConfig = FuConfig (
    name = "mul",
    fuType = FuType.mul,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new MulUnit(cfg)(p).suggestName("Mul")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    writeIntRf = true,
    writeFpRf = false,
    latency = CertainLatency(2),
  )

  val DivCfg: FuConfig = FuConfig (
    name = "div",
    fuType = FuType.div,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new DivUnit(cfg)(p).suggestName("Div")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    writeIntRf = true,
    writeFpRf = false,
    latency = UncertainLatency(),
    hasInputBuffer = (true, 4, true)
  )

  val FenceCfg: FuConfig = FuConfig (
    name = "fence",
    FuType.fence,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new Fence(cfg)(p).suggestName("Fence")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    writeIntRf = false,
    writeFpRf = false,
    latency = UncertainLatency(),
    exceptionOut = Seq(illegalInstr),
    flushPipe = true
  )

  // Todo: split it to simple bitmap exu and complex bku
  val BkuCfg: FuConfig = FuConfig (
    name = "bku",
    fuType = FuType.bku,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new Bku(cfg)(p).suggestName("Bku")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    writeIntRf = true,
    writeFpRf = false,
    latency = CertainLatency(1),
  )

  val VsetCfg: FuConfig = FuConfig (
    name = "vset",
    fuType = FuType.vset,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    writeIntRf = true,
    writeFpRf = false,
    latency = CertainLatency(0)
  )

  val FmacCfg: FuConfig = FuConfig (
    name = "fmac",
    fuType = FuType.fmac,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(FpData(), FpData()),
      Seq(FpData(), FpData(), FpData()),
    ),
    writeIntRf = false,
    writeFpRf = true,
    writeFflags = true,
    latency = UncertainLatency(),
    needSrcFrm = true,
  )

  val F2iCfg: FuConfig = FuConfig (
    name = "f2i",
    fuType = FuType.fmisc,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(FpData()),
    ),
    writeIntRf = true,
    writeFpRf = false,
    writeFflags = true,
    latency = CertainLatency(2),
    needSrcFrm = true,
  )

  val F2fCfg: FuConfig = FuConfig (
    name = "f2f",
    fuType = FuType.fDivSqrt,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(FpData()),
    ),
    writeIntRf = false,
    writeFpRf = true,
    writeFflags = true,
    latency = UncertainLatency(),
    hasInputBuffer = (true, 8, true),
    needSrcFrm = true,
  )

  val FDivSqrtCfg: FuConfig = FuConfig (
    name = "fdiv",
    fuType = FuType.fDivSqrt,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(FpData(), FpData()),
    ),
    writeIntRf = false,
    writeFpRf = true,
    writeFflags = true,
    latency = UncertainLatency(),
    hasInputBuffer = (true, 8, true),
    needSrcFrm = true,
  )

  val LduCfg: FuConfig = FuConfig (
    name = "ldu",
    fuType = FuType.ldu,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(IntData()),
    ),
    writeIntRf = true,
    writeFpRf = true,
    latency = UncertainLatency(),
    exceptionOut = Seq(loadAddrMisaligned, loadAccessFault, loadPageFault),
    flushPipe = true,
    replayInst = true,
    hasLoadError = true,
    immType = Set(SelImm.IMM_I),
  )

  val StaCfg: FuConfig = FuConfig (
    name = "sta",
    fuType = FuType.stu,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(IntData()),
    ),
    writeIntRf = false,
    writeFpRf = false,
    latency = UncertainLatency(),
    exceptionOut = Seq(storeAddrMisaligned, storeAccessFault, storePageFault),
    immType = Set(SelImm.IMM_S),
  )

  val StdCfg: FuConfig = FuConfig (
    name = "std",
    fuType = FuType.stu,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(IntData()),
      Seq(FpData()),
    ),
    writeIntRf = false,
    writeFpRf = false,
    latency = CertainLatency(1),
    exceptionOut = Seq(storeAddrMisaligned, storeAccessFault, storePageFault)
  )

  val MouCfg: FuConfig = FuConfig (
    name = "mou",
    fuType = FuType.mou,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(IntData()),
    ),
    writeIntRf = false,
    writeFpRf = false,
    latency = UncertainLatency(),
    exceptionOut = (LduCfg.exceptionOut ++ StaCfg.exceptionOut ++ StdCfg.exceptionOut).distinct
  )

  val MoudCfg: FuConfig = FuConfig (
    name = "moud",
    fuType = FuType.mou,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(IntData()),
    ),
    writeIntRf = false,
    writeFpRf = false,
    latency = UncertainLatency()
  )

  val VipuCfg: FuConfig = FuConfig (
    name = "vipu",
    fuType = FuType.vipu,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(VecData(), VecData(), VecData(), MaskSrcData(), VConfigData()),  // vs1, vs2, vd_old, v0
      Seq(VecData(), VecData(), VecData(), VecData(), VConfigData()),      // vs1_1, vs2, vs1_2, vs2_2 // no mask and vta
    ),
    writeIntRf = false,
    writeFpRf = false,
    writeVecRf = true,
    latency = UncertainLatency(),
  )

  val VfpuCfg: FuConfig = FuConfig (
    name = "vfpu",
    fuType = FuType.vfpu,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(VecData(), VecData(), VecData(), MaskSrcData(), VConfigData()),  // vs1, vs2, vd_old, v0
      Seq(VecData(), VecData(), VecData(), VecData(), VConfigData()),      // vs1_1, vs2, vs1_2, vs2_2 // no mask and vta
      Seq(FpData(), VecData(), VecData(), MaskSrcData(), VConfigData()),   // f[rs1], vs2, vd_old, v0
    ),
    writeIntRf = false,
    writeFpRf = false,
    writeVecRf = true,
    latency = UncertainLatency(),
  )
  // Todo
  // def VlduCfg = FuConfig ()
  // def VstuCfg = FuConfig ()
}
