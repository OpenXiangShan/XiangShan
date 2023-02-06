package xiangshan

import chisel3._
import chisel3.util.{BitPat, log2Up}
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
    wbSink: SchedulerType,
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

    def apply() = UInt(log2Up(num).W)

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
  
  case class FuConfig (
    fuType: Int,
    numIntSrc: Int,
    numFpSrc: Int,
    numVecSrc: Int = 0,
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
  )

  def JmpCfg: FuConfig = FuConfig (
    fuType = FuType.jmp,
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    hasRedirect = true,
  )

  def BrhCfg = FuConfig (
    fuType = FuType.brh,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = false,
    writeFpRf = false,
    hasRedirect = true,
  )

  def I2fCfg = FuConfig (
    FuType.i2f,
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = false,
    writeFpRf = true,
    writeFflags = true,
    latency = CertainLatency(2),
  )

  def CsrCfg = FuConfig (
    fuType = FuType.csr,
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    exceptionOut = Seq(illegalInstr, breakPoint, ecallU, ecallS, ecallM),
    flushPipe = true
  )

  def AluCfg: FuConfig = FuConfig (
    fuType = FuType.alu,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
  )

  def MulCfg = FuConfig (
    FuType.mul,
    2,
    0,
    writeIntRf = true,
    writeFpRf = false,
    latency = CertainLatency(2),
  )

  def DivCfg = FuConfig (
    FuType.div,
    2,
    0,
    writeIntRf = true,
    writeFpRf = false,
    latency = UncertainLatency(),
    hasInputBuffer = (true, 4, true)
  )

  def FenceCfg: FuConfig = FuConfig (
    FuType.fence,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = false,
    writeFpRf = false,
    latency = UncertainLatency(),
    exceptionOut = Seq(illegalInstr),
    flushPipe = true
  )

  // Todo: split it to simple bitmap exu and complex bku
  def BkuCfg = FuConfig (
    fuType = FuType.bku,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    latency = CertainLatency(1),
  )

  def VsetCfg = FuConfig (
    fuType = FuType.vset,
    numIntSrc = 2,
    numFpSrc = 0,
    writeIntRf = true,
    writeFpRf = false,
    latency = CertainLatency(0)
  )

  def FmacCfg = FuConfig (
    fuType = FuType.fmac,
    numIntSrc = 0,
    numFpSrc = 3,
    writeIntRf = false,
    writeFpRf = true,
    writeFflags = true,
    latency = UncertainLatency(),
  )

  def F2iCfg = FuConfig (
    fuType = FuType.fmisc,
    numIntSrc = 0,
    numFpSrc = 1,
    writeIntRf = true,
    writeFpRf = false,
    writeFflags = true,
    latency = CertainLatency(2),
  )

  def F2fCfg = FuConfig (
    fuType = FuType.fDivSqrt,
    numIntSrc = 0,
    numFpSrc = 2,
    writeIntRf = false,
    writeFpRf = true,
    writeFflags = true,
    latency = UncertainLatency(),
    hasInputBuffer = (true, 8, true)
  )

  def FDivSqrtCfg = FuConfig (
    fuType = FuType.fDivSqrt,
    numIntSrc = 0,
    numFpSrc = 2,
    writeIntRf = false,
    writeFpRf = true,
    writeFflags = true,
    latency = UncertainLatency(),
    hasInputBuffer = (true, 8, true)
  )

  def LduCfg = FuConfig (
    fuType = FuType.ldu,
    numIntSrc = 1,
    numFpSrc = 0,
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
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = false,
    writeFpRf = false,
    latency = UncertainLatency(),
    exceptionOut = Seq(storeAddrMisaligned, storeAccessFault, storePageFault)
  )

  def StdCfg = FuConfig (
    fuType = FuType.stu,
    numIntSrc = 1,
    numFpSrc = 1,
    writeIntRf = false,
    writeFpRf = false,
    latency = CertainLatency(1),
    exceptionOut = Seq(storeAddrMisaligned, storeAccessFault, storePageFault)
  )

  def MouCfg = FuConfig (
    fuType = FuType.mou,
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = false,
    writeFpRf = false,
    latency = UncertainLatency(),
    exceptionOut = (LduCfg.exceptionOut ++ StaCfg.exceptionOut ++ StdCfg.exceptionOut).distinct
  )

  def MoudCfg = FuConfig (
    fuType = FuType.mou,
    numIntSrc = 1,
    numFpSrc = 0,
    writeIntRf = false,
    writeFpRf = false,
    latency = UncertainLatency()
  )

  def VipuCfg = FuConfig (
    fuType = FuType.vipu,
    numIntSrc = 1,
    numFpSrc = 0,
    numVecSrc = 4,
    writeIntRf = true,
    writeFpRf = false,
    writeVecRf = true,
    latency = UncertainLatency(),
  )

  def VfpuCfg = FuConfig (
    fuType = FuType.vfpu,
    numIntSrc = 0,
    numFpSrc = 1,
    numVecSrc = 4,
    writeIntRf = true,
    writeFpRf = false,
    writeVecRf = true,
    latency = UncertainLatency(),
  )
  // Todo
  // def VlduCfg = FuConfig ()
  // def VstuCfg = FuConfig ()
}
