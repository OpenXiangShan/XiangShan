package xiangshan.backend.vector.fu

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.Bundles.ExuInput
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.decode._
import xiangshan.backend.fu.wrapper._
import xiangshan.backend.fu.{FuConfig, FuType, FuncUnit}
import xiangshan.backend.vector.Exu
import xiangshan.backend.vector.fu.VecFuConfig.VecVectorV2Config

import scala.beans.BeanProperty

/**
  *
  * @param name [[String]] name of fuConfig
  * @param fuType [[Int]] type of func, select from [[xiangshan.backend.fu.FuType]]
  * @param fuGen how to create $fu
  * @param srcData type of src data used by this $fu
  * @param piped if the $fu is pipelined
  * @param maybeBlock the $fu need ready signal to block internal pipeline
  * @param writeIntRf the $fu write int regfiles
  * @param writeFpRf the $fu write float regfiles
  * @param writeVecRf the $fu write vector regfiles
  * @param writeV0Rf the $fu write v0 regfiles
  * @param writeVlRf the $fu write vl regfiles
  * @param writeFflags the $fu write fflags csr
  * @param writeVxsat the $fu write vxsat csr
  * @param destDataBits the width of output data in the $fu
  * @param srcDataBits the width of input data in the $fu, the default value is destDataBits
  * @param latency the latency of instuction executed in the $fu
  * @param hasInputBuffer if the $fu has input buffer
  * @param exceptionOut the $fu can produce these exception
  * @param hasLoadError if the $fu has load error out
  * @param flushPipe if the instuction executed in the $fu need flush out
  * @param replayInst if the instuction executed in the $fu can replay in some condition
  * @param trigger if the $fu need trigger out
  * @param needSrcFrm if the $fu need float rounding mode signal
  * @param needSrcVxrm if the $fu need vector fixed-point rounding mode signal
  * @param immType the immediate type of this $fu
  * @param vlWakeUp
  * @param maskWakeUp
  * @param readVl if the $fu need read vl
  *
  * @define fu function unit
  */
case class VecFuConfig (
  name          : String,
  fuType        : FuType.OHType,
  fuGen         : (Parameters, FuConfig) => FuncUnit,
  fuGen2        : (Parameters, VecFuConfig) => Func = (_, cfg) => throw new NotImplementedError(s"fuGen is not set for ${cfg.name}"),
  srcData       : Seq[Seq[DataConfig]],
  piped         : Boolean,
  maybeBlock    : Boolean = false,
  writeIntRf    : Boolean = false,
  writeFpRf     : Boolean = false,
  writeVecRf    : Boolean = false,
  writeV0Rf     : Boolean = false,
  writeVlRf     : Boolean = false,
  writeFakeIntRf: Boolean = false,
  writeFflags   : Boolean = false,
  writeVxsat    : Boolean = false,
  destDataBits  : Int = 64,
  srcDataBits   : Option[Int] = None,
  srcNeedCopy   : Boolean = false,
  latency       : Int = 0,
  hasInputBuffer: (Boolean, Int, Boolean) = (false, 0, false),
  exceptionOut  : Seq[Int] = Seq(),
  hasLoadError  : Boolean = false,
  flushPipe     : Boolean = false,
  replayInst    : Boolean = false,
  trigger       : Boolean = false,
  needSrcFrm    : Boolean = false,
  needSrcVxrm   : Boolean = false,
  writeVType    : Boolean = false,
  immType       : Set[Imm] = Set(),
  vlWakeUp      : Boolean = false,
  maskWakeUp    : Boolean = false,
  readV0        : Boolean = false,
  readVl        : Boolean = false,
  readVType     : Boolean = false,
) extends VecVectorV2Config {
  require(srcData.forall(!_.contains(V0Data())), s"V0Data() should not appear in srcData args")
  require(srcData.forall(!_.contains(VlData())), s"VlData() should not appear in srcData args")

  def needIntWen: Boolean = writeIntRf || writeFakeIntRf
  def needFpWen:  Boolean = writeFpRf
  def needVecWen: Boolean = writeVecRf
  def needV0Wen:  Boolean = writeV0Rf
  def needVlWen:  Boolean = writeVlRf

  def numIntSrc : Int = srcData.map(_.count(x => IntRegSrcDataSet.contains(x))).fold(0)(_ max _)
  def numFpSrc  : Int = srcData.map(_.count(x => FpRegSrcDataSet.contains(x))).fold(0)(_ max _)
  def numVecSrc : Int = srcData.map(_.count(x => VecRegSrcDataSet.contains(x))).fold(0)(_ max _)
  def numVfSrc  : Int = srcData.map(_.count(x => VecRegSrcDataSet.contains(x))).fold(0)(_ max _)
  def numV0Src  : Int = if (this.readV0) 1 else 0
  def numVlSrc  : Int = if (this.readVl) 1 else 0
  def numRegSrc : Int = srcData.map(_.count(x => RegSrcDataSet.contains(x))).fold(0)(_ max _)
  def numSrc    : Int = (if (isSta) 3 else srcData.map(_.length).fold(0)(_ max _))

  def readFp: Boolean = numFpSrc > 0

  def fuSel(uop: ExuInput): Bool = {
    // Don't add more shit here!!!
    // Todo: add new FuType to distinguish f2i, f2f
    uop.fuType === this.fuType.U
  }

  def fuSel2(uop: Exu.InUop): Bool = {
    (uop.ctrl.fuType & this.fuType.U).orR
  }

  /**
    * params(i): data type set of the ith src port
    * @return
    */
  def getRfReadDataCfgSet: Seq[Set[DataConfig]] = {
    val numSrcMax = srcData.map(_.length).fold(0)(_ max _)
    // make srcData is uniform sized to avoid exception when transpose
    val alignedSrcData: Seq[Seq[DataConfig]] = srcData.map(x => x ++ Seq.fill(numSrcMax - x.length)(null))
    alignedSrcData.transpose.map(_.toSet.intersect(RegSrcDataSet))
  }

  def getSrcDataType(srcIdx: Int): Set[DataConfig] = {
    srcData
      .map((x: Seq[DataConfig]) => if(x.isDefinedAt(srcIdx)) Some(x(srcIdx)) else None)
      .filter(_.nonEmpty)
      .map(_.get)
      .toSet
  }

  def hasNoDataWB: Boolean = {
    !(writeIntRf || writeFpRf || writeVecRf || writeV0Rf || writeVlRf)
  }

  def getSrcMaxWidthVec = {
    getRfReadDataCfgSet.map(_.map(_.dataWidth).maxOption.getOrElse(0))
  }

  def genSrcDataVec: Seq[UInt] = {
    getSrcMaxWidthVec.map(w => UInt(w.W))
  }

  // csr's redirect also uses redirect bundle
  def hasRedirect: Boolean = Seq(FuType.jmp, FuType.brh, FuType.csr).contains(fuType)

  def hasIsRVC: Boolean = Seq(FuType.jmp, FuType.brh, FuType.csr, FuType.ldu).contains(fuType)

  def hasRasAction: Boolean = Seq(FuType.jmp).contains(fuType)

  def needTargetPc: Boolean = Seq(FuType.jmp, FuType.brh).contains(fuType)

  // predict info
  def needPdInfo: Boolean = Seq(FuType.jmp, FuType.brh, FuType.csr).contains(fuType)

  def needPc: Boolean = Seq(FuType.jmp, FuType.brh, FuType.ldu).contains(fuType)

  var aluNeedPc: Boolean = false

  def needCriticalErrors: Boolean = Seq(FuType.csr).contains(fuType)

  def needSqIdx: Boolean = Seq(FuType.ldu, FuType.stu).contains(fuType)

  def isAlu: Boolean = fuType == FuType.alu

  def isMul: Boolean = fuType == FuType.mul

  def isDiv: Boolean = fuType == FuType.div

  def isCsr: Boolean = fuType == FuType.csr

  def isBrh: Boolean = fuType == FuType.brh

  def isJmp: Boolean = fuType == FuType.jmp

  def isFence: Boolean = fuType == FuType.fence

  def isVStd: Boolean = name == "vstd"

  def isVecArith: Boolean = fuType == FuType.vialuF || fuType == FuType.vimac ||
                            fuType == FuType.vppu || fuType == FuType.vipu ||
                            fuType == FuType.vfalu || fuType == FuType.vfma ||
                            fuType == FuType.vfdiv || fuType == FuType.vfcvt ||
                            fuType == FuType.vidiv || fuType == FuType.vmove

  def isVecMem: Boolean = fuType == FuType.vldu || fuType == FuType.vstu ||
                          fuType == FuType.vsegldu || fuType == FuType.vsegstu ||
                          name == "vstd"


  def needOg2: Boolean = isVecArith || isVecMem

  def isSta: Boolean = name.contains("sta")

  def isStd: Boolean = name.contains("std")

  def ckAlwaysEn: Boolean = isCsr || isFence

  @BeanProperty
  var fuConfig: FuConfig = _

  override def toString: String = {
    var str = s"${this.name}: "
    str += s"latency($latency)"
    str += s"src($srcData)"
    str
  }
}

object VecFuConfig {
  def fromFuConfig(cfg: FuConfig, fuGen2: (Parameters, VecFuConfig) => Func = null, _latency: Option[Int] = None): VecFuConfig = {
    val newCfg = VecFuConfig(
      name = cfg.name,
      fuType = cfg.fuType,
      fuGen = cfg.fuGen,
      fuGen2 = fuGen2,
      srcData = cfg.srcData,
      piped = cfg.piped,
      maybeBlock = cfg.maybeBlock,
      writeIntRf = cfg.writeIntRf,
      writeFpRf = cfg.writeFpRf,
      writeVecRf = cfg.writeVecRf,
      writeV0Rf = cfg.writeV0Rf,
      writeVlRf = cfg.writeVlRf,
      writeFakeIntRf = cfg.writeFakeIntRf,
      writeFflags = cfg.writeFflags,
      writeVxsat = cfg.writeVxsat,
      destDataBits = cfg.destDataBits,
      srcDataBits = cfg.srcDataBits,
      srcNeedCopy = cfg.srcNeedCopy,
      latency = _latency.getOrElse(cfg.latency.latencyVal.getOrElse(0)),
      hasInputBuffer = cfg.hasInputBuffer,
      exceptionOut = cfg.exceptionOut,
      hasLoadError = cfg.hasLoadError,
      flushPipe = cfg.flushPipe,
      replayInst = cfg.replayInst,
      trigger = cfg.trigger,
      needSrcFrm = cfg.needSrcFrm,
      needSrcVxrm = cfg.needSrcVxrm,
      writeVType = cfg.writeVType,
      immType = cfg.immType,
      vlWakeUp = cfg.vlWakeUp,
      maskWakeUp = cfg.maskWakeUp,
      readV0 = cfg.readV0,
      readVl = cfg.readVl,
      readVType = cfg.readVType
    )

    newCfg.setFuConfig(cfg)

    newCfg
  }

  val JmpCfg = VecFuConfig.fromFuConfig(FuConfig.JmpCfg)
  val BrhCfg = VecFuConfig.fromFuConfig(FuConfig.BrhCfg)
  val I2fCfg = VecFuConfig.fromFuConfig(FuConfig.I2fCfg)
  val FcmpCfg = VecFuConfig.fromFuConfig(FuConfig.FcmpCfg)
  val I2vCfg = VecFuConfig.fromFuConfig(FuConfig.I2vCfg)
  val F2vCfg = VecFuConfig.fromFuConfig(FuConfig.F2vCfg)
  val CsrCfg = VecFuConfig.fromFuConfig(FuConfig.CsrCfg)
  val AluCfg = VecFuConfig.fromFuConfig(FuConfig.AluCfg)
  val MulCfg = VecFuConfig.fromFuConfig(FuConfig.MulCfg)
  val DivCfg = VecFuConfig.fromFuConfig(FuConfig.DivCfg)
  val FenceCfg = VecFuConfig.fromFuConfig(FuConfig.FenceCfg)
  val BkuCfg = VecFuConfig.fromFuConfig(FuConfig.BkuCfg)
  val VSetCfg = VecFuConfig.fromFuConfig(FuConfig.VSetCfg)
  val VSetRvfWvfCfg = VecFuConfig.fromFuConfig(FuConfig.VSetRvfWvfCfg)
  val VSetRiWvfCfg = VecFuConfig.fromFuConfig(FuConfig.VSetRiWvfCfg)
  val VSetRiWiCfg = VecFuConfig.fromFuConfig(FuConfig.VSetRiWiCfg)
  val LduCfg = VecFuConfig.fromFuConfig(FuConfig.LduCfg)
  val StaCfg = VecFuConfig.fromFuConfig(FuConfig.StaCfg)
  val StdCfg = VecFuConfig.fromFuConfig(FuConfig.StdCfg)
  val HyldaCfg = VecFuConfig.fromFuConfig(FuConfig.HyldaCfg)
  val HystaCfg = VecFuConfig.fromFuConfig(FuConfig.HystaCfg)
  val FakeHystaCfg = VecFuConfig.fromFuConfig(FuConfig.FakeHystaCfg)
  val MouCfg = VecFuConfig.fromFuConfig(FuConfig.MouCfg)
  val MoudCfg = VecFuConfig.fromFuConfig(FuConfig.MoudCfg)
  val VialuCfg = VecFuConfig.fromFuConfig(FuConfig.VialuCfg, (p: Parameters, cfg: VecFuConfig) => Module(new VIAluWrapper(cfg)(p).suggestName("Vialu")))
  val VimacCfg = VecFuConfig.fromFuConfig(FuConfig.VimacCfg, (p: Parameters, cfg: VecFuConfig) => Module(new VIMacU(cfg)(p).suggestName("Vimac")))
  val VidivCfg = VecFuConfig.fromFuConfig(FuConfig.VidivCfg, (p: Parameters, cfg: VecFuConfig) => Module(new VIDiv(cfg)(p).suggestName("Vidiv")))
  val VppuCfg = VecFuConfig.fromFuConfig(FuConfig.VppuCfg)
  val VipuCfg = VecFuConfig.fromFuConfig(FuConfig.VipuCfg)
  val VmoveCfg = VecFuConfig.fromFuConfig(FuConfig.VmoveCfg, (p: Parameters, cfg: VecFuConfig) => Module(new VMove(cfg)(p).suggestName("Vmove")))
  val VfaluCfg = VecFuConfig.fromFuConfig(FuConfig.VfaluCfg)
  val VfmaCfg  = VecFuConfig.fromFuConfig(FuConfig.VfmaCfg,  (p: Parameters, cfg: VecFuConfig) => Module(new VFMacWrapper(cfg)(p).suggestName("Vfma")))
  val VfdivCfg = VecFuConfig.fromFuConfig(FuConfig.VfdivCfg)
  val VfcvtCfg = VecFuConfig.fromFuConfig(FuConfig.VfcvtCfg, (p: Parameters, cfg: VecFuConfig) => Module(new VCVTWrapper(cfg)(p).suggestName("Vfcvt")))
  val VSha256msCfg = VecFuConfig.fromFuConfig(FuConfig.VSha256msCfg)
  val VSha256cCfg = VecFuConfig.fromFuConfig(FuConfig.VSha256cCfg)
  val FaluCfg = VecFuConfig.fromFuConfig(FuConfig.FaluCfg)
  val FmacCfg = VecFuConfig.fromFuConfig(FuConfig.FmacCfg)
  val FdivCfg = VecFuConfig.fromFuConfig(FuConfig.FdivCfg)
  val FcvtCfg = VecFuConfig.fromFuConfig(FuConfig.FcvtCfg)
  val VStdCfg = VecFuConfig.fromFuConfig(FuConfig.VStdCfg, (p: Parameters, cfg: VecFuConfig) => Module(new VStdWrapper(cfg)(p).suggestName("Vstd")))
  val VlduCfg = VecFuConfig.fromFuConfig(FuConfig.VlduCfg)
  val VstuCfg = VecFuConfig.fromFuConfig(FuConfig.VstuCfg)
  val VseglduCfg = VecFuConfig.fromFuConfig(FuConfig.VseglduCfg)
  val VsegstuCfg = VecFuConfig.fromFuConfig(FuConfig.VsegstuCfg)

  def allConfigs = Seq(
    JmpCfg,
    BrhCfg,
    I2fCfg,
    FcmpCfg,
    I2vCfg,
    F2vCfg,
    CsrCfg,
    AluCfg,
    MulCfg,
    DivCfg,
    FenceCfg,
    BkuCfg,
    VSetCfg,
    VSetRvfWvfCfg,
    VSetRiWvfCfg,
    VSetRiWiCfg,
    LduCfg,
    StaCfg,
    StdCfg,
    HyldaCfg,
    HystaCfg,
    FakeHystaCfg,
    MouCfg,
    MoudCfg,
    VialuCfg,
    VimacCfg,
    VidivCfg,
    VppuCfg,
    VipuCfg,
    VmoveCfg,
    VfaluCfg,
    VfmaCfg,
    VfdivCfg,
    VfcvtCfg,
    VSha256msCfg,
    VSha256cCfg,
    FaluCfg,
    FmacCfg,
    FdivCfg,
    FcvtCfg,
    VStdCfg,
    VlduCfg,
    VstuCfg,
    VseglduCfg,
    VsegstuCfg,
  )

  trait VecVectorV2Config { self: VecFuConfig =>
    def writeGpRf = this.writeIntRf
    def writeVpRf = this.writeVecRf

    def needFlushPipe = this.flushPipe
    def needVM = this.readV0
    def readOldVType = this.writeVlRf

  }
}
