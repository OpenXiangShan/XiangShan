package xiangshan.backend.fu

import chisel3._
import org.chipsalliance.cde.config.Parameters
import xiangshan.ExceptionNO._
import xiangshan.backend.Bundles.{ExuInput, NewExuInput}
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.decode._
import xiangshan.backend.fu.FuConfig.VectorV2Config
import xiangshan.backend.fu.fpu.IntFPToVec
import xiangshan.backend.fu.wrapper._
import xiangshan.backend.vector.Exu
import xiangshan.mem.{Std, VStd}

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
  * @param needWidenOut the $fu need isWiden in vector output data
  * @param needNarrowOut the $fu need isNarrow in vector output data
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
  * @param writeVType if the $fu need write vtype
  * @param immType the immediate type of this $fu
  * @param vlWakeUp
  * @param maskWakeUp
  * @param readVl if the $fu need read vl
  * @param readOldVtype if the $fu need read old vtype
  *
  * @define fu function unit
  */
case class FuConfig (
  name          : String,
  fuType        : FuType.OHType,
  fuGen         : (Parameters, FuConfig) => FuncUnit =  (_, cfg) => throw new NotImplementedError(s"fuGen is not set for ${cfg.name}"),
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
  needWidenOut  : Boolean = false,
  needNarrowOut : Boolean = false,
  destDataBits  : Int = 64,
  srcDataBits   : Option[Int] = None,
  srcNeedCopy   : Boolean = false,
  latency       : HasFuLatency = CertainLatency(0), // two field (base latency, extra latency(option))
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
  readOldVtype  : Boolean = false,
) extends VectorV2Config {
  require(srcData.forall(!_.contains(V0Data())), s"V0Data() should not appear in srcData args")
  require(srcData.forall(!_.contains(VlData())), s"VlData() should not appear in srcData args")

  def needIntWen: Boolean = writeIntRf || writeFakeIntRf
  def needFpWen:  Boolean = writeFpRf
  def needVecWen: Boolean = writeVecRf
  def needV0Wen:  Boolean = writeV0Rf
  def needVlWen:  Boolean = writeVlRf

  require(!piped || piped && latency.latencyVal.isDefined, "The latency value must be set when piped is enable")

  def numIntSrc : Int = srcData.map(_.count(x => IntRegSrcDataSet.contains(x))).fold(0)(_ max _)
  def numFpSrc  : Int = srcData.map(_.count(x => FpRegSrcDataSet.contains(x))).fold(0)(_ max _)
  def numVecSrc : Int = srcData.map(_.count(x => VecRegSrcDataSet.contains(x))).fold(0)(_ max _)
  def numVfSrc  : Int = srcData.map(_.count(x => VecRegSrcDataSet.contains(x))).fold(0)(_ max _)
  def numV0Src  : Int = if (this.readV0) 1 else 0
  def numVlSrc  : Int = if (this.readVl) 1 else 0
  def numRegSrc : Int = srcData.map(_.count(x => RegSrcDataSet.contains(x))).fold(0)(_ max _)
  def numSrc    : Int = (if (isSta) 3 else srcData.map(_.length).fold(0)(_ max _))

  def readFp: Boolean = numFpSrc > 0

  def fuSel(uop: NewExuInput): Bool = {
    // Don't add more shit here!!!
    // Todo: add new FuType to distinguish f2i, f2f
    uop.ctrl.fuType === this.fuType.U
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
  def hasRedirect: Boolean = Seq(FuType.jmp, FuType.njmp, FuType.brh, FuType.csr).contains(fuType)

  def hasIsRVC: Boolean = Seq(FuType.jmp, FuType.njmp, FuType.brh, FuType.csr, FuType.ldu, FuType.stu).contains(fuType)

  def hasRasAction: Boolean = Seq(FuType.jmp, FuType.njmp).contains(fuType)

  def needTargetPc: Boolean = Seq(FuType.jmp, FuType.njmp, FuType.brh).contains(fuType)

  // predict info
  def needPdInfo: Boolean = Seq(FuType.jmp, FuType.njmp, FuType.brh).contains(fuType)

  def needPc: Boolean = Seq(FuType.jmp, FuType.njmp, FuType.link, FuType.brh, FuType.ldu).contains(fuType)

  def aluBjuNeedPc: Boolean = Seq(FuType.jmp, FuType.njmp, FuType.link, FuType.brh).contains(fuType)

  var aluNeedPc: Boolean = false

  def needVecCtrl: Boolean = {
    import FuType._
    Seq(vipu, vialuF, vimac, vidiv, vppu, vfalu, vmove, vfma, vfdiv, vfcvt, vldu, vstu, vsha256ms, vsha256c).contains(fuType)
  }

  def needUncertainWakeup: Boolean = {
    FuConfig.needUncertainWakeupFuConfigs.contains(this)
  }
  def needCriticalErrors: Boolean = Seq(FuType.csr).contains(fuType)

  def isAlu: Boolean = fuType == FuType.alu

  def isMul: Boolean = fuType == FuType.mul

  def isDiv: Boolean = fuType == FuType.div

  def isFalu: Boolean = fuType == FuType.falu

  def isFmul: Boolean = fuType == FuType.fmul

  def isCsr: Boolean = fuType == FuType.csr

  def isBrh: Boolean = fuType == FuType.brh

  def isJmp: Boolean = fuType == FuType.jmp

  def isNewJmp: Boolean = fuType == FuType.njmp

  def isLink: Boolean = fuType == FuType.link

  def isFence: Boolean = fuType == FuType.fence

  def isVecArith: Boolean = fuType == FuType.vialuF || fuType == FuType.vimac ||
                            fuType == FuType.vppu || fuType == FuType.vipu ||
                            fuType == FuType.vfalu || fuType == FuType.vfma ||
                            fuType == FuType.vfdiv || fuType == FuType.vfcvt ||
                            fuType == FuType.vidiv || fuType == FuType.vmove ||
                            fuType == FuType.vsha256ms || fuType == FuType.vsha256c

  def isVecMem: Boolean = fuType == FuType.vldu || fuType == FuType.vstu ||
                          fuType == FuType.vsegldu || fuType == FuType.vsegstu ||
                          name == "vstd"

  def needOg2: Boolean = isVecArith || isVecMem

  def isSta: Boolean = name.contains("sta")

  def isStd: Boolean = name.contains("std")

  def ckAlwaysEn: Boolean = isCsr || isFence

  def needInstFrm: Boolean = needSrcFrm && !needOg2

  override def toString: String = {
    var str = s"${this.name}: "
    str += s"latency($latency)"
    str += s"src($srcData)"
    str
  }
}

object FuConfig {
  val JmpCfg: FuConfig = FuConfig (
    name = "jmp",
    fuType = FuType.jmp,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new JumpUnit(cfg)(p)).suggestName("jmp"),
    srcData = Seq(
      Seq(IntData()), // jal
    ),
    piped = true,
    immType = Set(Imm_I(), Imm_J(), Imm_U()),
  )

  val NJmpCfg: FuConfig = FuConfig (
    name = "njmp",
    fuType = FuType.njmp,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new NewJumpUnit(cfg)(p)).suggestName("njmp"),
    srcData = Seq(
      Seq(IntData()), // jal -> Seq(ja, link)
    ),
    piped = true,
    immType = Set(Imm_I(), Imm_J()),
  )

  val LinkCfg: FuConfig = FuConfig (
    name = "link",
    fuType = FuType.link,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new LinkUnit(cfg)(p)).suggestName("link"),
    srcData = Seq(Seq(NoData())), // pc + 4 / pc + imm
    piped = true,
    writeIntRf = true,
    immType = Set(Imm_U()),
  )

  val BrhCfg: FuConfig = FuConfig (
    name = "brh",
    fuType = FuType.brh,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new BranchUnit(cfg)(p).suggestName("brh")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    piped = true,
    immType = Set(Imm_B()),
  )

  val I2fCfg: FuConfig = FuConfig (
    name = "i2f",
    FuType.i2f,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new I2F(cfg)(p).suggestName("i2f")),
    srcData = Seq(
      Seq(IntData()),
    ),
    piped = true,
    writeFpRf = true,
    writeFflags = true,
    latency = CertainLatency(2, extraValue = 1),
    needSrcFrm = true,
  )

  val FcmpCfg: FuConfig = FuConfig(
    name = "fcmp",
    FuType.fcmp,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new FCMP(cfg)(p).suggestName("fcmp")),
    srcData = Seq(
      Seq(FpData(), FpData()),
    ),
    piped = true,
    writeIntRf = true,
    writeFflags = true,
    latency = CertainLatency(0, extraValue = 3),
  )

  val I2vCfg: FuConfig = FuConfig (
    name = "i2v",
    FuType.i2v,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new IntFPToVec(cfg)(p).suggestName("i2v")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    piped = true,
    writeFpRf = true,
    writeVecRf = true,
    writeV0Rf = true,
    latency = CertainLatency(0, extraValue = 3),
    destDataBits = 128,
    srcDataBits = Some(64),
    immType = Set(Imm_OPIVIU(), Imm_OPIVIS(), Imm_VRORVI()),
  )

  val F2vCfg: FuConfig = FuConfig (
    name = "f2v",
    FuType.f2v,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new IntFPToVec(cfg)(p).suggestName("f2v")),
    srcData = Seq(
      Seq(FpData(), FpData()),
      Seq(FpData()),
    ),
    piped = true,
    writeFpRf = true,
    writeVecRf = true,
    writeV0Rf = true,
    latency = CertainLatency(0, extraValue = 3),
    destDataBits = 128,
    srcDataBits = Some(64),
  )

  val CsrCfg: FuConfig = FuConfig (
    name = "csr",
    fuType = FuType.csr,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new CSR(cfg)(p).suggestName("csr")),
    srcData = Seq(
      Seq(IntData()),
    ),
    piped = false,
    writeIntRf = true,
    latency = UncertainLatency(),
    exceptionOut = Seq(illegalInstr, virtualInstr, breakPoint, ecallU, ecallS, ecallVS, ecallM),
    flushPipe = true,
  )

  val AluCfg: FuConfig = FuConfig (
    name = "alu",
    fuType = FuType.alu,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new Alu(cfg)(p).suggestName("Alu")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    piped = true,
    writeIntRf = true,
    immType = Set(Imm_I(), Imm_J(), Imm_U(), Imm_LUI32()),
  )

  val MulCfg: FuConfig = FuConfig (
    name = "mul",
    fuType = FuType.mul,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new MulUnit(cfg)(p).suggestName("Mul")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    piped = true,
    writeIntRf = true,
    latency = CertainLatency(2),
  )

  val DivCfg: FuConfig = FuConfig (
    name = "div",
    fuType = FuType.div,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new DivUnit(cfg)(p).suggestName("Div")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    piped = false,
    writeIntRf = true,
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
    piped = false,
    latency = UncertainLatency(),
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
    piped = true,
    writeIntRf = true,
    latency = CertainLatency(2),
  )

  val VSetCfg: FuConfig = FuConfig (
    name = "vset",
    fuType = FuType.vset,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new VSetUnit(cfg)(p).suggestName("VSet")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    piped = true,
    writeVlRf = true,
    writeVType = true,
    writeIntRf = true,
    latency = CertainLatency(0),
    immType = Set(Imm_VSETVLI(), Imm_VSETIVLI()),
    readVl = true,
  )

  val VSetRvfWvfCfg: FuConfig = FuConfig(
    name = "vsetrvfwvf",
    fuType = FuType.vset,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new VSetRvfWvf(cfg)(p).suggestName("VSetRvfWvf")),
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()),  // vs1, vs2, vd_old
    ),
    piped = true,
    writeVlRf = true,
    writeVType = true,
    writeIntRf = true,
    latency = CertainLatency(0),
    immType = Set(Imm_VSETVLI(), Imm_VSETIVLI()),
    readVl = true,
    readOldVtype = true,
  )

  val VSetRiWvfCfg: FuConfig = FuConfig(
    name = "vsetriwvf",
    fuType = FuType.vset,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new VSetRiWvf(cfg)(p).suggestName("VSetRiWvf")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    piped = true,
    writeVlRf = true,
    writeVType = true,
    latency = CertainLatency(0),
    immType = Set(Imm_VSETVLI(), Imm_VSETIVLI()),
  )

  val VSetRiWiCfg: FuConfig = FuConfig(
    name = "vsetriwi",
    fuType = FuType.vset,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new VSetRiWi(cfg)(p).suggestName("VSetRiWi")),
    srcData = Seq(
      Seq(IntData(), IntData()),
    ),
    piped = true,
    writeIntRf = true,
    latency = CertainLatency(0),
    immType = Set(Imm_VSETVLI(), Imm_VSETIVLI()),
  )

  val LduCfg: FuConfig = FuConfig (
    name = "ldu",
    fuType = FuType.ldu,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(IntData()),
    ),
    piped = false, // Todo: check it
    writeIntRf = true,
    writeFpRf = true,
    writeVecRf = true,
    writeV0Rf = true,
    latency = UncertainLatency(3),
    exceptionOut = Seq(loadAddrMisaligned, loadAccessFault, loadPageFault, loadGuestPageFault, storeAddrMisaligned, storeAccessFault, storePageFault, storeGuestPageFault, breakPoint, hardwareError), // for sc from atomics unit
    flushPipe = false,
    replayInst = false,
    hasLoadError = true,
    trigger = true,
    immType = Set(Imm_I()),
    readVl = true,
    readV0 = true,
    readVType = true,
  )

  val StaCfg: FuConfig = FuConfig (
    name = "sta",
    fuType = FuType.stu,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(IntData()),
    ),
    piped = false,
    latency = UncertainLatency(),
    exceptionOut = Seq(storeAddrMisaligned, storeAccessFault, storePageFault, storeGuestPageFault, breakPoint, hardwareError),
    flushPipe = false,
    trigger = true,
    immType = Set(Imm_S()),
    readVl = true,
    readV0 = true,
    readVType = true,
  )

  val StdCfg: FuConfig = FuConfig (
    name = "std",
    fuType = FuType.stu,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new Std(cfg)(p).suggestName("Std")),
    srcData = Seq(
      Seq(IntData()),
      Seq(FpData()),
    ),
    piped = true,
    latency = CertainLatency(0)
  )

  val HyldaCfg = FuConfig (
    name = "hylda",
    fuType = FuType.ldu,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(IntData()),
    ),
    piped = false, // Todo: check it
    writeIntRf = true,
    writeFpRf = true,
    latency = UncertainLatency(3),
    exceptionOut = Seq(loadAddrMisaligned, loadAccessFault, loadPageFault, loadGuestPageFault, breakPoint, hardwareError),
    flushPipe = false,
    replayInst = false,
    hasLoadError = true,
    immType = Set(Imm_I()),
  )

  val HystaCfg = FuConfig (
    name = "hysta",
    fuType = FuType.stu,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(IntData()),
    ),
    piped = false,
    latency = UncertainLatency(),
    exceptionOut = Seq(storeAddrMisaligned, storeAccessFault, storePageFault, storeGuestPageFault, breakPoint, hardwareError),
    immType = Set(Imm_S()),
  )

  val FakeHystaCfg = FuConfig (
    name = "hysta",
    fuType = FuType.stu,
    fuGen = null, // Todo
    srcData = Seq(),
    piped = false,
    latency = UncertainLatency(),
    exceptionOut = Seq(storeAddrMisaligned, storeAccessFault, storePageFault, storeGuestPageFault),
    immType = Set(),
  )

  val MouCfg: FuConfig = FuConfig (
    name = "mou",
    fuType = FuType.mou,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(IntData()),
    ),
    piped = false, // Todo: check it
    writeFakeIntRf = true,
    latency = UncertainLatency(),
    exceptionOut = (LduCfg.exceptionOut ++ StaCfg.exceptionOut ++ StdCfg.exceptionOut).distinct,
    trigger = true,
  )

  val MoudCfg: FuConfig = FuConfig (
    name = "moud",
    fuType = FuType.mou,
    fuGen = null, // Todo
    srcData = Seq(
      Seq(IntData()),
    ),
    piped = true,
    latency = CertainLatency(0),
  )

  val VialuCfg = FuConfig (
    name = "vialuFix",
    fuType = FuType.vialuF,
    fuGen = null,
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()),  // vs1, vs2, vd_old
      Seq(IntData(), VecData(), VecData()),  // rs1, vs2, vd_old
    ),
    piped = true,
    writeVecRf = true,
    writeV0Rf = true,
    writeVxsat = true,
    needWidenOut = true,
    needNarrowOut = true,
    needSrcVxrm = true,
    latency = CertainLatency(1),
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    exceptionOut = Seq(illegalInstr),
    readV0 = true,
    readVl = true,
    readVType = true,
    immType = Set(Imm_OPIVIS(), Imm_OPIVIU()),
  )

  val VimacCfg = FuConfig (
    name = "vimac",
    fuType = FuType.vimac,
    fuGen = null,
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()), // vs1, vs2, vd_old
      Seq(IntData(), VecData(), VecData()), // rs1, vs2, vd_old
    ),
    piped = true,
    writeVecRf = true,
    writeV0Rf = true,
    writeVxsat = true,
    needWidenOut = true,
    needSrcVxrm = true,
    latency = CertainLatency(2),
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    exceptionOut = Seq(illegalInstr),
    readV0 = true,
    readVl = true,
    readVType = true,
    immType = Set(Imm_OPIVIS(), Imm_OPIVIU()),
  )

  val VidivCfg = FuConfig (
    name = "vidiv",
    fuType = FuType.vidiv,
    fuGen = null,
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()), // vs1, vs2, vd_old
      Seq(IntData(), VecData(), VecData()), // rs1, vs2, vd_old
    ),
    piped = false,
    writeVecRf = true,
    writeV0Rf = true,
    latency = UncertainLatency(),
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    exceptionOut = Seq(illegalInstr),
    readVl = true,
    readV0 = true,
    readVType = true,
    immType = Set(Imm_OPIVIS(), Imm_OPIVIU()),
  )

  val VppuCfg = FuConfig (
    name = "vppu",
    fuType = FuType.vppu,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new VPPU(cfg)(p).suggestName("Vppu")),
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()),  // vs1, vs2, vd_old
    ),
    piped = true,
    writeVecRf = true,
    writeV0Rf = true,
    latency = CertainLatency(2),
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    exceptionOut = Seq(illegalInstr),
    readV0 = true,
    readVl = true,
    readVType = true,
    immType = Set(Imm_OPIVIS(), Imm_OPIVIU()),
  )

  val VipuCfg: FuConfig = FuConfig (
    name = "vipu",
    fuType = FuType.vipu,
    fuGen = null,
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()),  // vs1, vs2, vd_old
    ),
    piped = true,
    writeIntRf = true,
    writeVecRf = true,
    writeV0Rf = true,
    latency = CertainLatency(2),
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    exceptionOut = Seq(illegalInstr),
    readV0 = true,
    readVl = true,
    readVType = true,
    immType = Set(Imm_OPIVIS(), Imm_OPIVIU()),
  )

  val VmoveCfg = FuConfig (
    name = "vmove",
    fuType = FuType.vmove,
    fuGen = null,
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()), // vs1, vs2, vd_old
    ),
    piped = true,
    writeIntRf = true,
    writeFpRf = true,
    writeVecRf = true,
    writeV0Rf = true,
    latency = CertainLatency(0),
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    readV0 = true,
    readVl = true,
    readVType = true,
  )

  val VfaluCfg = FuConfig (
    name = "vfalu",
    fuType = FuType.vfalu,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new VFAlu(cfg)(p).suggestName("Vfalu")),
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()), // vs1, vs2, vd_old
    ),
    piped = true,
    writeVecRf = true,
    writeV0Rf = true,
    writeFpRf = true,
    writeFflags = true,
    latency = CertainLatency(1),
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    exceptionOut = Seq(illegalInstr),
    needSrcFrm = true,
    readV0 = true,
    readVl = true,
    readVType = true,
  )

  val VfmaCfg = FuConfig (
    name = "vfma",
    fuType = FuType.vfma,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new VFMA(cfg)(p).suggestName("Vfma")),
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()), // vs1, vs2, vd_old
    ),
    piped = true,
    writeVecRf = true,
    writeV0Rf = true,
    writeFflags = true,
    needWidenOut = true,
    latency = CertainLatency(3),
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    exceptionOut = Seq(illegalInstr),
    needSrcFrm = true,
    readV0 = true,
    readVl = true,
    readVType = true,
  )

  val VfdivCfg = FuConfig(
    name = "vfdiv",
    fuType = FuType.vfdiv,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new VFDivSqrt(cfg)(p).suggestName("Vfdiv")),
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()), // vs1, vs2, vd_old
    ),
    piped = false,
    writeVecRf = true,
    writeV0Rf = true,
    writeFflags = true,
    latency = UncertainLatency(),
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    exceptionOut = Seq(illegalInstr),
    needSrcFrm = true,
    readV0 = true,
    readVl = true,
    readVType = true,
  )

  val VfcvtCfg = FuConfig(
    name = "vfcvt",
    fuType = FuType.vfcvt,
    fuGen = null,
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()), // vs1, vs2, vd_old
    ),
    piped = true,
    writeVecRf = true,
    writeV0Rf = true,
    writeFflags = true,
    needWidenOut = true,
    needNarrowOut = true,
    latency = CertainLatency(2),
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    exceptionOut = Seq(illegalInstr),
    needSrcFrm = true,
    readV0 = true,
    readVl = true,
    readVType = true,
  )

  val VSha256msCfg = FuConfig(
    name = "vsha256ms",
    fuType = FuType.vsha256ms,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new VSha256msWrapper(cfg)(p).suggestName("VSha256ms")),
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()), // vs1, vs2, vs3
    ),
    piped = false,
    writeVecRf = true,
    writeV0Rf = true,
    writeFflags = false,
    latency = CertainLatency(4),
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    needSrcFrm = true,
    readV0 = true,
    readVl = true,
    readVType = true,
  )

  val VSha256cCfg = FuConfig(
    name = "vsha256c",
    fuType = FuType.vsha256c,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new VSha256cWrapper(cfg)(p).suggestName("VSha256c")),
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()), // vs1, vs2, vs3
    ),
    piped = true,
    writeVecRf = true,
    writeV0Rf = true,
    writeFflags = false,
    latency = CertainLatency(2),
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    needSrcFrm = true,
    readV0 = true,
    readVl = true,
    readVType = true,
  )

  val FaluCfg = FuConfig(
    name = "falu",
    fuType = FuType.falu,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new FAluV2(cfg)(p).suggestName("Falu")),
    srcData = Seq(
      Seq(FpData(), FpData()),
    ),
    piped = true,
    writeFpRf = true,
    writeFflags = true,
    latency = CertainLatency(1),
    destDataBits = 64,
    needSrcFrm = true,
    srcNeedCopy = true,
  )

  val FmulCfg = FuConfig(
    name = "fmul",
    fuType = FuType.fmul,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new FMul(cfg)(p).suggestName("Fmul")),
    srcData = Seq(
      Seq(FpData(), FpData(), FpData()),
    ),
    piped = true,
    writeFpRf = true,
    writeFflags = true,
    latency = CertainLatency(value = 2, isFmul = true),
    destDataBits = 64,
    needSrcFrm = true,
  )

  val FdivCfg = FuConfig(
    name = "fdiv",
    fuType = FuType.fDivSqrt,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new FDivSqrt(cfg)(p).suggestName("Fdiv")),
    srcData = Seq(
      Seq(FpData(), FpData()),
    ),
    piped = false,
    writeFpRf = true,
    writeFflags = true,
    latency = UncertainLatency(),
    destDataBits = 64,
    needSrcFrm = true,
  )

  val FcvtCfg = FuConfig(
    name = "fcvt",
    fuType = FuType.fcvt,
    fuGen = (p: Parameters, cfg: FuConfig) => Module(new FCVT(cfg)(p).suggestName("Fcvt")),
    srcData = Seq(
      Seq(FpData()),
    ),
    piped = true,
    writeFpRf = true,
    writeIntRf = true,
    writeFflags = true,
    latency = CertainLatency(2, extraValue = 1),
    destDataBits = 64,
    needSrcFrm = true,
  )

  val VStdCfg: FuConfig = FuConfig(
    name = "vstd",
    fuType = FuType.stu,
    fuGen = null,
    srcData = Seq(
      Seq(VecData()),
    ),
    piped = true,
    latency = CertainLatency(0),
    destDataBits = 128,
  )

  val VlduCfg: FuConfig = FuConfig (
    name = "vldu",
    fuType = FuType.vldu,
    fuGen = null,
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()),  //vs1, vs2, vd_old
    ),
    piped = false, // Todo: check it
    writeVecRf = true,
    writeV0Rf = true,
    latency = UncertainLatency(),
    exceptionOut = Seq(loadAddrMisaligned, loadAccessFault, loadPageFault, loadGuestPageFault, breakPoint, hardwareError),
    flushPipe = false,
    replayInst = false,
    trigger = true,
    hasLoadError = true,
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    readV0 = true,
    readVl = true,
    readVType = true,
  )

  val VstuCfg: FuConfig = FuConfig (
    name = "vstu",
    fuType = FuType.vstu,
    fuGen = null,
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()),  //vs1, vs2, vd_old
    ),
    piped = false,
    latency = UncertainLatency(),
    exceptionOut = Seq(storeAddrMisaligned, storeAccessFault, storePageFault, storeGuestPageFault, breakPoint, hardwareError),
    flushPipe = false,
    replayInst = false,
    trigger = true,
    hasLoadError = true,
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    readV0 = true,
    readVl = true,
    readVType = true,
  )

  val VseglduCfg: FuConfig = FuConfig (
    name = "vsegldu",
    fuType = FuType.vsegldu,
    fuGen = null,
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()), //vs1, vs2, vd_old
    ),
    piped = false, // Todo: check it
    writeVecRf = true,
    writeV0Rf = true,
    latency = UncertainLatency(),
    exceptionOut = Seq(loadAddrMisaligned, loadAccessFault, loadPageFault, breakPoint, hardwareError),
    flushPipe = false,
    replayInst = false,
    trigger = true,
    hasLoadError = true,
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    readV0 = true,
    readVl = true,
    readVType = true,
  )

  val VsegstuCfg: FuConfig = FuConfig(
    name = "vsegstu",
    fuType = FuType.vsegstu,
    fuGen = null,
    srcData = Seq(
      Seq(VecData(), VecData(), VecData()), //vs1, vs2, vd_old
    ),
    piped = false,
    latency = UncertainLatency(),
    exceptionOut = Seq(storeAddrMisaligned, storeAccessFault, storePageFault, breakPoint, hardwareError),
    flushPipe = false,
    replayInst = false,
    trigger = true,
    hasLoadError = true,
    vlWakeUp = true,
    maskWakeUp = true,
    destDataBits = 128,
    readV0 = true,
    readVl = true,
    readVType = true,
  )

  def allConfigs = Seq(
    JmpCfg, NJmpCfg, LinkCfg, BrhCfg, I2fCfg, FcmpCfg, I2vCfg, F2vCfg, CsrCfg, AluCfg, MulCfg, DivCfg, FenceCfg, BkuCfg,
    VSetCfg, VSetRvfWvfCfg, VSetRiWvfCfg, VSetRiWiCfg,
    LduCfg, StaCfg, StdCfg, HyldaCfg, HystaCfg, FakeHystaCfg, MouCfg, MoudCfg,
    VialuCfg, VimacCfg, VidivCfg, VppuCfg, VipuCfg, VmoveCfg, VfaluCfg, VfmaCfg, VfdivCfg, VfcvtCfg, VSha256msCfg, VSha256cCfg,
    FaluCfg, FmulCfg, FdivCfg, FcvtCfg,
    VStdCfg, VlduCfg, VstuCfg, VseglduCfg, VsegstuCfg
  )

  def VecArithFuConfigs = Seq(
    VialuCfg, VimacCfg, VppuCfg, VipuCfg, VfaluCfg, VmoveCfg, VfmaCfg, VfcvtCfg, VfdivCfg
  )

  def needUncertainWakeupFuConfigs = Seq(
    CsrCfg, DivCfg, FdivCfg, VfdivCfg, VidivCfg
  )

  trait VectorV2Config { self: FuConfig =>
    def writeGpRf = this.writeIntRf
    def writeVpRf = this.writeVecRf

    def needFlushPipe = this.flushPipe
    def needVM = this.readV0
    def readOldVType = this.writeVlRf

  }
}
