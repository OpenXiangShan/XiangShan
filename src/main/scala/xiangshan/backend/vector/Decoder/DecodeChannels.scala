package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.experimental.hierarchy.core.{Definition, Instance}
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import top.{ArgParser, Generator}
import utils.BundleUtils.makeValid
import xiangshan._
import xiangshan.backend.Bundles.{DecodeInUopDebug, UopIdx}
import xiangshan.backend.decode.isa.Extensions.{A, D, ExtBase, F, I, M, S, System, V, Za64rs, Zawrs, Zba, Zbb, Zbc, Zbkb, Zbkc, Zbkx, Zbs, Zicsr}
import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, Riscv32BitInst}
import xiangshan.backend.fu.FuType
import xiangshan.backend.vector.Decoder.DecodeChannel._
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.Select.{BufferSelectModule, UopSelectModule}
import xiangshan.backend.vector.Decoder.Split.VecUopSplitModule
import xiangshan.backend.vector.Decoder.Types.{SelImm, _}
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt
import xiangshan.backend.vector.util.Verilog
import xiangshan.frontend.PreDecodeInfo
import xiangshan.frontend.ftq.FtqPtr

import scala.language.implicitConversions

class DecodeChannels(
  mopWidth: Int,
  uopWidth: Int,
  extensions: Seq[ExtBase],
  numM2M4M8Channel: (Int, Int, Int) = (8, 8, 8),
)(
  implicit p: Parameters
) extends Module with HasVectorSettings {
  val MaxM2UopIdx = numM2M4M8Channel._1
  val MaxM4UopIdx = numM2M4M8Channel._2
  val MaxM8UopIdx = numM2M4M8Channel._3
  require(mopWidth >= MaxM8UopIdx && MaxM8UopIdx >= MaxM4UopIdx && MaxM4UopIdx >= MaxM2UopIdx)

  override def desiredName: String = s"DecodeChannels" +
    s"_MOP${mopWidth}_UOP${uopWidth}" +
    s"_M2x${MaxM2UopIdx}_M4x${MaxM4UopIdx}_M8x${MaxM8UopIdx}"

  val simpleExts: Seq[ExtBase] = extensions.filterNot(Seq(V).contains)
  val simpleInsts = InstPattern.extensionInsts(simpleExts: _*)
  val simpleTable = simpleExts.map(_.table).reduce(_ ++ _)

  val vectorExts = extensions.filter(Seq(V).contains)
  val vectorInsts = InstPattern.extensionInsts(vectorExts: _*).map(_.asInstanceOf[VecInstPattern])
  for (inst <- vectorInsts) {
    println(inst)
  }

  val uopBufferSize = maxSplitUopNum - 1

  val in = IO(new Bundle {
    // used to flush UopBuffer
    val redirect = Input(Bool())
    val mops = Flipped(Vec(mopWidth, DecoupledIO(new Bundle {
      val info = new DecodeChannelInput
      val ctrl = new MopCtrlBundle
    } )))
  })

  val out = IO(Output(new Bundle {
    val uops = Vec(uopWidth, ValidIO(new Bundle {
      val info = new DecodeChannelOutput
      val ctrl = new MopCtrlBundle
    }))
  }))

  val instValids = in.mops.map(_.valid)
  val insts: Seq[Riscv32BitInst with BitFieldsVec] = in.mops.map(_.bits.info.rawInst.asTypeOf(new Riscv32BitInst with BitFieldsVec))
  val inMopCtrl: Seq[MopCtrlBundle] = in.mops.map(_.bits.ctrl)

  val vecDecodeChannelM8: Definition[VectorDecodeChannel] = Definition(new VectorDecodeChannel(vectorInsts))

//  lazy val vecDecodeChannelM8: Definition[VecDecodeChannel] = Definition(new VecDecodeChannel(vecInstPatterns, enableM2M4M8 = (true, true, true)))
//  lazy val vecDecodeChannelM4: Definition[VecDecodeChannel] = Definition(new VecDecodeChannel(vecInstPatterns, enableM2M4M8 = (true, true, false)))
//  lazy val vecDecodeChannelM2: Definition[VecDecodeChannel] = Definition(new VecDecodeChannel(vecInstPatterns, enableM2M4M8 = (true, false, false)))
//  lazy val vecDecodeChannelM1: Definition[VecDecodeChannel] = Definition(new VecDecodeChannel(vecInstPatterns, enableM2M4M8 = (false, false, false)))

  val simpleDecodeChannel: Definition[SimpleDecodeChannel] = Definition(new SimpleDecodeChannel(simpleInsts, simpleTable))

  val vecDecodeChannels: Seq[Instance[VectorDecodeChannel]] = Seq.tabulate(mopWidth) {
    i => Instance(vecDecodeChannelM8)
//      if (i < MaxM8UopIdx)
//        Instance(vecDecodeChannelM8)
//      else if(i < MaxM4UopIdx)
//        Instance(vecDecodeChannelM8)
//      else if(i < MaxM4UopIdx)
//        Instance(vecDecodeChannelM8)
//      else
//        Instance(vecDecodeChannelM8)
  }
  val simpleDecodeChannels = Seq.fill(mopWidth)(Instance(simpleDecodeChannel))

  val uopBufferUpdateVec = Wire(Vec(uopBufferLength, Bool()))
  val uopBufferUpdate = Cat(uopBufferUpdateVec).orR

  val vecUopOuts: Seq[Seq[ValidIO[VecDecodeChannelOutputUop]]] = vecDecodeChannels.map(_.out.uop)
  val simUopOuts: Seq[ValidIO[SimpleDecodeChannelOutput]] = simpleDecodeChannels.map(_.out)

  val vecChannelSelectVec: Seq[Bool] = insts.map(inst =>
    inst.isVecArith || inst.isVecLoad || inst.isVecStore
  )

  val vecChannelOut: Seq[Seq[ValidIO[DecodeChannelOutput]]] =
    vecUopOuts.map(_.map(x => makeValid(x.valid, DecodeChannelOutput.fromVecChannelUop(x.bits))))
  val simChannelOut: Seq[ValidIO[DecodeChannelOutput]] =
    simUopOuts.map(x => makeValid(x.valid, DecodeChannelOutput.fromSimpleChannelUop(x.bits)))

  // should be 0~7
  val uopBufferNumNext = Wire(UInt(log2Up(uopBufferSize).W))
  val uopBufferNum = RegEnable(uopBufferNumNext, 0.U(log2Up(uopBufferSize).W), uopBufferUpdate || in.redirect)

  val uopBufferValid = Wire(Vec(uopBufferSize, Bool()))
  val uopBufferNext = Wire(Vec(uopBufferSize, new DecodeChannelOutput))
  val uopBuffer: Seq[DecodeChannelOutput] = (uopBufferNext zip uopBufferUpdateVec).map {
    case (next, update) =>
      RegEnable(next, update)
  }


  val bufferedMopCtrlNext = Wire(new MopCtrlBundle)
  val bufferedMopCtrl = RegEnable(bufferedMopCtrlNext, uopBufferUpdate)

  val uopBufferCtrlDecoder = Module(new UopBufferCtrlDecoder(
    mopWidth = mopWidth,
    uopWidth = uopWidth,
    uopBufferLength = uopBufferSize,
    numM2M4M8Channel = numM2M4M8Channel,
  ))

  val uopSelectMod = Module(new UopSelectModule(
    uopBundle = new DecodeChannelOutput,
    mopBundle = new MopCtrlBundle,
    mopWidth = mopWidth,
    uopWidth = uopWidth,
    uopBufferSize = uopBufferSize,
    filteredPorts = uopBufferCtrlDecoder.uopUsedPorts,
  ))

  val bufferSelectMod = Module(new BufferSelectModule(
    uopBundle = new DecodeChannelOutput,
    mopBundle = new MopCtrlBundle,
    mopWidth = mopWidth,
    uopWidth = uopWidth,
    uopBufferLength = uopBufferSize,
    filteredPorts = uopBufferCtrlDecoder.bufferUsedPorts,
  ))

  /**
   * connection of [[vecDecodeChannels]]
   */

  val vecDecodeChannelsIn: Seq[DecodeChannelInput] = vecDecodeChannels.map(_.in)
  vecDecodeChannelsIn.zipWithIndex.foreach { case (modIn, i) =>
    modIn.rawInst := in.mops(i).bits.info.rawInst
    modIn.vtype := in.mops(i).bits.info.vtype
  }

  val simDecodeChannelsIn: Seq[DecodeChannelInput] = simpleDecodeChannels.map(_.in)
  simDecodeChannelsIn.zipWithIndex.foreach { case (modIn, i) =>
    modIn.rawInst := in.mops(i).bits.info.rawInst
    modIn.vtype := in.mops(i).bits.info.vtype
  }

  /**
   * connection of [[uopBufferCtrlDecoder]]
   */

  uopBufferCtrlDecoder.in.uopBufferNum := uopBufferNum
  uopBufferCtrlDecoder.in.channelUopNum := vecDecodeChannels.indices.map(
    i =>
      Mux(
        instValids(i),
        Mux(
          vecChannelSelectVec(i),
          vecDecodeChannels(i).out.uopNumOH,
          NumUopOH.N1,
        ),
        NumUopOH.N0
      )
  )
  uopBufferNumNext := Mux(in.redirect, 0.U, uopBufferCtrlDecoder.out.uopBufferNum)
  uopBufferUpdateVec := uopBufferCtrlDecoder.out.bufferValids
  uopBufferValid := uopBufferCtrlDecoder.out.bufferValids

  /**
   * connection of [[uopSelectMod]]
   */

  for (i <- 0 until mopWidth) {
    uopSelectMod.in.uopFromChannel(i)(0) := Mux(
      vecChannelSelectVec(i),
      vecChannelOut(i)(0).bits,
      simChannelOut(i).bits
    )
    for (j <- 1 until maxSplitUopNum) {
      uopSelectMod.in.uopFromChannel(i)(j) := vecChannelOut(i)(j).bits
    }
  }
  uopSelectMod.in.uopFromBuffer := uopBuffer
  uopSelectMod.in.mopFromInput := in.mops.map(_.bits.ctrl)
  uopSelectMod.in.mopFromBuffer := bufferedMopCtrl
  uopSelectMod.in.uopSelect := uopBufferCtrlDecoder.out.selForUop

  /**
   * connection of [[bufferSelectMod]]
   */

  bufferSelectMod.in.uopFromChannel := uopSelectMod.in.uopFromChannel
  bufferSelectMod.in.uopFromBuffer := uopBuffer
  bufferSelectMod.in.mopFromInput := in.mops.map(_.bits.ctrl)
  bufferSelectMod.in.mopFromBuffer := bufferedMopCtrl
  bufferSelectMod.in.uopSelect := uopBufferCtrlDecoder.out.selForBufffer

  uopBufferNext := bufferSelectMod.out.decodedInfoOut
  bufferedMopCtrlNext := bufferSelectMod.out.mopToBuffer


  for (i <- 0 until mopWidth) {
    in.mops(i).ready := uopBufferCtrlDecoder.out.acceptVec(i)
  }

  for (i <- 0 until uopWidth) {
    out.uops(i).valid := uopBufferCtrlDecoder.out.uopValids(i)
    out.uops(i).bits.info := uopSelectMod.out.decodedInfoOut(i)
    out.uops(i).bits.ctrl := uopSelectMod.out.bypassInfoOut(i)
  }
}

class DecodeChannelOutput extends Bundle {
  val fuType: UInt = FuType()
  val opcode: UInt = FuOpType()
  val isVset: Bool = Bool()

  val src1Ren = Bool()
  val src1Type = DecodeSrcType()
  val src2Ren = Bool()
  val src2Type = DecodeSrcType()
  val src3Ren = Bool()
  val src3Type = DecodeSrcType()
  val lsrc1 = UInt(5.W)
  val lsrc2 = UInt(5.W)
  val lsrc3 = UInt(5.W)
  val vlRen = Bool()
  val v0Ren = Bool()
  val frmRen = Bool()
  val maskType = MaskTypeChiselEnum()
  val intRmRen = Bool()

  val gpWen = Bool()
  val fpWen = Bool()
  val vpWen = Bool()
  val ldest = UInt(5.W)

  val vlWen = Bool()
  val vxsatWen = Bool()
  val fflagsWen = Bool()

  val noSpec = Bool()
  val blockBack = Bool()
  val flushPipe = Bool()
  val selImm = ValidIO(SelImm())
  val imm = UInt(32.W)
  val commitType = CommitType()
  val vdDepElim = VdDepElim()
  val isWritePartVd = Bool()

  val canRobCompress = Bool()

  val numUop = NumUop()
  val uopIdx = UopIdx()
  val isFirstUop = Bool()
  val isLastUop = Bool()
}

object DecodeChannelOutput {
  def fromVecChannelUop(vuop: VecDecodeChannelOutputUop): DecodeChannelOutput = {
    val uop = Wire(new DecodeChannelOutput)

    uop.fuType := 0.U
    uop.opcode := 0.U
    uop.isVset := vuop.isVset

    uop.src1Ren := vuop.renameInfo.uop.src1Ren
    uop.src1Type := vuop.renameInfo.uop.src1Type
    uop.src2Ren := vuop.renameInfo.uop.src2Ren
    uop.src2Type := vuop.renameInfo.uop.src2Type
    uop.src3Ren := vuop.renameInfo.uop.readVdAsSrc
    uop.src3Type.value := DecodeSrcType.VP
    uop.lsrc1 := vuop.src.src1
    uop.lsrc2 := vuop.src.src2
    uop.lsrc3 := vuop.src.dest
    uop.vlRen := vuop.renameInfo.uop.vlRen
    uop.v0Ren := vuop.renameInfo.uop.v0Ren
    uop.frmRen := vuop.frmRen
    uop.maskType := vuop.renameInfo.uop.maskType
    uop.intRmRen := vuop.renameInfo.uop.intRmRen
    uop.gpWen := vuop.renameInfo.uop.gpWen
    uop.fpWen := vuop.renameInfo.uop.fpWen
    uop.vpWen := vuop.renameInfo.uop.vpWen
    uop.ldest := vuop.src.dest

    uop.vlWen := vuop.renameInfo.uop.vlWen
    uop.vxsatWen := vuop.renameInfo.uop.vxsatWen
    uop.fflagsWen := vuop.fflagsWen

    uop.noSpec := false.B
    uop.blockBack := false.B
    uop.flushPipe := false.B
    uop.selImm.valid := false.B
    uop.selImm.bits := SelImm.OPIVIU // Todo
    uop.imm := 0.U
    uop.commitType := vuop.commitType
    uop.vdDepElim := vuop.vdDepElim
    uop.isWritePartVd := vuop.isWritePartVd

    uop.canRobCompress := false.B

    uop.numUop := vuop.numUop
    uop.uopIdx := vuop.uopIdx
    uop.isFirstUop := vuop.isFirstUop
    uop.isLastUop := vuop.isLastUop

    uop
  }

  def fromSimpleChannelUop(suop: SimpleDecodeChannelOutput): DecodeChannelOutput = {
    val uop = Wire(new DecodeChannelOutput)
    uop.fuType := suop.fuType
    uop.opcode := suop.opcode
    uop.isVset := false.B

    uop.src1Ren := suop.src1RenType.ren
    uop.src1Type := suop.src1RenType.typ
    uop.src2Ren := suop.src2RenType.ren
    uop.src2Type := suop.src2RenType.typ
    uop.src3Ren := suop.src3RenType.ren
    uop.src3Type := suop.src3RenType.typ
    uop.lsrc1 := suop.lsrc1
    uop.lsrc2 := suop.lsrc2
    uop.lsrc3 := suop.lsrc3
    uop.vlRen := false.B
    uop.v0Ren := false.B
    uop.frmRen := suop.frmRen
    uop.maskType := DontCare
    uop.intRmRen := DontCare
    uop.gpWen := suop.gpWen
    uop.fpWen := suop.fpWen
    uop.vpWen := false.B
    uop.ldest := suop.ldest

    uop.vlWen := false.B
    uop.vxsatWen := false.B
    uop.fflagsWen := suop.fflagsWen

    uop.noSpec := suop.noSpec
    uop.blockBack := suop.blockBack
    uop.flushPipe := suop.flushPipe
    uop.selImm := suop.selImm
    uop.imm := suop.imm
    uop.commitType := suop.commitType
    uop.vdDepElim := VdDepElim.Always // never used
    uop.isWritePartVd := false.B

    uop.canRobCompress := suop.canRobCompress

    uop.numUop := suop.numUop
    uop.uopIdx := 0.U
    uop.isFirstUop := true.B
    uop.isLastUop := true.B

    uop
  }
}

class MopCtrlBundle(implicit p: Parameters) extends XSBundle {
  val foldpc           = UInt(MemPredPCWidth.W)
  val exceptionVec     = ExceptionVec()
  val isFetchMalAddr   = Bool()
  val trigger          = TriggerAction()
  val isRVC            = Bool()
  val fixedTaken       = Bool()
  val predTaken        = Bool()
  val crossPageIPFFix  = Bool()
  val ftqPtr           = new FtqPtr
  val ftqOffset        = UInt(log2Up(FetchBlockInstOffsetWidth).W)
  val isLastInFtqEntry = Bool()
  val rawInst          = UInt(32.W)
  val debug            = Option.when(backendParams.debugEn)(new DecodeInUopDebug())
}

class SrcInfo extends Bundle {
  val typ = DecodeSrcType()
  val idx = UInt(5.W)
}

class SrcRenType extends Bundle {
  val ren = Bool()
  val typ = DecodeSrcType()
}

object SrcRenType {
  def genBitPat(
    typ: Option[OperandType],
  ): BitPat = {
    val typBP = OperandType.genBitPat(typ)
    typ.nonEmpty.toBitPat ## typBP
  }

  def genBitPat(
    typ: OperandType,
  ): BitPat = {
    genBitPat(Some(typ))
  }
}

object DecodeChannelsMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => config(XSTileKey).head
    case XSVectorParamKey => XSVectorParameters(128)
  })

  val extensions: Seq[ExtBase] = Seq(
    I, M, A, F, D, Zicsr,
    System, S,
    Za64rs, /*Zacas,*/ Zawrs,
    Zba, Zbb, Zbc, Zbs, Zbkb, Zbkc, Zbkx,
    V,
    // Zcb, Zcmop,
    // Zfa, Zfh, ZfaZfh, ZfaF, ZfaD, Zfhmin,
  )

  Verilog.emitVerilog(
    new DecodeChannels(
      mopWidth = 8,
      uopWidth = 8,
      extensions = extensions,
      numM2M4M8Channel = (8, 8, 8),
    )(defaultConfig),
    Array(
      "--full-stacktrace",
      "--target-dir", "build/decoder",
    ),
  )
}

object UopSplitModuleMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => config(XSTileKey).head
    case XSVectorParamKey => XSVectorParameters(128)
  })

  Generator.execute(
    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "backend",
    new VecUopSplitModule()(defaultConfig),
    firtoolOpts
  )
}
