package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.experimental.hierarchy.core.{Definition, Instance}
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import top.{ArgParser, Generator}
import utils.BundleUtils.makeValid
import xiangshan._
import xiangshan.backend.Bundles.{DecodeInUopDebug, UopIdx}
import xiangshan.backend.decode.isa.Extensions._
import xiangshan.backend.decode.isa.bitfield.{BitFieldsVec, Riscv32BitInst}
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.decode.opcode.Opcode.Opcode
import xiangshan.backend.fu.FuType
import xiangshan.backend.fu.vector.Bundles.VType
import xiangshan.backend.vector.Decoder.DecodeChannel.VectorDecodeChannel.VecDecodeChannelOutputUop
import xiangshan.backend.vector.Decoder.DecodeChannel._
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.Frm
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.Select.{BufferSelectModule, UopSelectModule}
import xiangshan.backend.vector.Decoder.Split.VecUopSplitModule
import xiangshan.backend.vector.Decoder.Types._
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt
import xiangshan.backend.vector.util.Select.Mux1HLookUp
import xiangshan.backend.vector.util.Verilog
import xiangshan.frontend.ftq.FtqPtr

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.language.implicitConversions

class DecodeChannels(
  mopWidth: Int,
  uopWidth: Int,
  extensions: Seq[ExtBase],
  numM2M4M8Channel: (Int, Int, Int) = (8, 8, 8),
  postfix: String = "",
)(
  implicit p: Parameters
) extends Module with HasVectorSettings {
  val MaxM2UopIdx = numM2M4M8Channel._1
  val MaxM4UopIdx = numM2M4M8Channel._2
  val MaxM8UopIdx = numM2M4M8Channel._3
  require(mopWidth >= MaxM8UopIdx && MaxM8UopIdx >= MaxM4UopIdx && MaxM4UopIdx >= MaxM2UopIdx)

  override def desiredName: String = s"DecodeChannels${postfix}" +
    s"_MOP${mopWidth}_UOP${uopWidth}" +
    s"_M2x${MaxM2UopIdx}_M4x${MaxM4UopIdx}_M8x${MaxM8UopIdx}"

  val simpleExts: Seq[ExtBase] = extensions.filterNot(Seq(V, Zvbb, Zvknha).contains)
  val simpleInsts = InstPattern.extensionInsts(simpleExts: _*)
  val simpleTable = simpleExts.map(_.table).reduce(_ ++ _)

  val vectorExts = extensions.filter(Seq(V, Zvbb, Zvknha).contains)
  val vectorInsts = InstPattern.extensionInsts(vectorExts: _*).map(_.asInstanceOf[VecInstPattern])

  val uopBufferSize = maxSplitUopNum - 1

  val in = IO(new Bundle {
    // used to flush UopBuffer
    val redirect = Input(Bool())
    // if rename can accept all UopWidth uops
    val renameCanAccept = Input(Bool())
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

  val vectorDecodeChannelM8: Definition[VectorDecodeChannel] = Definition(new VectorDecodeChannel(vectorInsts))
  val vsetDecodeChannel: Definition[VsetDecoder] = Definition(new VsetDecoder)

//  lazy val vecDecodeChannelM8: Definition[VecDecodeChannel] = Definition(new VecDecodeChannel(vecInstPatterns, enableM2M4M8 = (true, true, true)))
//  lazy val vecDecodeChannelM4: Definition[VecDecodeChannel] = Definition(new VecDecodeChannel(vecInstPatterns, enableM2M4M8 = (true, true, false)))
//  lazy val vecDecodeChannelM2: Definition[VecDecodeChannel] = Definition(new VecDecodeChannel(vecInstPatterns, enableM2M4M8 = (true, false, false)))
//  lazy val vecDecodeChannelM1: Definition[VecDecodeChannel] = Definition(new VecDecodeChannel(vecInstPatterns, enableM2M4M8 = (false, false, false)))

  val simpleDecodeChannel: Definition[SimpleDecodeChannel] = Definition(new SimpleDecodeChannel(simpleInsts, simpleTable))
  val pseudoDecodeChannel: Definition[PseudoDecodeChannel] = Definition(new PseudoDecodeChannel())

//  out.bits.imm := Mux1HLookUp(
//    out.bits.selImm.bits,
//    Seq(
//      DecodeSelImm.CSRRVLENB -> (VLEN / 8).U,
//    ),
//  )

  val vectorDecodeChannels: Seq[Instance[VectorDecodeChannel]] = Seq.tabulate(mopWidth) {
    i => Instance(vectorDecodeChannelM8)
//      if (i < MaxM8UopIdx)
//        Instance(vecDecodeChannelM8)
//      else if(i < MaxM4UopIdx)
//        Instance(vecDecodeChannelM8)
//      else if(i < MaxM4UopIdx)
//        Instance(vecDecodeChannelM8)
//      else
//        Instance(vecDecodeChannelM8)
  }
  val vsetDecodeChannels = Seq.fill(mopWidth)(Instance(vsetDecodeChannel))
  val simpleDecodeChannels = Seq.fill(mopWidth)(Instance(simpleDecodeChannel))
  val pseudoDecodeChannels = Seq.fill(mopWidth)(Instance(pseudoDecodeChannel))

  val uopBufferUpdateVec = Wire(Vec(uopBufferLength, Bool()))
  val uopBufferUpdate = Cat(uopBufferUpdateVec).orR

  val vecUopOuts: Seq[Seq[ValidIO[VecDecodeChannelOutputUop]]] = vectorDecodeChannels.map(_.out.uop)
  val vsetUopOuts: Seq[ValidIO[VsetDecoder.Out]] = vsetDecodeChannels.map(_.out)
  val simUopOuts: Seq[ValidIO[SimpleDecodeChannelOutput]] = simpleDecodeChannels.map(_.out)
  val psdUopOuts: Seq[ValidIO[PseudoDecodeChannel.Out]] = pseudoDecodeChannels.map(_.out)

  val vecUopNumOHs: Seq[NumUopOH.Type] = vectorDecodeChannels.map(_.out.uopNumOH)

  val vecChannelOut: Seq[Seq[ValidIO[DecodeChannelOutput]]] =
    vecUopOuts.map(_.map(x => makeValid(x.valid, DecodeChannelOutput.fromVecChannelUop(x.bits))))
  val vsetChannelOut =
    vsetUopOuts.map(x => makeValid(x.valid, DecodeChannelOutput.fromVSetChannelUop(x.bits)))
  val simChannelOut: Seq[ValidIO[DecodeChannelOutput]] =
    simUopOuts.map(x => makeValid(x.valid, DecodeChannelOutput.fromSimpleChannelUop(x.bits)))
  val psdChannelOut: Seq[ValidIO[DecodeChannelOutput]] =
    psdUopOuts.map(x => makeValid(x.valid, DecodeChannelOutput.fromPseudoChannelUop(x.bits, VLEN)))

  // should be 0~7
  val uopBufferNumNext = Wire(UInt(log2Up(uopBufferSize).W))
  val uopBufferNum = RegEnable(uopBufferNumNext, 0.U(log2Up(uopBufferSize).W), out.uops.head.valid && in.renameCanAccept || in.redirect)

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
   * connection of [[vectorDecodeChannels]]
   */

  val vecDecodeChannelsIn: Seq[DecodeChannelInput] = vectorDecodeChannels.map(_.in)
  vecDecodeChannelsIn.zipWithIndex.foreach { case (modIn, i) =>
    modIn.rawInst := in.mops(i).bits.info.rawInst
    modIn.vtype := in.mops(i).bits.info.vtype
  }

  val vsetDecodeChannelsIn: Seq[VsetDecoder.In] = vsetDecodeChannels.map(_.in)
  vsetDecodeChannelsIn.zipWithIndex.foreach { case (modIn, i) =>
    modIn.rawInst := in.mops(i).bits.info.rawInst
  }

  val simDecodeChannelsIn: Seq[DecodeChannelInput] = simpleDecodeChannels.map(_.in)
  simDecodeChannelsIn.zipWithIndex.foreach { case (modIn, i) =>
    modIn.rawInst := in.mops(i).bits.info.rawInst
    modIn.vtype := in.mops(i).bits.info.vtype
  }

  val psdDecodeChannelsIn: Seq[PseudoDecodeChannel.In] = pseudoDecodeChannels.map(_.in)
  psdDecodeChannelsIn.zipWithIndex.foreach { case (modIn, i) =>
    modIn.rawInst := in.mops(i).bits.info.rawInst
  }

  /**
   * connection of [[uopBufferCtrlDecoder]]
   */

  uopBufferCtrlDecoder.in.uopBufferNum := uopBufferNum
  uopBufferCtrlDecoder.in.channelUopNum := {
    for (i <- 0 until mopWidth) yield {
      Mux(
        !instValids(i),
        NumUopOH.N0,
        Mux(
          psdChannelOut(i).valid,
          NumUopOH.N1,
          Mux1H(Seq(
            vecChannelOut(i).head.valid -> vecUopNumOHs(i),
            vsetChannelOut(i).valid -> NumUopOH.N1,
            simChannelOut(i).valid -> NumUopOH.N1,
          )),
        ),
      )
    }
  }

  uopBufferNumNext := Mux(in.redirect, 0.U, uopBufferCtrlDecoder.out.uopBufferNum)
  uopBufferUpdateVec := uopBufferCtrlDecoder.out.bufferValids.map(_ && in.renameCanAccept)
  uopBufferValid := uopBufferCtrlDecoder.out.bufferValids

  /**
   * connection of [[uopSelectMod]]
   */

  for (i <- 0 until mopWidth) {
    uopSelectMod.in.uopFromChannel(i)(0) :=
      Mux(
        psdChannelOut(i).valid,
        psdChannelOut(i).bits,
        Mux1H(Seq(
          vecChannelOut(i)(0).valid -> vecChannelOut(i)(0).bits,
          vsetChannelOut(i).valid -> vsetChannelOut(i).bits,
          simChannelOut(i).valid -> simChannelOut(i).bits,
        ))
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
  bufferSelectMod.in.mopAcceptVec := uopBufferCtrlDecoder.out.acceptVec.map(_ && in.renameCanAccept)

  uopBufferNext := bufferSelectMod.out.decodedInfoOut
  bufferedMopCtrlNext := bufferSelectMod.out.mopToBuffer


  for (i <- 0 until mopWidth) {
    in.mops(i).ready := uopBufferCtrlDecoder.out.acceptVec(i) && in.renameCanAccept
  }

  for (i <- 0 until uopWidth) {
    out.uops(i).valid := uopBufferCtrlDecoder.out.uopValids(i)
    out.uops(i).bits.info := uopSelectMod.out.decodedInfoOut(i)
    out.uops(i).bits.ctrl := uopSelectMod.out.bypassInfoOut(i)
  }
}

class DecodeChannelInput extends Bundle {
  val rawInst = UInt(32.W)
  val vtype = VType()
  def sew: UInt = vtype.vsew
  def lmul: UInt = vtype.vlmul
  def ma: Bool = vtype.vma
  def ta: Bool = vtype.vta
}

class DecodeChannelOutput extends Bundle {
  val fuType: UInt = FuType()
  val opcode: UInt = Opcode()
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

  val frm = Frm()
  val frmIll = Bool()

  val vm = Bool()

  val noSpec = Bool()
  val blockBack = Bool()
  val flushPipe = Bool()
  val selImm = ValidIO(DecodeSelImm())
  val imm = UInt(32.W)
  val commitType = CommitType()
  val vdDepElim = VdDepElim()
  val isWritePartVd = Bool()

  val canRobCompress = Bool()

  val numWb = NumWB()
  val uopIdx = UopIdx()
  val isFirstUop = Bool()
  val isLastUop = Bool()
}

object DecodeChannelOutput {
  def fromVecChannelUop(vuop: VecDecodeChannelOutputUop): DecodeChannelOutput = {
    val uop = Wire(new DecodeChannelOutput)

    uop.fuType := vuop.fuType
    uop.opcode := vuop.opcode
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

    uop.vlWen := vuop.renameInfo.uop.vlWen
    uop.vxsatWen := vuop.renameInfo.uop.vxsatWen
    uop.fflagsWen := vuop.fflagsWen
    uop.ldest := vuop.src.dest

    uop.frm := vuop.frm
    uop.frmIll := false.B

    uop.vm := vuop.vm

    uop.noSpec := false.B
    uop.blockBack := false.B
    uop.flushPipe := false.B
    uop.selImm := vuop.selImm
    uop.imm := vuop.imm
    uop.commitType := vuop.commitType
    uop.vdDepElim := vuop.vdDepElim
    uop.isWritePartVd := vuop.isWritePartVd

    uop.canRobCompress := false.B

    uop.numWb := vuop.numWb
    uop.uopIdx := vuop.uopIdx
    uop.isFirstUop := vuop.isFirstUop
    uop.isLastUop := vuop.isLastUop

    uop
  }

  def fromVSetChannelUop(vuop: VsetDecoder.Out): DecodeChannelOutput = {
    val uop = Wire(new DecodeChannelOutput)

    uop.fuType := vuop.fuType
    uop.opcode := vuop.opcode
    uop.isVset := true.B

    uop.src1Ren := vuop.renameInfo.bits.src1Ren
    uop.src1Type := vuop.renameInfo.bits.src1Type
    uop.src2Ren := vuop.renameInfo.bits.src2Ren
    uop.src2Type := vuop.renameInfo.bits.src2Type
    uop.src3Ren := false.B
    uop.src3Type := 0.U.asTypeOf(uop.src3Type)
    uop.lsrc1 := vuop.src.src1
    uop.lsrc2 := vuop.src.src2
    uop.lsrc3 := 0.U
    uop.vlRen := vuop.renameInfo.bits.vlRen
    uop.v0Ren := false.B
    uop.frmRen := false.B
    uop.maskType := DontCare
    uop.intRmRen := false.B
    uop.gpWen := vuop.renameInfo.bits.gpWen
    uop.fpWen := vuop.renameInfo.bits.fpWen
    uop.vpWen := vuop.renameInfo.bits.vpWen

    uop.vlWen := vuop.renameInfo.bits.vlWen
    uop.vxsatWen := vuop.renameInfo.bits.vxsatWen
    uop.fflagsWen := false.B

    uop.ldest := vuop.src.dest

    uop.frm := 0.U.asTypeOf(uop.frm)
    uop.frmIll := false.B

    uop.vm := false.B

    uop.noSpec := false.B
    uop.blockBack := false.B
    uop.flushPipe := vuop.flushPipe
    uop.selImm := vuop.selImm
    uop.imm := vuop.imm
    uop.commitType := CommitType.NORMAL
    uop.vdDepElim := VdDepElim.Always // never used
    uop.isWritePartVd := false.B

    // maybe true in the future
    uop.canRobCompress := false.B

    uop.numWb := NumWB.$1
    uop.uopIdx := 0.U
    uop.isFirstUop := true.B
    uop.isLastUop := true.B

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
    uop.maskType := 0.U.asTypeOf(uop.maskType)
    uop.intRmRen := false.B
    uop.gpWen := suop.gpWen
    uop.fpWen := suop.fpWen
    uop.vpWen := false.B

    uop.vlWen := false.B
    uop.vxsatWen := false.B
    uop.fflagsWen := suop.fflagsWen

    uop.ldest := suop.ldest

    uop.frm := suop.frm
    uop.frmIll := suop.frmIll

    uop.vm := false.B

    uop.noSpec := suop.noSpec
    uop.blockBack := suop.blockBack
    uop.flushPipe := suop.flushPipe
    uop.selImm := suop.selImm
    uop.imm := suop.imm
    uop.commitType := suop.commitType
    uop.vdDepElim := VdDepElim.Always // never used
    uop.isWritePartVd := false.B

    uop.canRobCompress := suop.canRobCompress

    uop.numWb := suop.numWb
    uop.uopIdx := 0.U
    uop.isFirstUop := true.B
    uop.isLastUop := true.B

    uop
  }

  def fromPseudoChannelUop(puop: PseudoDecodeChannel.Out, VLEN: Int): DecodeChannelOutput = {
    val uop = Wire(new DecodeChannelOutput)

    uop.fuType := puop.fuType
    uop.opcode := puop.opcode
    uop.isVset := false.B

    uop.src1Ren := puop.src1RenType.ren
    uop.src1Type := puop.src1RenType.typ
    uop.src2Ren := puop.src2RenType.ren
    uop.src2Type := puop.src2RenType.typ
    uop.src3Ren := false.B
    uop.src3Type.value := DecodeSrcType.NO
    uop.lsrc1 := puop.lsrc1
    uop.lsrc2 := puop.lsrc2
    uop.lsrc3 := 0.U
    uop.vlRen := false.B
    uop.v0Ren := false.B
    uop.frmRen := false.B
    uop.maskType := DontCare
    uop.intRmRen := false.B
    uop.gpWen := puop.gpWen
    uop.fpWen := puop.fpWen
    uop.vpWen := false.B

    uop.vlWen := false.B
    uop.vxsatWen := false.B
    uop.fflagsWen := false.B

    uop.ldest := puop.ldest

    uop.frm := Frm.DYN
    uop.frmIll := false.B

    uop.vm := false.B

    uop.noSpec := puop.noSpec
    uop.blockBack := puop.blockBack
    uop.flushPipe := puop.flushPipe
    uop.selImm := puop.selImm
    uop.imm := Mux1HLookUp(
      puop.selImm.bits,
      Seq(
        DecodeSelImm.CSRRVLENB -> (VLEN / 8).U,
      ),
    )
    uop.commitType := CommitType.NORMAL
    uop.vdDepElim := VdDepElim.Always // never used
    uop.isWritePartVd := false.B
    uop.canRobCompress := puop.canRobCompress

    uop.numWb := NumWB.$1
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
  val ftqOffset        = UInt(FetchBlockInstOffsetWidth.W)
  val isLastInFtqEntry = Bool()
  val vtype            = VType()
  val oldVType         = VType()
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
  println(s"The width of Opcode: ${Opcode.getWidth}")

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
    Zvbb,
    Zvknha,
    XSTrap,
    // Zcb, Zcmop,
    // Zfa, Zfh, ZfaZfh, ZfaF, ZfaD, Zfhmin,
  )

  Verilog.emitVerilog(
    new DecodeChannels(
      mopWidth = 8,
      uopWidth = 8,
      extensions = extensions,
      numM2M4M8Channel = (8, 8, 8),
      postfix = "_" + LocalDate.now().format(DateTimeFormatter.ofPattern("yyyyMMdd")) + "_1"
    )(defaultConfig),
    Array(
      "--full-stacktrace",
      "--target-dir", "build/decoder",
    ),
  )

  println(s"The width of Opcode: ${Opcode.getWidth}")
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
