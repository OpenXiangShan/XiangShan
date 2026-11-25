package xiangshan.backend.rename

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.Bundles.RenameOutUop
import xiangshan.backend.fu.FuType
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.LqPtr
import xiangshan.mem.prefetch.{LldpLoadInfo, LldpReplayOp, LldpReplayStep}

private[rename] class LldpRename(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val redirect = Flipped(Valid(new Redirect))
    val input = Flipped(Vec(RenameWidth, Valid(new Bundle {
      val uop = new RenameOutUop
      val pc = UInt(VAddrBits.W)
    })))
    val load = Vec(RenameWidth, Valid(new LldpLoadInfo))
  })

  private class Definition extends Bundle {
    val valid = Bool()
    val pc = UInt(VAddrBits.W)
    val robIdx = new RobPtr
    val lqIdx = new LqPtr
    val writer = new RobPtr
    val length = UInt(2.W)
    val steps = Vec(2, new LldpReplayStep)
  }

  private val int = RegInit(VecInit.fill(IntPhyRegs)(0.U.asTypeOf(new Definition)))
  private val fp = RegInit(VecInit.fill(FpPhyRegs)(0.U.asTypeOf(new Definition)))
  private val writes = Wire(Vec(RenameWidth, new Definition))
  private val writeInt = Wire(Vec(RenameWidth, Bool()))
  private val writeFp = Wire(Vec(RenameWidth, Bool()))

  for (entry <- int ++ fp) {
    when(entry.valid && entry.writer.needFlush(io.redirect)) {
      entry.valid := false.B
    }
  }

  private val supported = Seq(
    ALUOpType.add -> LldpReplayOp.Add,
    ALUOpType.addw -> LldpReplayOp.Add,
    ALUOpType.sll -> LldpReplayOp.Shl,
    ALUOpType.sllw -> LldpReplayOp.Shl,
    ALUOpType.srl -> LldpReplayOp.Shr,
    ALUOpType.srlw -> LldpReplayOp.Shr,
    ALUOpType.sra -> LldpReplayOp.Sar,
    ALUOpType.sraw -> LldpReplayOp.Sar,
    ALUOpType.and -> LldpReplayOp.And,
    ALUOpType.or -> LldpReplayOp.Or,
    ALUOpType.xor -> LldpReplayOp.Xor
  )
  private val wordOps = Seq(ALUOpType.addw, ALUOpType.sllw, ALUOpType.srlw, ALUOpType.sraw)

  for (i <- io.input.indices) {
    val in = io.input(i)
    val uop = in.bits.uop
    val info = io.load(i)
    val scalarLoad = FuType.isLoad(uop.fuType) && !LSUOpType.isPrefetch(uop.fuOpType) &&
      !LSUOpType.isHlv(uop.fuOpType) && LSUOpType.size(uop.fuOpType) <= 3.U
    val scalar = !uop.vecWen && !uop.vlsInstr && !uop.hasException

    val sources = (0 until 2).map { src =>
      val isInt = SrcType.isXp(uop.srcType(src)) && uop.psrc(src).orR
      val isFp = SrcType.isFp(uop.srcType(src))
      val intIndex = uop.psrc(src)(IntPhyRegIdxWidth - 1, 0)
      val fpIndex = uop.psrc(src)(FpPhyRegIdxWidth - 1, 0)
      val latest = (0 until i).foldLeft(Mux(isFp, fp(fpIndex), int(intIndex))) { (definition, previous) =>
        val prev = io.input(previous).bits.uop
        val bypass = ((isInt && writeInt(previous)) || (isFp && writeFp(previous))) &&
          prev.pdest === uop.psrc(src)
        Mux(bypass, writes(previous), definition)
      }
      val result = Wire(new Definition)
      result := latest
      result.valid := latest.valid && (isInt || isFp)
      result
    }
    val base = sources.head
    val source = Mux(base.valid, base, sources(1))
    val exactOps = supported.map { case (op, replayOp) => (uop.fuOpType === op.encode) -> replayOp }
    val singleImmediate = SrcType.isXp(uop.srcType.head) && SrcType.isNotReg(uop.srcType(1)) &&
      uop.selImm === SelImm.IMM_I && uop.fuType === FuType.alu.U && exactOps.map(_._1).reduce(_ || _)
    val extendable = singleImmediate && base.length =/= 0.U && base.length < 3.U

    val definition = writes(i)
    definition := source
    definition.valid := scalar && (scalarLoad || source.valid)
    definition.writer := uop.robIdx
    definition.length := Mux(extendable, source.length + 1.U, 0.U)
    for (step <- 0 until 2) {
      when(extendable && source.length === (step + 1).U) {
        definition.steps(step).op := MuxCase(LldpReplayOp.Add, exactOps)
        definition.steps(step).immediate := uop.imm(11, 0).asSInt
        definition.steps(step).word := wordOps.map(op => uop.fuOpType === op.encode).reduce(_ || _)
      }
    }
    when(scalarLoad) {
      definition.pc := in.bits.pc
      definition.robIdx := uop.robIdx
      definition.lqIdx := uop.lsqIdxStart.lqIdx
      definition.length := Mux(uop.fpWen, 0.U, 1.U)
      definition.steps := 0.U.asTypeOf(definition.steps)
    }
    writeInt(i) := in.valid && !io.redirect.valid && uop.rfWen && uop.pdest.orR && !uop.isMove
    writeFp(i) := in.valid && !io.redirect.valid && uop.fpWen
    when(writeInt(i)) {
      int(uop.pdest(IntPhyRegIdxWidth - 1, 0)) := definition
    }
    when(writeFp(i)) {
      fp(uop.pdest(FpPhyRegIdxWidth - 1, 0)) := definition
    }

    info.valid := scalarLoad && scalar
    info.bits.pc := in.bits.pc
    info.bits.dataType.size := LSUOpType.size(uop.fuOpType)(1, 0)
    info.bits.dataType.sext := !LSUOpType.getSignSize(uop.fuOpType)(3)
    info.bits.dataType.floating := uop.fpWen
    info.bits.producer.valid := base.valid
    info.bits.producer.bits.producer := base.pc
    info.bits.producer.bits.consumer := in.bits.pc
    info.bits.producer.bits.consumerType := info.bits.dataType
    info.bits.producer.bits.descriptor.length := base.length
    info.bits.producer.bits.descriptor.steps := base.steps
    info.bits.producer.bits.descriptor.displacement := uop.imm(11, 0).asSInt
    info.bits.producerRobIdx := base.robIdx
    info.bits.producerLqIdx := base.lqIdx
  }
}
