package xiangshan.mem.prefetch

import chisel3._
import chisel3.util._

private[xiangshan] object LldpReplayOp extends ChiselEnum {
  val Add, Shl, Shr, Sar, And, Or, Xor = Value
}

private[xiangshan] class LldpReplayStep extends Bundle {
  val op = LldpReplayOp()
  val immediate = SInt(12.W)
  val word = Bool()
}

private[xiangshan] class LldpReplayDescriptor extends Bundle {
  // 0: dependency only. 1..3: loaded value plus zero to two operations.
  val length = UInt(2.W)
  val steps = Vec(2, new LldpReplayStep)
  val displacement = SInt(12.W)
}

/** Performs LLDP value replay after the producer bytes have been selected from the returned data. */
private[prefetch] class LldpReplay(sourceBits: Int) extends Module {
  val io = IO(new Bundle {
    val input = Flipped(Decoupled(new Bundle {
      val bytes = UInt(64.W)
      val size = UInt(2.W)
      val sext = Bool()
      val floating = Bool()
      val source = UInt(sourceBits.W)
      val descriptor = new LldpReplayDescriptor
    }))
    val output = Decoupled(new Bundle {
      val vaddr = UInt(64.W)
      val source = UInt(sourceBits.W)
    })
  })

  private val idle :: execute :: send :: Nil = Enum(3)
  private val state = RegInit(idle)
  private val value = Reg(UInt(64.W))
  private val descriptor = Reg(new LldpReplayDescriptor)
  private val source = Reg(UInt(sourceBits.W))
  private val stepIndex = Reg(Bool())

  private def extend(word: UInt, bits: Int, signed: Bool): UInt =
    Cat(Fill(64 - bits, signed && word(bits - 1)), word(bits - 1, 0))

  private val loaded = MuxLookup(io.input.bits.size, io.input.bits.bytes)(
    Seq(8, 16, 32).zipWithIndex.map { case (bits, index) =>
      index.U -> extend(io.input.bits.bytes, bits, io.input.bits.sext)
    }
  )

  io.input.ready := state === idle
  io.output.valid := state === send
  io.output.bits.vaddr := value + descriptor.displacement.pad(64).asUInt
  io.output.bits.source := source

  when(io.input.fire && io.input.bits.descriptor.length =/= 0.U && !io.input.bits.floating) {
    value := loaded
    descriptor := io.input.bits.descriptor
    source := io.input.bits.source
    stepIndex := false.B
    state := Mux(io.input.bits.descriptor.length === 1.U, send, execute)
  }

  when(state === execute) {
    val step = descriptor.steps(stepIndex)
    val operand = Mux(step.word, value(31, 0).pad(64), value)
    val signedOperand = Mux(step.word, value(31, 0).asSInt.pad(64), value.asSInt)
    val immediate = step.immediate.pad(64).asUInt
    val shift = Cat(!step.word && immediate(5), immediate(4, 0))
    val result = MuxLookup(step.op.asUInt, operand)(
      Seq(
        LldpReplayOp.Add.asUInt -> (operand + immediate),
        LldpReplayOp.Shl.asUInt -> (operand << shift)(63, 0),
        LldpReplayOp.Shr.asUInt -> (operand >> shift),
        LldpReplayOp.Sar.asUInt -> (signedOperand >> shift).asUInt,
        LldpReplayOp.And.asUInt -> (operand & immediate),
        LldpReplayOp.Or.asUInt -> (operand | immediate),
        LldpReplayOp.Xor.asUInt -> (operand ^ immediate)
      )
    )
    value := Mux(step.word, extend(result, 32, true.B), result)
    when(stepIndex || descriptor.length === 2.U) {
      state := send
    }
    stepIndex := true.B
  }

  when(io.output.fire) {
    state := idle
  }
}
