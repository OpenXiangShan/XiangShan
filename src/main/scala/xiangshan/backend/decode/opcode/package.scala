package xiangshan.backend.decode

import chisel3._
import chisel3.util.log2Up

package object opcode {
  val OpcodeTraits = yunsuan.encoding.Opcode.OpcodeTraits

  object Latency {
    // Reserve an encoding beyond the largest fixed latency for uncertain ops.
    lazy val width: Int = yunsuan.encoding.Opcode.Latency.width.max(log2Up(Opcode.FDivOpcodes.FixedLatency + 2))

    def apply(): UInt = UInt(width.W)
    def uncertainLitVal(): Int = yunsuan.encoding.Opcode.Latency.uncertainLitVal()
  }
}
