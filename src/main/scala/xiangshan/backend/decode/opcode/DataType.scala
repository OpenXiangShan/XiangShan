package xiangshan.backend.decode.opcode

import xiangshan.backend.vector.util.BString.BinaryStringHelper

trait DataType {
  private[opcode] val F = b"0"
  private[opcode] val V = b"1"

  /**
   * The H data type encoding is b10 in instruction.
   * We convert its encoding as b01 which mean fp16
   */
  private[opcode] val FP16 = b"01"

  /**
   * The S data type encoding is b00 in instruction.
   * We convert its encoding as b10 which mean fp32
   */
  private[opcode] val FP32 = b"10"

  /**
   * The D data type encoding is b01 in instruction.
   * We convert its encoding as b11 which mean fp64
   */
  private[opcode] val FP64 = b"11"

  // ATTENTION!!!
  // New floating point DataType such as FP128 and BF16 will cause breaking modification in this class.

  private[opcode] val I8  = b"00"
  private[opcode] val I16 = b"01"
  private[opcode] val I32 = b"10"
  private[opcode] val I64 = b"11"
}
