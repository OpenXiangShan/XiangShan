package xiangshan.backend.decode.opcode

import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.vector.util.BString.BinaryStringHelper

trait DataType {
  protected val F = bb"0"
  protected val V = bb"1"

  /**
   * The H data type encoding is b10 in instruction.
   * We convert its encoding as b01 which mean fp16
   */
  protected val FP16 = bb"01"

  /**
   * The S data type encoding is b00 in instruction.
   * We convert its encoding as b10 which mean fp32
   */
  protected val FP32 = bb"10"

  /**
   * The D data type encoding is b01 in instruction.
   * We convert its encoding as b11 which mean fp64
   */
  protected val FP64 = bb"11"

  // ATTENTION!!!
  // New floating point DataType such as FP128 and BF16 will cause breaking modification in this class.

  protected val I8  = bb"00"
  protected val I16 = bb"01"
  protected val I32 = bb"10"
  protected val I64 = bb"11"

  protected val E8  = bb"00"
  protected val E16 = bb"01"
  protected val E32 = bb"10"
  protected val E64 = bb"11"
  protected val EX  = bb"00"
}
