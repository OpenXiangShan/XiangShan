package xiangshan.backend.fu.fpu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

class FliTable(table: Seq[Int]) extends Module {
  val src = IO(Input(UInt(5.W)))
  val out = IO(Output(UInt(16.W)))

  out := chisel3.util.experimental.decode.decoder(src,
    TruthTable(table.zipWithIndex.map { case(data, in) =>
      (BitPat(in.U(5.W)), BitPat(data.U(16.W)))
    },
      BitPat.N(16)
    )
  )
}

class FliHTable extends FliTable(
  Seq(
    0xBC00, // -1.0
    0x0400, // minimum positive normal
    0x0100, // 1.0 * 2^-16
    0x0200, // 1.0 * 2^-15
    0x1C00, // 1.0 * 2^-8
    0x2000, // 1.0 * 2^-7
    0x2C00, // 1.0 * 2^-4
    0x3000, // 1.0 * 2^-3
    0x3400, // 0.25
    0x3500, // 0.3125
    0x3600, // 0.375
    0x3700, // 0.4375
    0x3800, // 0.5
    0x3900, // 0.625
    0x3A00, // 0.75
    0x3B00, // 0.875
    0x3C00, // 1.0
    0x3D00, // 1.25
    0x3E00, // 1.5
    0x3F00, // 1.75
    0x4000, // 2.0
    0x4100, // 2.5
    0x4200, // 3
    0x4400, // 4
    0x4800, // 8
    0x4C00, // 16
    0x5800, // 2^7
    0x5C00, // 2^8
    0x7800, // 2^15
    0x7C00, // +inf(2^16 is not expressible)
    0x7C00, // +inf
    0x7E00  // CNaN
  )
)

class FliSTable extends FliTable(
  Seq(
    0xBF80, // -1.0
    0x0080, // minimum positive normal
    0x3780, // 1.0 * 2^-16
    0x3800, // 1.0 * 2^-15
    0x3B80, // 1.0 * 2^-8
    0x3C00, // 1.0 * 2^-7
    0x3D80, // 1.0 * 2^-4
    0x3E00, // 1.0 * 2^-3
    0x3E80, // 0.25
    0x3EA0, // 0.3125
    0x3EC0, // 0.375
    0x3EE0, // 0.4375
    0x3F00, // 0.5
    0x3F20, // 0.625
    0x3F40, // 0.75
    0x3F60, // 0.875
    0x3F80, // 1.0
    0x3FA0, // 1.25
    0x3FC0, // 1.5
    0x3FE0, // 1.75
    0x4000, // 2.0
    0x4020, // 2.5
    0x4040, // 3
    0x4080, // 4
    0x4100, // 8
    0x4180, // 16
    0x4300, // 2^7
    0x4380, // 2^8
    0x4700, // 2^15
    0x4780, // 2^16
    0x7F80, // +inf
    0x7FC0  // CNaN
  )
)

class FliDTable extends FliTable(
  Seq(
    0xBFF0, // -1.0
    0x0010, // minimum positive normal
    0x3EF0, // 1.0 * 2^-16
    0x3F00, // 1.0 * 2^-15
    0x3F70, // 1.0 * 2^-8
    0x3F80, // 1.0 * 2^-7
    0x3FB0, // 1.0 * 2^-4
    0x3FC0, // 1.0 * 2^-3
    0x3FD0, // 0.25
    0x3FD4, // 0.3125
    0x3FD8, // 0.375
    0x3FDC, // 0.4375
    0x3FE0, // 0.5
    0x3FE4, // 0.625
    0x3FE8, // 0.75
    0x3FEC, // 0.875
    0x3FF0, // 1.0
    0x3FF4, // 1.25
    0x3FF8, // 1.5
    0x3FFC, // 1.75
    0x4000, // 2.0
    0x4004, // 2.5
    0x4008, // 3
    0x4010, // 4
    0x4020, // 8
    0x4030, // 16
    0x4060, // 2^7
    0x4070, // 2^8
    0x40E0, // 2^15
    0x40F0, // 2^16
    0x7FF0, // +inf
    0x7FF8  // CNaN
  )
)