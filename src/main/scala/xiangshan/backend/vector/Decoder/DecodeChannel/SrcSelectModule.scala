package xiangshan.backend.vector.Decoder.DecodeChannel

import chisel3.util.Mux1H
import chisel3._
import xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel.{DestSelectEnum, Src1SelectEnum, Src2SelectEnum}
import xiangshan.backend.vector.HasVectorSettings
import xiangshan.backend.vector.util.Select.Mux1HLookUp
import xiangshan.backend.vector.util.Verilog

class SrcSelectModule extends Module with HasVectorSettings {
  val in = IO(Input(new Bundle {
    // 1,2,4,8,
    val uopNum = UInt(4.W)
    val src1Sel = Src1SelectEnum.UInt()
    val src2Sel = Src2SelectEnum.UInt()
    val destSel = DestSelectEnum.UInt()
    val rs1 = UInt(5.W)
    val rs2 = UInt(5.W)
    val rd = UInt(5.W)
  }))

  val out = IO(Output(new Bundle {
    val src = Vec(maxSplitUopNum, new UopSrcBundle)
  }))

  val uopNum = in.uopNum
  val rs1 = in.rs1
  val rs2 = in.rs2
  val rd = in.rd

  // uopNum - 1
  val uopNumM1 = Mux1H(
    uopNum, Seq(
      0.U, 1.U, 3.U, 7.U,
    )
  )

  val uopNumD2M1 = Mux1H(
    uopNum, Seq(
      0.U, 0.U, 1.U, 3.U,
    )
  )

  for (i <- out.src.indices) {
    out.src(i).src1 := Mux1HLookUp(
      in.src1Sel,
      Seq(
        Src1SelectEnum.NONE -> 0.U,
        Src1SelectEnum.INC1 -> (rs1 + i.U),
        Src1SelectEnum.INCF2 -> (rs1 + (i / 2).U),
        Src1SelectEnum.INCF4 -> (rs1 + (i / 4).U),
        Src1SelectEnum.CONST -> rs1,
        Src1SelectEnum.S1MINx1_DCONST -> (
          if (i == 0) {
            rs1
          } else {
            rd
          }),
        Src1SelectEnum.S2MAXx1_DCONST -> (
          if (i == 0) {
            rs2 | uopNumM1
          } else {
            rd
          }),
        Src1SelectEnum.S2MAXF2x1_DCONST -> (
          if (i == 0) {
            rs2 | uopNumD2M1
          } else {
            rd
          })
      ).map { case (k, v) => k.toUInt -> v }
    )

    out.src(i).src2 := Mux1HLookUp(
      in.src2Sel,
      Seq(
        Src2SelectEnum.NONE -> 0.U,
        Src2SelectEnum.INC1 -> (rs2 + i.U),
        Src2SelectEnum.INCF2 -> (rs2 + (i / 2).U),
        Src2SelectEnum.INCF4 -> (rs2 + (i / 4).U),
        Src2SelectEnum.CONST -> rs2,
        Src2SelectEnum.INC1x7_S1 -> Mux1H(Seq(
          ((i == 0).B && uopNum(0)) -> rs1,
          ((i == 1).B && uopNum(1)) -> rs1,
          ((i == 3).B && uopNum(2)) -> rs1,
          ((i == 7).B && uopNum(3)) -> rs1,
          ((i != 1).B && uopNum(1)) -> (rs2 | i.U),
          ((i != 3).B && uopNum(2)) -> (rs2 | i.U),
          ((i != 7).B && uopNum(3)) -> (rs2 | i.U),
        )),
        Src2SelectEnum.INCF2x7_S1 -> Mux1H(Seq(
          ((i == 0).B && uopNum(0)) -> rs1,
          ((i == 1).B && uopNum(1)) -> rs1,
          ((i == 3).B && uopNum(2)) -> rs1,
          ((i == 7).B && uopNum(3)) -> rs1,
          ((i != 1).B && uopNum(1)) -> (rs2 | (i / 2).U),
          ((i != 3).B && uopNum(2)) -> (rs2 | (i / 2).U),
          ((i != 7).B && uopNum(3)) -> (rs2 | (i / 2).U),
        ))
      ).map { case (k, v) => k.toUInt -> v }
    )

    out.src(i).dest := Mux1HLookUp(
      in.destSel,
      Seq(
        DestSelectEnum.NONE -> 0.U,
        DestSelectEnum.INC1 -> (rd + i.U),
        DestSelectEnum.INCF2 -> (rd + (i / 2).U),
        DestSelectEnum.INCF4 -> (rd + (i / 4).U),
        DestSelectEnum.CONST -> rd,
      ).map { case (k, v) => k.toUInt -> v }
    )
  }
}

object SrcSelectModule {
  def main(args: Array[String]): Unit = {
    Verilog.emitVerilog(
      new SrcSelectModule,
      Array(
        "--throw-on-first-error",
        "--full-stacktrace",
        "--target-dir", "build/decoder"
      ),
    )
  }
}