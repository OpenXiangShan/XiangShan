// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.ifu

import chisel3._
import freechips.rocketchip.rocket.ExpandedInstruction
import freechips.rocketchip.rocket.Instructions.{C_SSPOPCHK, C_SSPUSH}
import freechips.rocketchip.rocket.RVCDecoder
import org.chipsalliance.cde.config.Parameters
import chisel3.util.Cat

class RvcExpander(implicit p: Parameters) extends IfuModule {
  class RVCExpanderIO(implicit p: Parameters) extends IfuBundle {
    val in:      UInt                = Input(UInt(32.W))
    val fsIsOff: Bool                = Input(Bool())
    val out:     ExpandedInstruction = Output(new ExpandedInstruction)
    val ill:     Bool                = Output(Bool())
  }
  val io: RVCExpanderIO = IO(new RVCExpanderIO)

  private val decoder = new RVCDecoder(io.in, io.fsIsOff, XLEN, fLen, useAddiForMv = true)
  // Zicfiss: C.SSPUSH/C.SSPOPCHK are Zcmop encodings redefined by Zicfiss.
  private val isCSSPUSH = C_SSPUSH === io.in
  private val isCSSPOPCHK = C_SSPOPCHK === io.in

  // Zicfiss: expand compressed shadow-stack instructions to their 32-bit MOP forms.
  private def zicfissExpandedInst(bits: UInt, rd: UInt, rs1: UInt, rs2: UInt): ExpandedInstruction = {
    val expanded = Wire(new ExpandedInstruction)
    expanded.bits := bits
    expanded.rd := rd
    expanded.rs1 := rs1
    expanded.rs2 := rs2
    expanded.rs3 := 0.U
    expanded
  }

  // Zicfiss: C.SSPUSH expands to SSPUSH x1.
  private val zicfissSSPUSH = zicfissExpandedInst(
    Cat("b1100111".U(7.W), 1.U(5.W), 0.U(5.W), "b100".U(3.W), 0.U(5.W), "b1110011".U(7.W)),
    rd = 0.U(5.W),
    rs1 = 0.U(5.W),
    rs2 = 1.U(5.W),
  )

  // Zicfiss: C.SSPOPCHK expands to SSPOPCHK x5.
  private val zicfissSSPOPCHK = zicfissExpandedInst(
    Cat("b110011011100".U(12.W), 5.U(5.W), "b100".U(3.W), 0.U(5.W), "b1110011".U(7.W)),
    rd = 0.U(5.W),
    rs1 = 5.U(5.W),
    rs2 = 0.U(5.W),
  )

  if (HasCExtension) {
    io.out := (if (HasShadowStack) Mux(isCSSPUSH, zicfissSSPUSH, Mux(isCSSPOPCHK, zicfissSSPOPCHK, decoder.decode))
      else decoder.decode)
    io.ill := decoder.ill && (if (HasShadowStack) !(isCSSPUSH || isCSSPOPCHK) else true.B)
  } else {
    io.out := decoder.passthrough
    io.ill := false.B
  }
}
