/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.decode

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility._
import utils._
import xiangshan._
import xiangshan.backend.rename.RatReadPort
import xiangshan.backend.Bundles._

class DecodeStage(implicit p: Parameters) extends XSModule with HasPerfEvents {
  val io = IO(new Bundle() {
    // from Ibuffer
    val in = Vec(DecodeWidth, Flipped(DecoupledIO(new StaticInst)))
    // to Rename
    val out = Vec(DecodeWidth, DecoupledIO(new DecodedInst))
    // RAT read
    val intRat = Vec(RenameWidth, Vec(3, Flipped(new RatReadPort))) // Todo: make it configurable
    val fpRat = Vec(RenameWidth, Vec(4, Flipped(new RatReadPort)))
    val vecRat = Vec(RenameWidth, Vec(5, Flipped(new RatReadPort)))
    // csr control
    val csrCtrl = Input(new CustomCSRCtrlIO)
    val fusion = Vec(DecodeWidth - 1, Input(Bool()))
  })

  val decoders = Seq.fill(DecodeWidth)(Module(new DecodeUnit))

  private val v0Idx = 0
  private val vconfigIdx = 32


  for (i <- 0 until DecodeWidth) {
    decoders(i).io.enq.ctrlFlow <> io.in(i).bits

    // csr control
    decoders(i).io.csrCtrl := io.csrCtrl

    // We use the lsrc/ldest before fusion decoder to read RAT for better timing.
    io.intRat(i)(0).addr := io.out(i).bits.lsrc(0)
    io.intRat(i)(1).addr := io.out(i).bits.lsrc(1)
    io.intRat(i)(2).addr := io.out(i).bits.ldest
    io.intRat(i).foreach(_.hold := !io.out(i).ready)

    // Floating-point instructions can not be fused now.
    io.fpRat(i)(0).addr := io.out(i).bits.lsrc(0)
    io.fpRat(i)(1).addr := io.out(i).bits.lsrc(1)
    io.fpRat(i)(2).addr := io.out(i).bits.lsrc(2)
    io.fpRat(i)(3).addr := io.out(i).bits.ldest
    io.fpRat(i).foreach(_.hold := !io.out(i).ready)

    // Vec instructions
    // TODO: vec uop dividers need change this
    io.vecRat(i)(0).addr := decoders(i).io.deq.decodedInsts.lsrc(0) // vs1
    io.vecRat(i)(1).addr := decoders(i).io.deq.decodedInsts.lsrc(1) // vs2
    io.vecRat(i)(2).addr := decoders(i).io.deq.decodedInsts.ldest   // old_vd
    io.vecRat(i)(3).addr := v0Idx.U                                 // v0
    io.vecRat(i)(4).addr := vconfigIdx.U                            // vtype
    io.vecRat(i).foreach(_.hold := !io.out(i).ready)
  }

  io.out.zip(decoders.map(_.io.deq)).foreach { case (out, decodeOut) =>
    out.bits := decodeOut.decodedInsts
  }
  io.out.zip(io.in).foreach { case (out, in) =>
    out.valid := in.valid
    in.ready := out.ready
  }

  val hasValid = VecInit(io.in.map(_.valid)).asUInt.orR
  XSPerfAccumulate("utilization", PopCount(io.in.map(_.valid)))
  XSPerfAccumulate("waitInstr", PopCount((0 until DecodeWidth).map(i => io.in(i).valid && !io.in(i).ready)))
  XSPerfAccumulate("stall_cycle", hasValid && !io.out(0).ready)

  if (env.EnableTopDown) {
    XSPerfAccumulate("slots_issued", PopCount(io.out.map(_.fire)))
    XSPerfAccumulate("decode_bubbles", PopCount(io.out.map(x => !x.valid && x.ready))) // Unutilized issue-pipeline slots while there is no backend-stall
    XSPerfAccumulate("fetch_bubbles", PopCount((0 until DecodeWidth).map(i => !io.in(i).valid && io.in(i).ready))) //slots
    XSPerfAccumulate("ifu2id_allNO_cycle", VecInit((0 until DecodeWidth).map(i => !io.in(i).valid && io.in(i).ready)).asUInt.andR)
  }

  val fusionValid = RegNext(io.fusion)
  val inFire = io.in.map(in => RegNext(in.valid && !in.ready))
  val perfEvents = Seq(
    ("decoder_fused_instr", PopCount(fusionValid)       ),
    ("decoder_waitInstr",   PopCount(inFire)            ),
    ("decoder_stall_cycle", hasValid && !io.out(0).ready),
    ("decoder_utilization", PopCount(io.in.map(_.valid))),
  )
  generatePerfEvent()
}
