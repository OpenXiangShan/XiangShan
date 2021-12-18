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

package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import chisel3.experimental.chiselName
import xiangshan.cache.mmu.CAMTemplate

trait MicroBTBParams extends HasXSParameter {
  val numWays = 1024
  val tagSize = 20
  // val lowerBitSize = 20
  val untaggedBits = log2Ceil(numWays) + instOffsetBits
}

@chiselName
class MicroBTB(implicit p: Parameters) extends BasePredictor
  with MicroBTBParams with HasPerfEvents
{
  val ubtbAddr = new TableAddr(log2Up(numWays), 1)

  class MicroBTBOutMeta extends XSBundle {
    val hit = Bool()
  }

  class MicroBTBEntry extends FTBEntryWithTag {}

  override val meta_size = WireInit(0.U.asTypeOf(new MicroBTBOutMeta)).getWidth // TODO: ReadResp shouldn't save useless members

  class UBTBBank(val nWays: Int) extends XSModule with BPUUtils {
    val io = IO(new Bundle {
      val read_pc = Flipped(DecoupledIO(UInt(VAddrBits.W))) // TODO: Add ready
      // val read_taken_mask = Input(Vec(numBr, Bool()))
      val read_entry = Output(new MicroBTBEntry)
      val read_hit = Output(Bool())

      val update_valid = Input(Bool())
      val update_write_entry = Input(new MicroBTBEntry)
      val update_pc = Input(UInt(VAddrBits.W))
    })

    val dataMem = Module(new SRAMTemplate(new MicroBTBEntry, set = numWays, way = 1, shouldReset = true, holdRead = true, singlePort = true))

    val read_pc = RegNext(io.read_pc.bits)
    val read_tag = ubtbAddr.getTag(read_pc)(tagSize-1,0)

    dataMem.io.r.req.valid := io.read_pc.valid
    dataMem.io.r.req.bits.setIdx := ubtbAddr.getIdx(io.read_pc.bits)

    io.read_pc.ready := dataMem.io.r.req.ready

    val hit_entry = dataMem.io.r.resp.data(0)
    val hit = hit_entry.entry.valid && hit_entry.tag === read_tag

    io.read_entry := hit_entry
    io.read_hit := hit

    dataMem.io.w.apply(io.update_valid, io.update_write_entry, ubtbAddr.getIdx(io.update_pc), io.update_valid)

  } // uBTBBank

  val ubtbBank = Module(new UBTBBank(numWays))
  val bank = ubtbBank.io
  val read_entry = bank.read_entry
  val outMeta = Wire(new MicroBTBOutMeta)

  XSDebug(p"uBTB entry, read_pc=${Hexadecimal(s0_pc)}\n")

  bank.read_pc.valid := io.s0_fire
  bank.read_pc.bits := s0_pc

  io.s1_ready := bank.read_pc.ready

  io.out.resp := io.in.bits.resp_in(0)
  io.out.resp.s1.pc := s1_pc
  io.out.resp.s1.preds.hit := bank.read_hit
  io.out.resp.s1.ftb_entry := read_entry.entry
  io.out.resp.s1.preds.fromFtbEntry(read_entry.entry, s1_pc)

  outMeta.hit := bank.read_hit
  io.out.s3_meta := RegEnable(RegEnable(outMeta.asUInt, io.s1_fire), io.s2_fire)

  // Update logic
  val update = RegNext(io.update.bits)
  val u_valid = RegNext(io.update.valid)
  val u_pc = update.pc
  val u_taken = update.preds.taken
  val u_br_taken_mask = update.preds.br_taken_mask
  val u_meta = update.meta.asTypeOf(new MicroBTBOutMeta)

  val u_tag = ubtbAddr.getTag(u_pc)

  bank.update_valid := u_valid && u_taken && ((u_meta.hit && !update.old_entry) || !u_meta.hit)
  bank.update_pc := u_pc
  bank.update_write_entry.entry := update.ftb_entry
  bank.update_write_entry.entry.valid := true.B
  bank.update_write_entry.tag := u_tag

  XSDebug("req_v=%b, req_pc=%x, hit=%b\n", io.s1_fire, s1_pc, bank.read_hit)
  XSDebug("target=%x, real_taken_mask=%b, taken_mask=%b, brValids=%b, jmpValid=%b\n",
    io.out.resp.s1.target, io.out.resp.s1.real_slot_taken_mask.asUInt, io.out.resp.s1.preds.br_taken_mask.asUInt, read_entry.entry.brValids.asUInt, read_entry.entry.jmpValid.asUInt)

  XSDebug(u_valid, "[update]Update from ftq\n")
  XSDebug(u_valid, "[update]update_pc=%x, tag=%x\n", u_pc, ubtbAddr.getTag(u_pc))
  XSDebug(u_valid, "[update]taken_mask=%b, brValids=%b, jmpValid=%b\n",
    u_br_taken_mask.asUInt, update.ftb_entry.brValids.asUInt, update.ftb_entry.jmpValid)

  XSPerfAccumulate("ubtb_read_hits", RegNext(io.s1_fire) && bank.read_hit)
  XSPerfAccumulate("ubtb_read_misses", RegNext(io.s1_fire) && !bank.read_hit)

  XSPerfAccumulate("ubtb_commit_hits", u_valid && u_meta.hit)
  XSPerfAccumulate("ubtb_commit_misses", u_valid && !u_meta.hit)

  val perfEvents = Seq(
    ("ubtb_commit_hit       ", u_valid &&  u_meta.hit),
    ("ubtb_commit_miss      ", u_valid && !u_meta.hit),
  )
  generatePerfEvent()
}
