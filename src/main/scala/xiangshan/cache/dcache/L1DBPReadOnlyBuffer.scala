/***************************************************************************************
 * Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
 *
 * XiangShan is licensed under Mulan PSL v2.
 ***************************************************************************************/

package xiangshan.cache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class L1DBPReadOnlyBufferWrite(implicit p: Parameters) extends DCacheBundle {
  val paddr = UInt(PAddrBits.W)
  val data = Vec(blockRows, UInt(rowBits.W))
}

/** Small fully-associative clean-data cache for confirmed L1DBP bypasses. */
class L1DBPReadOnlyBuffer(entries: Int)(implicit p: Parameters) extends DCacheModule {
  require(entries > 0 && isPow2(entries))

  val io = IO(new Bundle {
    val forward = Flipped(Vec(LoadPipelineWidth, new DCacheForward))
    val inflightMSHR = Input(Vec(cfg.nMissEntries, Valid(UInt(PAddrBits.W))))
    val install = Flipped(ValidIO(new L1DBPReadOnlyBufferWrite))
    val invalidate = Flipped(ValidIO(UInt(PAddrBits.W)))
  })

  val valid = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val tags = Reg(Vec(entries, UInt((PAddrBits - blockOffBits).W)))
  val data = Reg(Vec(entries, Vec(blockRows, UInt(rowBits.W))))
  val fifo = RegInit(0.U(log2Ceil(entries).W))

  def blockTag(paddr: UInt) = paddr(PAddrBits - 1, blockOffBits)
  def wordData(line: Vec[UInt], paddr: UInt): UInt = {
    val words = line.grouped(VLEN / rowBits).map(VecInit(_).asUInt).toSeq
    Mux1H(words.zipWithIndex.map { case (word, i) =>
      (paddr(blockOffBits - 1, log2Up(VLEN / 8)) === i.U) -> word
    })
  }

  val installTag = blockTag(io.install.bits.paddr)
  val installMatchOH = VecInit((0 until entries).map(i => valid(i) && tags(i) === installTag)).asUInt
  val invalidOH = VecInit((0 until entries).map(i => valid(i) && tags(i) === blockTag(io.invalidate.bits))).asUInt
  val installIdx = Mux(installMatchOH.orR, OHToUInt(installMatchOH), fifo)

  when (io.invalidate.valid) {
    for (i <- 0 until entries) {
      when (invalidOH(i)) { valid(i) := false.B }
    }
  }.elsewhen (io.install.valid) {
    valid(installIdx) := true.B
    tags(installIdx) := installTag
    data(installIdx) := io.install.bits.data
    when (!installMatchOH.orR) { fifo := fifo + 1.U }
  }

  io.forward.foreach { forward =>
    val s1ReqValid = RegNext(forward.s0Req.valid, false.B)
    val paddr = forward.s1Req.paddr
    val queryTag = blockTag(paddr)
    val mshrBusy = io.inflightMSHR.map(info => info.valid && blockTag(info.bits) === queryTag).reduce(_ || _)
    val storedHitOH = VecInit((0 until entries).map { i =>
      valid(i) && tags(i) === queryTag && !(io.install.valid && installIdx === i.U)
    }).asUInt
    val installHit = io.install.valid && installTag === queryTag
    val invalidHit = io.invalidate.valid && blockTag(io.invalidate.bits) === queryTag
    val hitOH = Mux(invalidHit, 0.U, Mux(installHit, UIntToOH(installIdx, entries), storedHitOH))
    val s1Hit = s1ReqValid && hitOH.orR && !mshrBusy
    val selectedData = Mux(installHit, wordData(io.install.bits.data, paddr),
      Mux1H(hitOH, data.map(line => wordData(line, paddr))))

    forward.s2Resp.valid := RegNext(s1Hit, false.B)
    forward.s2Resp.bits.matchInvalid := false.B
    forward.s2Resp.bits.forwardData := RegEnable(selectedData.asTypeOf(forward.s2Resp.bits.forwardData), s1Hit)
    forward.s2Resp.bits.forwardMask := VecInit(Seq.fill(VLEN / 8)(RegNext(s1Hit, false.B)))
    forward.s2Resp.bits.denied := false.B
    forward.s2Resp.bits.corrupt := false.B
    forward.s2Resp.bits.l1dbpFinalBypass := false.B
  }
}
