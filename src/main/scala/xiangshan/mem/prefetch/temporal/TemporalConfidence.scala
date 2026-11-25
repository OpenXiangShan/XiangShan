package xiangshan.mem.prefetch.temporal

import chisel3._
import chisel3.util._

private[prefetch] object TemporalConfOp extends ChiselEnum {
  val Lookup, Touch, Mark, Issued, Feedback = Value
}

private[prefetch] class TemporalConfRequest(contextBits: Int) extends Bundle {
  val op = TemporalConfOp()
  val pc = UInt(TemporalAddress.PcBits.W)
  val context = UInt(contextBits.W)
  val roles = UInt(2.W)
  val issued = UInt(log2Ceil(TemporalAddress.RecordLength + 1).W)
  val legacyUseful = Bool()
  val legacyTimely = Bool()
  val sent = Bool()
  val useful = Bool()
  val timely = Bool()
}

private[prefetch] class TemporalConfidence(ways: Int, contextBits: Int) extends Module {
  import TemporalAddress._
  import TemporalConfOp._

  private val confSets = 64
  private val confIndexBits = log2Ceil(confSets)
  private val roleSets = 256
  private val roleIndexBits = log2Ceil(roleSets)

  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new TemporalConfRequest(contextBits)))
    val response = Decoupled(new Bundle {
      val roles = UInt(2.W)
      val issue = UInt(6.W)
      val meta = UInt(6.W)
      val canBypassSampler = Bool()
    })
  })

  private class Key extends Bundle {
    val pcTag = UInt((PcBits - confIndexBits).W)
    val context = UInt(contextBits.W)
  }
  private class Confidence extends Bundle {
    val key = new Key
    val issue = UInt(6.W)
    val meta = UInt(6.W)
    val dependent = Bool()
    val usefulness = UInt(6.W)
    val timeliness = UInt(6.W)
    val sentEvidence = UInt(4.W)
  }
  private class RoleEntry extends Bundle {
    val pcTag = UInt((PcBits - roleIndexBits).W)
    val context = UInt(contextBits.W)
    val roles = UInt(2.W)
  }

  private val table = Module(new TemporalTrainingTable(
    confSets, ways, new Confidence, new Key, (entry: Confidence, key: Key) =>
      entry.key.pcTag === key.pcTag && entry.key.context === key.context
  ))
  private val roles = RegInit(VecInit(Seq.fill(roleSets)(0.U.asTypeOf(new RoleEntry))))
  private val idle :: update :: send :: Nil = Enum(3)
  private val state = RegInit(idle)
  private val request = RegEnable(io.request.bits, io.request.fire)
  private val response = Reg(chiselTypeOf(io.response.bits))
  private val roleSet = request.pc(roleIndexBits - 1, 0)
  private val roleTag = request.pc(PcBits - 1, roleIndexBits)
  private val roleMatch = roles(roleSet).pcTag === roleTag && roles(roleSet).context === request.context
  private val priorRoles = Mux(roleMatch, roles(roleSet).roles, 0.U)
  private val currentRoles = priorRoles | Mux(request.op === Mark, request.roles, 0.U)

  private def decrease(value: UInt, amount: UInt): UInt = Mux(value >= amount, value - amount, 0.U)
  private def increase(value: UInt, amount: UInt): UInt = {
    val sum = value +& amount
    Mux(sum(value.getWidth), Fill(value.getWidth, true.B), sum(value.getWidth - 1, 0))
  }

  io.request.ready := state === idle
  io.response.valid := state === send
  io.response.bits := response
  table.io.read.valid := io.request.fire
  table.io.read.bits.set := placementPC(io.request.bits.pc)(confIndexBits - 1, 0)
  table.io.read.bits.key.pcTag := io.request.bits.pc(PcBits - 1, confIndexBits)
  table.io.read.bits.key.context := io.request.bits.context
  table.io.touch := state === update && request.op =/= Lookup
  table.io.write.valid := table.io.touch && (request.op =/= Touch || !table.io.hit)

  private val default = Wire(new Confidence)
  default.key.pcTag := request.pc(PcBits - 1, confIndexBits)
  default.key.context := request.context
  default.issue := 32.U
  default.meta := 32.U
  default.dependent := currentRoles.orR
  default.usefulness := 32.U
  default.timeliness := 32.U
  default.sentEvidence := 0.U
  private val current = Mux(table.io.hit, table.io.selectedEntry, default)
  private val next = WireInit(current)
  table.io.write.bits := next

  switch(request.op) {
    is(Mark) {
      next.dependent := current.dependent || request.roles.orR
    }
    is(Issued) {
      next.issue := decrease(current.issue, (request.issued +& 3.U) >> 2)
      next.meta := decrease(current.meta, (2.U * request.issued +& 4.U) / 5.U)
    }
    is(Feedback) {
      next.issue := increase(current.issue, request.legacyUseful)
      next.meta := increase(current.meta, request.legacyTimely)
      next.sentEvidence := increase(current.sentEvidence, request.sent)
      next.usefulness := increase(decrease(current.usefulness, request.sent), Mux(request.useful, 2.U, 0.U))
      next.timeliness := increase(decrease(current.timeliness, request.sent), Mux(request.timely, 2.U, 0.U))
    }
  }

  when(io.request.fire) {
    state := update
  }
  when(state === update) {
    when(request.op === Mark) {
      assert(request.roles.orR)
      roles(roleSet).roles := currentRoles
      roles(roleSet).pcTag := roleTag
      roles(roleSet).context := request.context
    }
    response.roles := currentRoles
    response.issue := current.issue
    response.meta := current.meta
    response.canBypassSampler := current.sentEvidence >= 12.U &&
      current.usefulness >= 56.U && current.timeliness >= 40.U
    state := send
  }
  when(io.response.fire) {
    state := idle
  }
}
