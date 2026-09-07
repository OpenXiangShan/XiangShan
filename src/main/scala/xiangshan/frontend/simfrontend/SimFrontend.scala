/***************************************************************************************
 * Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
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
 *
 *
 * Acknowledgement
 *
 * This implementation is inspired by several key papers:
 * [1] Robert. M. Tomasulo. "[An efficient algorithm for exploiting multiple arithmetic units.]
 * (https://doi.org/10.1147/rd.111.0025)" IBM Journal of Research and Development (IBMJ) 11.1: 25-33. 1967.
 ***************************************************************************************/

package xiangshan.frontend.simfrontend

import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import utility._
import utility.PerfCCT
import xiangshan._
import xiangshan.frontend._
import xiangshan.frontend.ifu._
import xiangshan.frontend.ibuffer.{IBufEntry, IBufExceptionEntry, IBufOutEntry, PredInstAccept, PredUopNum}
import xiangshan.backend.decode.VTypeGen

/**
 * Sim Frontend/Ideal Frontend
 *
 * Used for backend performance evaluation in Verilog simulation for decoupling.
 * Currently, this implementation cannot handle certain architecture-dependent scenarios, such as:
 * Dynamic execution of lr/sc instructions yielding different results,
 * Branching based on mcycle and mtime yielding different results, etc.
 *
 *
 * Under ideal conditions, each cycle will fully utilize the instruction bandwidth to transmit 8 instructions.
 * However, in most cases, when the backend executes/submits commands slowly, it causes a false ftq full condition.
 * This prevents sending all 8 commands within a cycle, limiting transmission to only the commands stored in the false ftq.
 */
class SimFrontFetchHelper extends ExtModule() with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val io = IO(new Bundle {
    // Shouldn't parameters be used?
    // But it seems that verilog is not convenient?
    val out = Vec(
      8,
      new Bundle {
        val pc        = Output(UInt(64.W))
        val instr     = Output(UInt(32.W))
        val preDecode = Output(UInt(32.W))
      }
    )

    val out_newestPc        = Output(UInt(64.W))
    val out_newestPreDecode = Output(UInt(32.W))

    val out_ftqPackData = Output(UInt(32.W))
    val out_ftqPc       = Output(UInt(64.W))

    val updatePtrCount = Input(UInt(32.W))
    val fetchOffset    = Input(UInt(4.W))
    val fetchCount     = Input(UInt(4.W))

    val robCommitValid    = Input(Bool())
    val robCommitFtqFlag  = Input(UInt(1.W))
    val robCommitFtqValue = Input(UInt(6.W))

    val redirect         = Input(Bool())
    val redirectFtqFlag  = Input(UInt(1.W))
    val redirectFtqValue = Input(UInt(6.W))
    val redirectType     = Input(UInt(32.W))
    val redirectPc       = Input(UInt(64.W))
    val redirectTarget   = Input(UInt(64.W))
  })

  val verilogLines = Seq(
    "import \"DPI-C\" function void SimFrontFetch (",
    "  input int offset,",
    "  output longint pc,",
    "  output int inst,",
    "  output int preDecode,",
    ");",
    "",
    "import \"DPI-C\" function void SimFrontUpdatePtr (",
    "  input int updateCount,",
    ");",
    "",
    "import \"DPI-C\" function void SimFrontRedirect (",
    "  input int redirect_valid,",
    "  input int redirect_ftq_flag,",
    "  input int redirect_ftq_value,",
    "  input int redirect_type,",
    "  input longint redirect_pc,",
    "  input longint redirect_target,",
    ");",
    "",
    "import \"DPI-C\" function void SimFrontGetFtqToBackEnd (",
    "  output longint pc,",
    "  output int pack_data,",
    "  output longint newest_pc,",
    "  output int newest_pack_data,",
    ");",
    "",
    "import \"DPI-C\" function void SimFrontRobCommit (",
    "  input int valid,",
    "  input int ftqIdxValue,",
    "  input int crossFtqIdxType,",
    ");",
    "",
    "module SimFrontFetchHelper(",
    "  input         clock,",
    "  input         reset,",
    "",
    "  output logic [63:0] io_out_0_pc,",
    "  output logic [31:0] io_out_0_instr,",
    "  output logic [63:0] io_out_1_pc,",
    "  output logic [31:0] io_out_1_instr,",
    "  output logic [63:0] io_out_2_pc,",
    "  output logic [31:0] io_out_2_instr,",
    "  output logic [63:0] io_out_3_pc,",
    "  output logic [31:0] io_out_3_instr,",
    "  output logic [63:0] io_out_4_pc,",
    "  output logic [31:0] io_out_4_instr,",
    "  output logic [63:0] io_out_5_pc,",
    "  output logic [31:0] io_out_5_instr,",
    "  output logic [63:0] io_out_6_pc,",
    "  output logic [31:0] io_out_6_instr,",
    "  output logic [63:0] io_out_7_pc,",
    "  output logic [31:0] io_out_7_instr,",
    "  output logic [31:0] io_out_0_preDecode,",
    "  output logic [31:0] io_out_1_preDecode,",
    "  output logic [31:0] io_out_2_preDecode,",
    "  output logic [31:0] io_out_3_preDecode,",
    "  output logic [31:0] io_out_4_preDecode,",
    "  output logic [31:0] io_out_5_preDecode,",
    "  output logic [31:0] io_out_6_preDecode,",
    "  output logic [31:0] io_out_7_preDecode,",
    "  output logic [63:0] io_out_newestPc,",
    "  output logic [31:0] io_out_newestPreDecode,",
    "  output logic [63:0] io_out_ftqPc,",
    "  output logic [31:0] io_out_ftqPackData,",
    "",
    "  input  [31:0] io_updatePtrCount,",
    "  input  [3:0]  io_fetchOffset,",
    "  input  [3:0]  io_fetchCount,",
    "  input         io_robCommitValid,",
    "  input         io_robCommitFtqFlag,",
    "  input  [5:0]  io_robCommitFtqValue,",
    "  input         io_redirect,",
    "  input         io_redirectFtqFlag,",
    "  input  [5:0]  io_redirectFtqValue,",
    "  input  [31:0] io_redirectType,",
    "  input  [63:0] io_redirectPc,",
    "  input  [63:0] io_redirectTarget",
    ");",
    "",
    "",
    "task automatic fetch_lane;",
    "  input integer offset;",
    "  input integer enabled;",
    "  output logic [63:0] pc;",
    "  output logic [31:0] instr;",
    "  output logic [31:0] preDecode;",
    "  begin",
    "    if (enabled != 0) begin",
    "      SimFrontFetch(offset, pc, instr, preDecode);",
    "    end else begin",
    "      pc = 0;",
    "      instr = 0;",
    "      preDecode = 0;",
    "    end",
    "  end",
    "endtask",
    "",
    "always @(posedge clock or posedge reset) begin",
    "  if (!reset) begin",
    "    SimFrontUpdatePtr(io_updatePtrCount);",
    "",
    "    SimFrontRedirect(io_redirect, io_redirectFtqFlag, io_redirectFtqValue, io_redirectType, io_redirectPc, io_redirectTarget);",
    "",
    "    fetch_lane(io_fetchOffset + 0, io_fetchCount > 0, io_out_0_pc, io_out_0_instr, io_out_0_preDecode);",
    "    fetch_lane(io_fetchOffset + 1, io_fetchCount > 1, io_out_1_pc, io_out_1_instr, io_out_1_preDecode);",
    "    fetch_lane(io_fetchOffset + 2, io_fetchCount > 2, io_out_2_pc, io_out_2_instr, io_out_2_preDecode);",
    "    fetch_lane(io_fetchOffset + 3, io_fetchCount > 3, io_out_3_pc, io_out_3_instr, io_out_3_preDecode);",
    "    fetch_lane(io_fetchOffset + 4, io_fetchCount > 4, io_out_4_pc, io_out_4_instr, io_out_4_preDecode);",
    "    fetch_lane(io_fetchOffset + 5, io_fetchCount > 5, io_out_5_pc, io_out_5_instr, io_out_5_preDecode);",
    "    fetch_lane(io_fetchOffset + 6, io_fetchCount > 6, io_out_6_pc, io_out_6_instr, io_out_6_preDecode);",
    "    fetch_lane(io_fetchOffset + 7, io_fetchCount > 7, io_out_7_pc, io_out_7_instr, io_out_7_preDecode);",
    "",
    "    SimFrontGetFtqToBackEnd(io_out_ftqPc, io_out_ftqPackData, io_out_newestPc, io_out_newestPreDecode);",
    "",
    "    SimFrontRobCommit(io_robCommitValid, io_robCommitFtqFlag, io_robCommitFtqValue);",
    "  end",
    "end",
    "",
    "endmodule"
  )
  setInline(s"$desiredName.v", verilogLines.mkString("\n"))
}

class SimFrontendInlinedImp(outer: FrontendInlined) extends FrontendInlinedImpBase(outer) {
  val instrUncache = outer.instrUncache.module
  val icache       = outer.icache.module
  icache.io <> WireDefault(0.U.asTypeOf(icache.io))
  instrUncache.io <> WireDefault(0.U.asTypeOf(instrUncache.io))
  io <> WireDefault(0.U.asTypeOf(io))

  val fetchHelper = Module(new SimFrontFetchHelper)
  val predInstAccept = Module(new PredInstAccept)
  val predUopNum     = Module(new PredUopNum)
  val vtypeGen       = Module(new VTypeGen)

  private val holdEntries = RegInit(VecInit.fill(DecodeWidth)(0.U.asTypeOf(Valid(new IBufEntry))))
  private val fetchPending = RegInit(false.B)
  private val holdValidNum = PriorityMuxDefault(
    holdEntries.map(_.valid).zip(Seq.range(1, DecodeWidth + 1).map(_.U)).reverse,
    0.U
  )

  private val redirect = io.backend.toFtq.redirect.valid
  private val presentationEnable = !fetchPending && !io.backend.toIBuf.resumingVType && !redirect
  private val presentationEntries = Wire(Vec(DecodeWidth, Valid(new IBufOutEntry)))
  private val emptyException = 0.U.asTypeOf(new IBufExceptionEntry)

  for (i <- 0 until DecodeWidth) {
    predUopNum.in.valid(i) := holdEntries(i).valid && presentationEnable
    predUopNum.in.inst(i)  := holdEntries(i).bits.inst
    predUopNum.in.vtype(i) := vtypeGen.out.vtype(i)
  }
  predUopNum.in.fromCSR := io.backend.toIBuf.fromCSR
  predUopNum.in.vstart  := io.backend.toIBuf.vstart

  for (i <- 0 until DecodeWidth) {
    presentationEntries(i).valid := holdEntries(i).valid && presentationEnable
    presentationEntries(i).bits := holdEntries(i).bits.toIBufOutEntry(
      emptyException,
      vtypeGen.out.vtype(i),
      vtypeGen.out.oldVType(i),
      predUopNum.out.uopNumOH(i)
    )
  }

  predInstAccept.in.outputEntries := presentationEntries
  predInstAccept.in.flush         := redirect
  predInstAccept.in.decodeAccept  := io.backend.toIBuf.decodeCanAccept && presentationEnable

  private val predAccNum = Mux(
    io.backend.toIBuf.decodeCanAccept && presentationEnable,
    predInstAccept.out.predAccNum.min(holdValidNum),
    0.U
  )
  private val holdKeepNum = holdValidNum - predAccNum
  private val issueFetch = redirect || (!fetchPending && !io.backend.toIBuf.resumingVType && holdKeepNum =/= DecodeWidth.U)
  private val fetchOffset = Mux(redirect, 0.U, holdKeepNum)
  private val fetchCount = Mux(redirect, DecodeWidth.U, DecodeWidth.U - holdKeepNum)

  fetchHelper.clock := this.clock
  fetchHelper.reset := this.reset

  fetchHelper.io.updatePtrCount := Mux(redirect, 0.U, predAccNum)
  fetchHelper.io.fetchOffset := Mux(issueFetch, fetchOffset, 0.U)
  fetchHelper.io.fetchCount := Mux(issueFetch, fetchCount, 0.U)

  // For now, there is only one type, but for the sake of scalability, let's write it this way.
  object RedirectType {
    def isMisPred     = 1.U
    def isFalseBranch = 2.U

    def genRedirectType(redirect: Redirect): UInt = {
      val this_pc = redirect.pc +& redirect.getPcOffset()
      Mux(
        redirect.isMisPred,
        Mux(this_pc + Mux(redirect.isRVC, 2.U, 4.U) === redirect.target, isFalseBranch, isMisPred),
        0.U
      )
    }
  }

  fetchHelper.io.redirect         := io.backend.toFtq.redirect.valid
  fetchHelper.io.redirectFtqFlag  := io.backend.toFtq.redirect.bits.ftqIdx.flag
  fetchHelper.io.redirectFtqValue := io.backend.toFtq.redirect.bits.ftqIdx.value
  fetchHelper.io.redirectType     := RedirectType.genRedirectType(io.backend.toFtq.redirect.bits)
  fetchHelper.io.redirectPc       := io.backend.toFtq.redirect.bits.pc + io.backend.toFtq.redirect.bits.getPcOffset()
  fetchHelper.io.redirectTarget   := io.backend.toFtq.redirect.bits.target

  fetchHelper.io.robCommitValid    := io.backend.toFtq.commit.valid
  fetchHelper.io.robCommitFtqFlag  := io.backend.toFtq.commit.bits.flag
  fetchHelper.io.robCommitFtqValue := io.backend.toFtq.commit.bits.value

  val fetchResponse = Wire(Vec(DecodeWidth, Valid(new IBufEntry)))
  fetchResponse.zip(fetchHelper.io.out).foreach { case (response, fetchOut) =>
    val rvcExpanders = Module(new RvcExpander)

    rvcExpanders.io.in      := fetchOut.instr
    rvcExpanders.io.fsIsOff := io.csrCtrl.fsIsOff

    response := 0.U.asTypeOf(response)
    response.valid := fetchOut.preDecode(0)
    response.bits.pc := fetchOut.pc(VAddrBits - 1, 0)
    response.bits.foldpc := XORFold(fetchOut.pc(VAddrBits - 1, 1), MemPredPCWidth)
    response.bits.inst := Mux(rvcExpanders.io.ill, fetchOut.instr, rvcExpanders.io.out.bits)
    response.bits.isRvc := fetchOut.preDecode(1)
    response.bits.fixedTaken := fetchOut.preDecode(6)
    response.bits.predTaken := fetchOut.preDecode(6)
    response.bits.ftqPtr.value := fetchOut.preDecode(12, 7)
    response.bits.ftqPtr.flag := fetchOut.preDecode(13)
    response.bits.isLastInFtqEntry := fetchOut.preDecode(14)
    response.bits.instrEndOffset := fetchOut.preDecode(18, 15)
    response.bits.triggered := TriggerAction.None
    response.bits.vtypeEntry := VTypeGen.Entry.fromInst(response.bits.inst)
  }

  when (redirect) {
    holdEntries := VecInit.fill(DecodeWidth)(0.U.asTypeOf(Valid(new IBufEntry)))
  }.elsewhen(fetchPending) {
    for (i <- 0 until DecodeWidth) {
      holdEntries(i) := Mux(
        i.U < holdValidNum,
        holdEntries(i),
        fetchResponse(i.U - holdValidNum)
      )
    }
  }.otherwise {
    for (i <- 0 until DecodeWidth) {
      holdEntries(i) := Mux(
        i.U < holdKeepNum,
        holdEntries(i.U + predAccNum),
        0.U.asTypeOf(holdEntries(i))
      )
    }
  }
  fetchPending := Mux(redirect, true.B, Mux(fetchPending, false.B, issueFetch))

  io.backend.cfVec.zip(presentationEntries).zipWithIndex.foreach { case ((cfVec, entry), idx) =>
    cfVec.valid := entry.valid
    cfVec.bits := entry.bits.toCtrlFlow
    cfVec.bits.debug_seqNum.seqNum := PerfCCT.createInstMetaAtFetch(
      (idx + 1).U,
      entry.bits.pc.toUInt,
      entry.bits.inst,
      predAccNum > idx.U,
      clock,
      reset
    )
  }

  vtypeGen.in.canUpdateVType  := !io.backend.toIBuf.resumingVType && !redirect
  vtypeGen.in.walkToArchVType := io.backend.toIBuf.walkToArchVType
  vtypeGen.in.walkVType       := io.backend.toIBuf.walkVType
  vtypeGen.in.vsetvlVType     := io.backend.toIBuf.vsetvlVType
  vtypeGen.in.commitVType     := io.backend.toIBuf.commitVType
  vtypeGen.in.validNum        := predAccNum
  for (i <- 0 until DecodeWidth) {
    vtypeGen.in.vtypeEntries(i) := VTypeGen.Entry.fromInst(holdEntries(i).bits.inst)
  }

  io.backend.fromFtq.wen     := fetchHelper.io.out_ftqPackData(6)
  io.backend.fromFtq.ftqIdx  := fetchHelper.io.out_ftqPackData(5, 0)
  io.backend.fromFtq.startPc := GuardedPcInit(fetchHelper.io.out_ftqPc(GuardedVAddrBits - 1, 0))

  XSPerfAccumulate("all_redirect", io.backend.toFtq.redirect.valid)
  XSPerfAccumulate("mispred_redirect", io.backend.toFtq.redirect.valid && io.backend.toFtq.redirect.bits.isMisPred)

  override val perfEvents: Seq[(String, UInt)] = Seq(
    ("empty_perf_event_0", 0.U(1.W)),
    ("empty_perf_event_1", 0.U(1.W)),
    ("empty_perf_event_2", 0.U(1.W)),
    ("empty_perf_event_3", 0.U(1.W)),
    ("empty_perf_event_4", 0.U(1.W)),
    ("empty_perf_event_5", 0.U(1.W)),
    ("empty_perf_event_6", 0.U(1.W)),
    ("empty_perf_event_7", 0.U(1.W))
  )
  generatePerfEvent()
}
