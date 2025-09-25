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
import xiangshan._
import xiangshan.frontend._
import xiangshan.frontend.ifu._

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
    "  output [63:0] io_out_0_pc,",
    "  output [31:0] io_out_0_instr,",
    "  output [63:0] io_out_1_pc,",
    "  output [31:0] io_out_1_instr,",
    "  output [63:0] io_out_2_pc,",
    "  output [31:0] io_out_2_instr,",
    "  output [63:0] io_out_3_pc,",
    "  output [31:0] io_out_3_instr,",
    "  output [63:0] io_out_4_pc,",
    "  output [31:0] io_out_4_instr,",
    "  output [63:0] io_out_5_pc,",
    "  output [31:0] io_out_5_instr,",
    "  output [63:0] io_out_6_pc,",
    "  output [31:0] io_out_6_instr,",
    "  output [63:0] io_out_7_pc,",
    "  output [31:0] io_out_7_instr,",
    "  output [31:0] io_out_0_preDecode,",
    "  output [31:0] io_out_1_preDecode,",
    "  output [31:0] io_out_2_preDecode,",
    "  output [31:0] io_out_3_preDecode,",
    "  output [31:0] io_out_4_preDecode,",
    "  output [31:0] io_out_5_preDecode,",
    "  output [31:0] io_out_6_preDecode,",
    "  output [31:0] io_out_7_preDecode,",
    "  output [63:0] io_out_newestPc,",
    "  output [31:0] io_out_newestPreDecode,",
    "  output [63:0] io_out_ftqPc,",
    "  output [31:0] io_out_ftqPackData,",
    "",
    "  input  [31:0] io_updatePtrCount,",
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
    "always @(posedge clock or posedge reset) begin",
    "  if (!reset) begin",
    "    SimFrontUpdatePtr(io_updatePtrCount);",
    "",
    "    SimFrontRedirect(io_redirect, io_redirectFtqFlag, io_redirectFtqValue, io_redirectType, io_redirectPc, io_redirectTarget);",
    "",
    "    SimFrontFetch(0, io_out_0_pc, io_out_0_instr, io_out_0_preDecode);",
    "    SimFrontFetch(1, io_out_1_pc, io_out_1_instr, io_out_1_preDecode);",
    "    SimFrontFetch(2, io_out_2_pc, io_out_2_instr, io_out_2_preDecode);",
    "    SimFrontFetch(3, io_out_3_pc, io_out_3_instr, io_out_3_preDecode);",
    "    SimFrontFetch(4, io_out_4_pc, io_out_4_instr, io_out_4_preDecode);",
    "    SimFrontFetch(5, io_out_5_pc, io_out_5_instr, io_out_5_preDecode);",
    "    SimFrontFetch(6, io_out_6_pc, io_out_6_instr, io_out_6_preDecode);",
    "    SimFrontFetch(7, io_out_7_pc, io_out_7_instr, io_out_7_preDecode);",
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
  icache.io <> 0.U.asTypeOf(icache.io)
  instrUncache.io <> 0.U.asTypeOf(instrUncache.io)
  io <> 0.U.asTypeOf(io.cloneType)

  val fetchHelper = Module(new SimFrontFetchHelper)

  val readyCount        = Mux(io.backend.canAccept, PopCount(io.backend.cfVec.map(_.ready)), 0.U)
  val robCommitValidVec = io.backend.toFtq.rob_commits.map(_.valid)
  val robCommitBitsVec  = io.backend.toFtq.rob_commits.map(_.bits)
  val robCommitValid    = robCommitValidVec.reduce(_ || _)
  val robCommitBits     = ParallelPosteriorityMux(robCommitValidVec, robCommitBitsVec)

  fetchHelper.clock := this.clock
  fetchHelper.reset := this.reset

  fetchHelper.io.updatePtrCount := readyCount

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

  fetchHelper.io.robCommitValid    := robCommitValid
  fetchHelper.io.robCommitFtqFlag  := robCommitBits.ftqIdx.flag
  fetchHelper.io.robCommitFtqValue := robCommitBits.ftqIdx.value

  io.backend.cfVec.zip(fetchHelper.io.out).map { case (cfVec, fetchOut) =>
    val rvcExpanders = Module(new RvcExpander)

    rvcExpanders.io.in      := fetchOut.instr
    rvcExpanders.io.fsIsOff := io.csrCtrl.fsIsOff

    cfVec.bits.pc     := fetchOut.pc
    cfVec.bits.foldpc := XORFold(fetchOut.pc(VAddrBits - 1, 1), MemPredPCWidth)
    cfVec.bits.instr  := Mux(rvcExpanders.io.ill, fetchOut.instr, rvcExpanders.io.out.bits)
    cfVec.valid       := fetchOut.preDecode(0)

    cfVec.bits.trigger := TriggerAction.None

    cfVec.bits.pd.valid  := fetchOut.preDecode(0)
    cfVec.bits.pd.isRVC  := fetchOut.preDecode(1)
    cfVec.bits.pd.brType := fetchOut.preDecode(3, 2)
    cfVec.bits.pd.isCall := fetchOut.preDecode(4)
    cfVec.bits.pd.isRet  := fetchOut.preDecode(5)

    cfVec.bits.pred_taken := fetchOut.preDecode(6)

    cfVec.bits.ftqPtr.value := fetchOut.preDecode(12, 7)
    cfVec.bits.ftqPtr.flag  := fetchOut.preDecode(13)

    cfVec.bits.isLastInFtqEntry := fetchOut.preDecode(14)
    cfVec.bits.ftqOffset        := fetchOut.preDecode(18, 15)
  }

  io.backend.fromFtq.pc_mem_wen   := fetchHelper.io.out_ftqPackData(6)
  io.backend.fromFtq.pc_mem_waddr := fetchHelper.io.out_ftqPackData(5, 0)
  io.backend.fromFtq.pc_mem_wdata := PrunedAddrInit(fetchHelper.io.out_ftqPc)

  io.backend.fromFtq.newest_entry_en        := fetchHelper.io.out_newestPreDecode(6)
  io.backend.fromFtq.newest_entry_ptr.value := fetchHelper.io.out_newestPreDecode(5, 0)
  io.backend.fromFtq.newest_entry_target    := fetchHelper.io.out_newestPc

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
