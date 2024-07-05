package xiangshan.backend.fu.matu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import matu.DataBuffer._
import matu.SystolicArray._
import utils._
import xiangshan.{MicroOp, _}
import xiangshan.backend.exu.ExuParameters
import xiangshan.backend.fu._
import xiangshan.backend.rob._


class ScoreboardPtr (implicit p: Parameters) extends CircularQueuePtr[ScoreboardPtr](
  p => p(XSCoreParamsKey).ScoreboardSize
) with HasCircularQueuePtrHelper {

}

object ScoreboardPtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): ScoreboardPtr = {
    val ptr = Wire(new ScoreboardPtr)
    ptr.flag := f
    ptr.value := v
    ptr
  }
}

class load_in(implicit p: Parameters) extends XSBundle {
  val data_in = Input(Vec(exuParameters.LduCnt, UInt(XLEN.W)))
  val uop_in = Input(Vec(exuParameters.LduCnt, new MicroOp))
  val valid_in = Input(Vec(exuParameters.LduCnt, Bool()))
}

class load_out(implicit p: Parameters) extends XSBundle {
  val wen = Output(Bool())
  val data_out = Output(UInt(XLEN.W))
  val addr_out = Output(UInt(3.W))
  val offset_out = Output(UInt(2.W))
}

class store_io(implicit p: Parameters) extends XSBundle {
  val raddr_out = Output(UInt(3.W))
  val roffset_out = Output(UInt(2.W))
  val store_flag = Output(Bool())
  val pc_out = Output(UInt(VAddrBits.W))
  val robIdx_out = Output(UInt(5.W))
}

class fu_io(implicit p: Parameters) extends XSBundle {
  val OpType_out = Output(FuOpType())
  val valid_out = Output(Bool())
  val rs1_out = Output(UInt(3.W))
  val rs2_out = Output(UInt(3.W))
  val rd_out = Output(UInt(3.W))
  val ready_in = Input(Bool())
}

class commits_scb_io(implicit p: Parameters) extends XSBundle {
  val commits_pc = Input(Vec(CommitWidth,  UInt(VAddrBits.W)))
  val commits_robIdx = Input(Vec(CommitWidth, new RobPtr))
  val commits_valid = Input(Vec(CommitWidth, Bool()))
}

class writeback_in(implicit p: Parameters) extends XSBundle {
  val wen = Input(Vec(2, Bool()))
  val waddr = Input(Vec(2, UInt(3.W)))
  val woffset = Input(Vec(2, UInt(2.W)))
}

class flush_in(implicit p: Parameters) extends XSBundle {
  val redirect = Flipped(ValidIO(new Redirect))
}

class Scoreboard (implicit  p: Parameters) extends XSModule with HasXSParameter with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val ldIn = new load_in()
    val ldOut = new load_out()
    val stIO = new store_io()
    val dpUopIn = Vec(RenameWidth, Flipped(ValidIO(new MicroOp)))
    val fuIO = new fu_io()
    val commitsIO = new commits_scb_io()
    val wbIn= new writeback_in()
    val flushIn = new flush_in()
    val canAccept = Output(Bool())
  })

  val s_idle :: s_wait :: s_commit :: s_retire :: s_unready :: s_ready :: Nil = Enum(6) // ins state

  val OpType_w = Wire(Vec(RenameWidth, FuOpType()))
  val instr_w = Wire(Vec(RenameWidth, UInt(32.W)))
  val rs1_w = Wire(Vec(RenameWidth, UInt(3.W)))
  val rs2_w = Wire(Vec(RenameWidth, UInt(3.W)))
  val rs2_offset_w = Wire(Vec(RenameWidth, UInt(2.W))) // for store
  val rd_w = Wire(Vec(RenameWidth, UInt(3.W)))
  val rd_offset_w = Wire(Vec(RenameWidth, UInt(2.W))) // for load
  val pc_w = Wire(Vec(RenameWidth, UInt(VAddrBits.W)))
  val robIdx_w = Wire(Vec(RenameWidth, new RobPtr))
  val dp_valid_w = Wire(Vec(RenameWidth, Bool()))




  for(i <- 0 until RenameWidth) {
    OpType_w(i) := io.dpUopIn(i).bits.ctrl.fuOpType
    instr_w(i) := io.dpUopIn(i).bits.cf.instr
    rs1_w(i) := io.dpUopIn(i).bits.cf.instr(17, 15)
    rs2_w(i) := io.dpUopIn(i).bits.cf.instr(22, 20)
    rs2_offset_w(i) := io.dpUopIn(i).bits.cf.instr(24, 23)
    rd_w(i) := io.dpUopIn(i).bits.cf.instr(9, 7)
    rd_offset_w(i) := io.dpUopIn(i).bits.cf.instr(11, 10)
    pc_w(i) := io.dpUopIn(i).bits.cf.pc
    robIdx_w(i) := io.dpUopIn(i).bits.robIdx
    dp_valid_w(i) := io.dpUopIn(i).valid && io.dpUopIn(i).bits.cf.instr(6, 0) === "b0101011".U
  }

  // predecode

  val state_array = dontTouch(RegInit(VecInit(Seq.fill(32)(s_idle))))
  val next_state_array = dontTouch(WireInit(state_array))
  val OpType_array = dontTouch(Reg(Vec(32, FuOpType())))
  val instr_array = dontTouch(Reg(Vec(32, UInt(32.W))))
  val rs1_array = dontTouch(Reg(Vec(32, UInt(3.W))))
  val rs1_ready_array = dontTouch(RegInit(VecInit(Seq.fill(32)(s_unready))))
  val rs2_array = dontTouch(Reg(Vec(32, UInt(3.W))))
  val rs2_offset_array = dontTouch(Reg(Vec(32, UInt(2.W))))
  val rs2_ready_array = dontTouch(RegInit(VecInit(Seq.fill(32)(s_unready))))
  val rd_array = dontTouch(Reg(Vec(32, UInt(3.W))))
  val rd_value_array = dontTouch(Reg(Vec(32, UInt(XLEN.W))))
  val rd_offset_array = dontTouch(Reg(Vec(32, UInt(2.W))))
  val pc_array = dontTouch(Reg(Vec(32, UInt(VAddrBits.W))))
  val robIdx_array = dontTouch(Reg(Vec(32, new RobPtr)))
  val time_out_cnt_array = dontTouch(RegInit(VecInit(Seq.fill(32)(0.U(10.W)))))
  val history_ptr_array = dontTouch(RegInit(VecInit(Seq.fill(32)(0.U.asTypeOf(new ScoreboardPtr)))))

  val flush_w = dontTouch(Wire(Vec(32, Bool())))
  val first_flush_w = dontTouch(Wire(Vec(32, Bool())))

  for (i <- 0 until 32) {
    flush_w(i) := robIdx_array(i).needFlush(io.flushIn.redirect) && (state_array(i) === s_wait)
  }

  for (i <- 0 until 32) {
    val uflush_w = dontTouch(Wire(Vec(32, Bool())))
    for (j <- 0 until 32) {
     uflush_w(j) := flush_w(i) && ((isAfter(robIdx_array(j), robIdx_array(i)) || !flush_w(j)) || (i.U === j.U))
    }
    first_flush_w(i) := uflush_w.asUInt.andR
  }

  time_out_cnt_array(0) := Mux(state_array(0)===s_wait && state_array(31)=/=s_wait, time_out_cnt_array(0)+1.U, 0.U)
  for (i <- 1 until 32) {
    time_out_cnt_array(i) := Mux(state_array(i)===s_wait && state_array(i-1)=/=s_wait, time_out_cnt_array(i)+1.U, 0.U)
  }


  for (i <- 0 until 32) {
    state_array(i) := next_state_array(i)
  }

  /** allow enqueue */
  val enqPtr = dontTouch(RegInit(0.U.asTypeOf(new ScoreboardPtr)))
  val next_enqPtr = dontTouch(Wire(new ScoreboardPtr))
  val next_enqPtr_redirect = dontTouch(Wire(new ScoreboardPtr))
  val deqPtr = dontTouch(RegInit(0.U.asTypeOf(new ScoreboardPtr)))

  val allowEnqueue = RegInit(true.B)
  val numValidEntries = distanceBetween(enqPtr, deqPtr)
  val deqNum = state_array(deqPtr.value) === s_retire
  allowEnqueue := numValidEntries + deqNum.asUInt  <= (MpuScoreboardSize - RenameWidth).U
  io.canAccept := allowEnqueue

  val dispatchNum = Mux(allowEnqueue, PopCount(dp_valid_w), 0.U)
  next_enqPtr_redirect.value := PriorityEncoder(first_flush_w.asUInt)
  next_enqPtr_redirect.flag := history_ptr_array(PriorityEncoder(first_flush_w.asUInt)).flag
  next_enqPtr := Mux(flush_w.asUInt.orR, next_enqPtr_redirect + dispatchNum, enqPtr + dispatchNum)
  enqPtr := next_enqPtr

  val enq_offset = dontTouch(Wire(Vec(RenameWidth, UInt(6.W))))
  val offset = (0 until RenameWidth).map(i => Mux(allowEnqueue, PopCount(dp_valid_w.take(i)), 0.U))
  enq_offset := offset

  /** enqueue
   * state: s_idle -> s_wait
   * when the type of a valid instr is Matrix-extension, the instr can enqueue
   * the corresponding state is set to s_wait
   * Entry type: 1. OpType  2. rs1 2. rs2 3. rs2_offset 4. rd  5. rd_offset  6. pc  7. robIdx  8. state
   */
  for(i <- 0 until RenameWidth) {
    when(state_array(enqPtr.value + enq_offset(i)) === s_idle && dp_valid_w(i) === true.B){
      OpType_array(enqPtr.value + enq_offset(i)) := OpType_w(i)
      instr_array(enqPtr.value + enq_offset(i)) := instr_w(i)
      rs1_array(enqPtr.value + enq_offset(i)) := rs1_w(i)
      rs2_array(enqPtr.value + enq_offset(i)) := rs2_w(i)
      rs2_offset_array(enqPtr.value + enq_offset(i)) := rs2_offset_w(i)
      rd_array(enqPtr.value + enq_offset(i)) := rd_w(i)
      rd_offset_array(enqPtr.value + enq_offset(i)) := rd_offset_w(i)
      pc_array(enqPtr.value + enq_offset(i)) := pc_w(i)
      robIdx_array(enqPtr.value + enq_offset(i)) := robIdx_w(i)
      next_state_array(enqPtr.value + enq_offset(i)) := s_wait
      history_ptr_array(enqPtr.value + enq_offset(i)) := enqPtr + enq_offset(i)
    }
  }

  /** load in
   * 2 channels load data in
   */
  for (i <- 0 until exuParameters.LduCnt) {
    for (j <- 0 until 32) {
      when(io.ldIn.uop_in(i).cf.pc === pc_array(j) && io.ldIn.valid_in(i)) {
        rd_value_array(j) := io.ldIn.data_in(i)
      }
    }
  }


  /** instr state switch
   * state: s_wait -> s_commit
   * when the instr is commited in rob, the state of instr is set to s_commit
   * state: s_commit -> s_retire
   * when the instr finally writeback data to regfile, the state of instr is set to s_retire
   */
  for (i <- 0 until 32) {
    val commit_flag = Seq.tabulate(CommitWidth)(j =>
      state_array(i) === s_wait && io.commitsIO.commits_valid(j) && (io.commitsIO.commits_pc(j) === pc_array(i)) &&
                         (io.commitsIO.commits_robIdx(j).value === robIdx_array(i).value)
                         && (io.commitsIO.commits_robIdx(j).flag === robIdx_array(i).flag)
    )
    when(state_array(i) === s_wait) {
      next_state_array(i) := Mux(time_out_cnt_array(i) >= 256.U, s_retire,
                             Mux(flush_w.asUInt.orR && (isAfter(history_ptr_array(i), next_enqPtr_redirect) ||
                             isItself(history_ptr_array(i), next_enqPtr_redirect)) , s_idle,
                             Mux(commit_flag.reduce(_||_), s_commit, s_wait)))
    }
  }


  val commitVec = Wire(Vec(3, Vec(32, Bool())))
  for (i <- 0 until 32) {
    commitVec(0)(i) := (state_array(i) === s_commit || state_array(i) === s_wait) && io.wbIn.wen(0) &&
                        io.wbIn.waddr(0) === rd_array(i) && io.wbIn.woffset(0) === rd_offset_array(i) &&
                       OpType_array(i) === LSUOpType.mld
    commitVec(1)(i) := state_array(i) === s_commit && io.wbIn.wen(1) && io.wbIn.waddr(1) === rd_array(i) &&
                       rs1_ready_array(i) === s_ready && rs2_ready_array(i) === s_ready &&
                      (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest)
    commitVec(2)(i) := (state_array(i) === s_commit) && (OpType_array(i) === LSUOpType.sd)
  }

  /*  Seq.tabulate(3)(i =>
    VecInit.tabulate(32)(j =>
      state_array(j) === s_commit &&
      io.wbIn.wen(i) &&
      io.wbIn.waddr(i) === rd_array(j) &&
        ((io.wbIn.woffset(i) === offset_array(j) ||
          OpType_array(j) === MATUOpType.mmul || OpType_array(j) === MATUOpType.mtest) */

  val commitVecUInt = Wire(Vec(3, UInt(32.W)))
  val real_commitVec = Wire(Vec(3, Vec(32, Bool())))
  real_commitVec.foreach(_.foreach(_ := false.B))
  for (i <- 0 until 3) {
    commitVecUInt(i) := commitVec(i).asUInt
  }

  val selBits = Seq.tabulate(3)(i => Seq.tabulate(32)(j => Mux(j.U >= deqPtr.value, true.B, false.B)))
  val shiftedIndices = Wire(Vec(3, Vec(32, UInt(5.W))))
  for (i <- 0 until 3) {
    for (j <- 0 until 32) {
      shiftedIndices(i)(j) := Mux(selBits(i)(j), j.U - (32.U - PopCount(selBits(i))), j.U + PopCount(selBits(i)))
    }
  }
  for (i <- 0 until 3) {
    for (j <- 0 until 32) {
      real_commitVec(i)(shiftedIndices(i)(j)) := commitVecUInt(i)(j).asBool
    }
  }

//  for (i <- 0 until 32) {
//    next_state_array(i) := Mux(state_array(i) === s_commit && OpType_array(i) === LSUOpType.sd && io.stIO.fire)
//  }

  for (i <- 0 until 3) {
    when(commitVec(i).asUInt.orR) {
      next_state_array(deqPtr.value + PriorityEncoder(real_commitVec(i))) := s_retire
    }
  }

  /** rs state switch
   * state: s_unready -> s_ready, s_ready -> s_unready
   */
  for (i <- 0 until 32) {
    val rs1MatchVec_1 = dontTouch(Wire(Vec(32, Bool())))
    val rs1MatchVec_2 = dontTouch(Wire(Vec(32, Bool())))
    val real_rs1MatchVec = dontTouch(Wire(Vec(32, Bool())))
    for (j <- 0 until 32) {
      rs1MatchVec_1(j) := rd_array(j) === rs1_array(i) &&
                          (state_array(i) === s_wait || state_array(i) === s_commit) &&
                          (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest) &&
                          (state_array(j) === s_retire || state_array(j) === s_idle) &&
                          isAfter(history_ptr_array(i), history_ptr_array(j))
      rs1MatchVec_2(j) := rd_array(j) === rs1_array(i) && (state_array(i) === s_wait || state_array(i) === s_commit) &&
                          (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest) &&
                          isAfter(history_ptr_array(i), history_ptr_array(j))
      real_rs1MatchVec(j) := ((rd_array(j) === rs1_array(i) &&
                             (state_array(i) === s_wait || state_array(i) === s_commit) &&
                             (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest) &&
                             (state_array(j) === s_retire || state_array(j) === s_idle) &&
                             isAfter(history_ptr_array(i), history_ptr_array(j))) === (rd_array(j) === rs1_array(i) &&
                             (state_array(i) === s_wait || state_array(i) === s_commit) &&
                             (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest) &&
                             isAfter(history_ptr_array(i), history_ptr_array(j)))) &&
                             (state_array(i) === s_wait || state_array(i) === s_commit)
    }
    rs1_ready_array(i) := Mux(real_rs1MatchVec.asUInt.andR, s_ready, s_unready)
  }

  for (i <- 0 until 32) {
    val rs2MatchVec_1 = dontTouch(Wire(Vec(32, Bool())))
    val rs2MatchVec_2 = dontTouch(Wire(Vec(32, Bool())))
    val real_rs2MatchVec = dontTouch(Wire(Vec(32, Bool())))
    for (j <- 0 until 32) {
      rs2MatchVec_1(j) := rd_array(j) === rs2_array(i) &&
                          (state_array(i) === s_wait || state_array(i) === s_commit) &&
                          (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest || OpType_array(i) === LSUOpType.sd) &&
                          (state_array(j) === s_retire || state_array(j) === s_idle) &&
                          isAfter(history_ptr_array(i), history_ptr_array(j))
      rs2MatchVec_2(j) := rd_array(j) === rs2_array(i) &&
                          (state_array(i) === s_wait || state_array(i) === s_commit) &&
                          (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest || OpType_array(i) ===LSUOpType.sd) &&
                          isAfter(history_ptr_array(i), history_ptr_array(j))
      real_rs2MatchVec(j) := ((rd_array(j) === rs2_array(i) &&
                             (state_array(i) === s_wait || state_array(i) === s_commit) &&
                             (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest || OpType_array(i) === LSUOpType.sd) &&
                             (state_array(j) === s_retire || state_array(j) === s_idle) &&
                             isAfter(history_ptr_array(i), history_ptr_array(j)))
                             === (rd_array(j) === rs2_array(i) && (state_array(i) === s_wait || state_array(i) === s_commit) &&
                             (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest || OpType_array(i) === LSUOpType.sd) &&
                             isAfter(history_ptr_array(i), history_ptr_array(j)))) && (state_array(i) === s_wait || state_array(i) === s_commit)
    }
    rs2_ready_array(i) := Mux(real_rs2MatchVec.asUInt.andR, s_ready, s_unready)
  }

  /** MAT instr WAW WAR
   * Handle write after write, write after read hazard
   * */
  val rd_ready_array = dontTouch(RegInit(VecInit(Seq.fill(32)(s_ready))))
  for (i <- 0 until 32) {
    val m_wawMatchVec = dontTouch(Wire(Vec(32, Bool())))
    val m_warMatchVec = dontTouch(Wire(Vec(32, Bool())))
    for (j <- 0 until 32) {
      m_wawMatchVec(j) := rd_array(j) === rd_array(i) &&
                         (OpType_array(i) === MATUOpType.mtest || OpType_array(i) === MATUOpType.mmul) &&
                         (state_array(j) === s_wait || state_array(j) === s_commit) &&
                         (state_array(i) === s_commit || state_array(i) === s_wait) &&
                         ((OpType_array(j) === LSUOpType.mld && rd_offset_array(i) === rd_offset_array(j)) ||
                         (OpType_array(j) === MATUOpType.mmul ||
                         OpType_array(j) === MATUOpType.mtest)) && isAfter(history_ptr_array(i), history_ptr_array(j))
      m_warMatchVec(j) := (rs1_array(j) === rd_array(i) || rs2_array(j) === rd_array(i)) &&
                          (OpType_array(i) === MATUOpType.mtest || OpType_array(i) === MATUOpType.mmul) &&
                          (state_array(j) === s_wait || state_array(j) === s_commit) &&
                          (state_array(i) === s_commit || state_array(i) === s_wait) &&
                          ((OpType_array(j) === LSUOpType.sd && rd_offset_array(i) === rs2_offset_array(j)) ||
                          (OpType_array(j) === MATUOpType.mmul || OpType_array(j) === MATUOpType.mtest)) &&
                          isAfter(history_ptr_array(i), history_ptr_array(j))
    }
    rd_ready_array(i) := Mux(m_wawMatchVec.asUInt.orR || m_warMatchVec.asUInt.orR, s_unready, s_ready)
  }

  val rs_ready_vec = Wire(Vec(32, Bool()))
  val st_ready_vec = Wire(Vec(32, Bool()))
  for (i <- 0 until 32) {
    rs_ready_vec(i) := rs1_ready_array(i) === s_ready && rs2_ready_array(i) === s_ready && rd_ready_array(i) === s_ready &&
                      (OpType_array(i) === MATUOpType.mmul || OpType_array(i) === MATUOpType.mtest) &&
                      (state_array(i) === s_wait || state_array(i) === s_commit)
    val stMatchVec = dontTouch(Wire(Vec(32, Bool())))
    for (j <- 0 until 32) {
      stMatchVec(j) := OpType_array(j) === LSUOpType.sd && state_array(j) === s_wait && isAfter(history_ptr_array(i), history_ptr_array(j))
    }
    st_ready_vec(i) := rs2_ready_array(i) === s_ready && OpType_array(i) === LSUOpType.sd &&
                       (state_array(i) === s_wait || state_array(i) === s_commit) && !stMatchVec.asUInt.orR
  }

  io.fuIO.OpType_out := OpType_array(PriorityEncoder(rs_ready_vec.asUInt))
  io.fuIO.valid_out := rs_ready_vec.asUInt.orR && io.fuIO.ready_in &&
                       (state_array(PriorityEncoder(rs_ready_vec.asUInt)) === s_wait ||
                       state_array(PriorityEncoder(rs_ready_vec.asUInt)) === s_commit)
  io.fuIO.rs1_out := rs1_array(PriorityEncoder(rs_ready_vec.asUInt))
  io.fuIO.rs2_out := rs2_array(PriorityEncoder(rs_ready_vec.asUInt))
  io.fuIO.rd_out := rd_array(PriorityEncoder(rs_ready_vec.asUInt))

  io.stIO.raddr_out := rs2_array(PriorityEncoder(st_ready_vec.asUInt))
  io.stIO.roffset_out := rs2_offset_array(PriorityEncoder(st_ready_vec.asUInt))
  io.stIO.store_flag := st_ready_vec.asUInt.orR
  io.stIO.pc_out := pc_array(PriorityEncoder(st_ready_vec.asUInt))
  io.stIO.robIdx_out := robIdx_array(PriorityEncoder(st_ready_vec.asUInt)).value

  /** load instr WAW
   * Handle write after write hazard
   * */
  val ld_waw_vec = Wire(Vec(32, Bool()))
  for (i <- 0 until 32) {
    val ld_wawMatchVec = dontTouch(Wire(Vec(32, Bool())))
    for (j <- 0 until 32) {
      ld_wawMatchVec(j) := rd_array(j) === rd_array(i) && OpType_array(i) === LSUOpType.mld &&
                           (state_array(j) === s_wait || state_array(j) === s_commit) &&
                           (state_array(i) === s_commit || state_array(i) === s_wait) &&
                           ((OpType_array(j) === LSUOpType.mld && rd_offset_array(i) === rd_offset_array(j)) ||
                           (OpType_array(j) === MATUOpType.mmul ||
                           OpType_array(j) === MATUOpType.mtest)) &&
                           isAfter(history_ptr_array(i), history_ptr_array(j))
    }
    ld_waw_vec(i) := ld_wawMatchVec.asUInt.orR
  }

  /**  WAR
   * Handle write after read hazard
   * */
  val ld_war_vec = Wire(Vec(32, Bool()))
  for (i <- 0 until 32) {
    val ld_warMatchVec = dontTouch(Wire(Vec(32, Bool())))
    for (j <- 0 until 32) {
      ld_warMatchVec(j) := (rs1_array(j) === rd_array(i) || rs2_array(j) === rd_array(i)) &&
                           OpType_array(i) === LSUOpType.mld && (state_array(j) === s_wait || state_array(j) === s_commit) &&
                           (state_array(i) === s_commit || state_array(i) === s_wait) &&
                           ((OpType_array(j) === LSUOpType.mld && rd_offset_array(i) === rd_offset_array(j)) ||
                           (OpType_array(j) === MATUOpType.mmul ||
                           OpType_array(j) === MATUOpType.mtest)) &&
                           isAfter(history_ptr_array(i), history_ptr_array(j))
    }
    ld_war_vec(i) := ld_warMatchVec.asUInt.orR
  }

  // load write back
  val ld_wr_en_vec = Wire(Vec(32, Bool()))
  for (i <- 0 until 32) {
    ld_wr_en_vec(i) := !ld_waw_vec(i) && !ld_war_vec(i) && (state_array(i) === s_commit) && (OpType_array(i) === LSUOpType.mld)
  }
  io.ldOut.wen := ld_wr_en_vec.asUInt.orR
  io.ldOut.data_out := rd_value_array(PriorityEncoder(ld_wr_en_vec))
  io.ldOut.addr_out := rd_array(PriorityEncoder(ld_wr_en_vec))
  io.ldOut.offset_out := rd_offset_array(PriorityEncoder(ld_wr_en_vec))



  /** dequeue
   * state: s_retire -> s_idle
   *
   */
  when(state_array(deqPtr.value) === s_retire) {
    next_state_array(deqPtr.value) := s_idle
    deqPtr := deqPtr + 1.U
  }

}