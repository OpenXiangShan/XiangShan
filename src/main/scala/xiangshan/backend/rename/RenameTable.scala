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

package xiangshan.backend.rename

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utility.HasCircularQueuePtrHelper
import utility.ParallelPriorityMux
import utility.GatedValidRegNext
import utils.XSError
import xiangshan._

abstract class RegType
case object Reg_I extends RegType
case object Reg_F extends RegType
case object Reg_V extends RegType
case object Reg_V0 extends RegType
case object Reg_Vl extends RegType

class RatReadPort(implicit p: Parameters) extends XSBundle {
  val hold = Input(Bool())
  val addr = Input(UInt(6.W))
  val data = Output(UInt(PhyRegIdxWidth.W))
}

class RatWritePort(implicit p: Parameters) extends XSBundle {
  val wen = Bool()
  val addr = UInt(6.W)
  val data = UInt(PhyRegIdxWidth.W)
}

class RenameTable(reg_t: RegType)(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {

  // params alias
  private val numVecRegSrc = backendParams.numVecRegSrc
  private val numVecRatPorts = numVecRegSrc

  val readPortsNum = reg_t match {
    case Reg_I => 2
    case Reg_F => 3
    case Reg_V => numVecRatPorts // +1 ldest
    case Reg_V0 => 1
    case Reg_Vl => 1
  }
  val rdataNums = reg_t match {
    case Reg_I => 32
    case Reg_F => 32
    case Reg_V => 31 // no v0
    case Reg_V0 => 1 // v0
    case Reg_Vl => 1 // vl
  }
  val io = IO(new Bundle {
    val redirect = Input(Bool())
    val readPorts = Vec(readPortsNum * RenameWidth, new RatReadPort)
    val specWritePorts = Vec(RabCommitWidth, Input(new RatWritePort))
    val archWritePorts = Vec(RabCommitWidth, Input(new RatWritePort))
    val old_pdest = Vec(RabCommitWidth, Output(UInt(PhyRegIdxWidth.W)))
    val need_free = Vec(RabCommitWidth, Output(Bool()))
    val snpt = Input(new SnapshotPort)
    val diffWritePorts = if (backendParams.debugEn) Some(Vec(RabCommitWidth * MaxUopSize, Input(new RatWritePort))) else None
    val debug_rdata = if (backendParams.debugEn) Some(Vec(rdataNums, Output(UInt(PhyRegIdxWidth.W)))) else None
    val diff_rdata = if (backendParams.debugEn) Some(Vec(rdataNums, Output(UInt(PhyRegIdxWidth.W)))) else None
    val debug_v0 = if (backendParams.debugEn) reg_t match {
      case Reg_V0 => Some(Output(UInt(PhyRegIdxWidth.W)))
      case _ => None
    } else None
    val diff_v0 = if (backendParams.debugEn) reg_t match {
      case Reg_V0 => Some(Output(UInt(PhyRegIdxWidth.W)))
      case _ => None
    } else None
    val debug_vl = if (backendParams.debugEn) reg_t match {
      case Reg_Vl => Some(Output(UInt(PhyRegIdxWidth.W)))
      case _ => None
    } else None
    val diff_vl = if (backendParams.debugEn) reg_t match {
      case Reg_Vl => Some(Output(UInt(PhyRegIdxWidth.W)))
      case _ => None
    } else None
  })

  // speculative rename table
  val rename_table_init = reg_t match {
    case Reg_I => VecInit.fill    (IntLogicRegs)(0.U(PhyRegIdxWidth.W))
    case Reg_F => VecInit.tabulate(FpLogicRegs)(_.U(PhyRegIdxWidth.W))
    case Reg_V => VecInit.tabulate(VecLogicRegs)(_.U(PhyRegIdxWidth.W))
    case Reg_V0 => VecInit.tabulate(V0LogicRegs)(_.U(PhyRegIdxWidth.W))
    case Reg_Vl => VecInit.tabulate(VlLogicRegs)(_.U(PhyRegIdxWidth.W))
  }
  val spec_table = RegInit(rename_table_init)
  val spec_table_next = WireInit(spec_table)
  // arch state rename table
  val arch_table = RegInit(rename_table_init)
  val arch_table_next = WireDefault(arch_table)
  // old_pdest
  val old_pdest = RegInit(VecInit.fill(RabCommitWidth)(0.U(PhyRegIdxWidth.W)))
  val need_free = RegInit(VecInit.fill(RabCommitWidth)(false.B))

  // For better timing, we optimize reading and writing to RenameTable as follows:
  // (1) Writing at T0 will be actually processed at T1.
  // (2) Reading is synchronous now.
  // (3) RAddr at T0 will be used to access the table and get data at T0.
  // (4) WData at T0 is bypassed to RData at T1.
  val t1_redirect = GatedValidRegNext(io.redirect, false.B)
  val t1_raddr = io.readPorts.map(p => RegEnable(p.addr, !p.hold))
  val t1_rdata_use_t1_raddr = VecInit(t1_raddr.map(spec_table(_)))
  val t1_wSpec = RegNext(Mux(io.redirect, 0.U.asTypeOf(io.specWritePorts), io.specWritePorts))

  val t1_snpt = RegNext(io.snpt, 0.U.asTypeOf(io.snpt))

  val snapshots = SnapshotGenerator(spec_table, t1_snpt.snptEnq, t1_snpt.snptDeq, t1_redirect, t1_snpt.flushVec)

  // WRITE: when instruction commits or walking
  val t1_wSpec_addr = t1_wSpec.map(w => Mux(w.wen, UIntToOH(w.addr), 0.U))
  for ((next, i) <- spec_table_next.zipWithIndex) {
    val matchVec = t1_wSpec_addr.map(w => w(i))
    val wMatch = ParallelPriorityMux(matchVec.reverse, t1_wSpec.map(_.data).reverse)
    // When there's a flush, we use arch_table to update spec_table.
    next := Mux(
      t1_redirect,
      Mux(t1_snpt.useSnpt, snapshots(t1_snpt.snptSelect)(i), arch_table(i)),
      Mux(VecInit(matchVec).asUInt.orR, wMatch, spec_table(i))
    )
  }
  spec_table := spec_table_next

  // READ: decode-rename stage
  for ((r, i) <- io.readPorts.zipWithIndex) {
    val t0_bypass = io.specWritePorts.map(w => w.wen && Mux(r.hold, w.addr === t1_raddr(i), w.addr === r.addr))
    val t1_bypass = RegNext(Mux(io.redirect, 0.U.asTypeOf(VecInit(t0_bypass)), VecInit(t0_bypass)))
    val bypass_data = ParallelPriorityMux(t1_bypass.reverse, t1_wSpec.map(_.data).reverse)
    r.data := Mux(t1_bypass.asUInt.orR, bypass_data, t1_rdata_use_t1_raddr(i))
  }

  for ((w, i) <- io.archWritePorts.zipWithIndex) {
    when (w.wen) {
      arch_table_next(w.addr) := w.data
    }
    val arch_mask = VecInit.fill(PhyRegIdxWidth)(w.wen).asUInt
    old_pdest(i) :=
      MuxCase(arch_table(w.addr) & arch_mask,
              io.archWritePorts.take(i).reverse.map(x => (x.wen && x.addr === w.addr, x.data & arch_mask)))
  }
  arch_table := arch_table_next

  for (((old, free), i) <- (old_pdest zip need_free).zipWithIndex) {
    val hasDuplicate = old_pdest.take(i).map(_ === old)
    val blockedByDup = if (i == 0) false.B else VecInit(hasDuplicate).asUInt.orR
    free := VecInit(arch_table.map(_ =/= old)).asUInt.andR && !blockedByDup
  }

  io.old_pdest := old_pdest
  io.need_free := need_free
  io.debug_rdata.foreach(_ := arch_table.take(rdataNums))
  io.debug_v0.foreach(_ := arch_table(0))
  io.debug_vl.foreach(_ := arch_table(0))
  if (env.EnableDifftest || env.AlwaysBasicDiff) {
    val difftest_table = RegInit(rename_table_init)
    val difftest_table_next = WireDefault(difftest_table)

    for (w <- io.diffWritePorts.get) {
      when(w.wen) {
        difftest_table_next(w.addr) := w.data
      }
    }
    difftest_table := difftest_table_next

    io.diff_rdata.foreach(_ := difftest_table.take(rdataNums))
    io.diff_v0.foreach(_ := difftest_table(0))
    io.diff_vl.foreach(_ := difftest_table(0))
  }
  else {
    io.diff_rdata.foreach(_ := 0.U.asTypeOf(io.debug_rdata.get))
    io.diff_v0.foreach(_ := 0.U)
    io.diff_vl.foreach(_ := 0.U)
  }
}

class RenameTableWrapper(implicit p: Parameters) extends XSModule {

  // params alias
  private val numVecRegSrc = backendParams.numVecRegSrc
  private val numVecRatPorts = numVecRegSrc

  val io = IO(new Bundle() {
    val redirect = Input(Bool())
    val rabCommits = Input(new RabCommitIO)
    val diffCommits = if (backendParams.debugEn) Some(Input(new DiffCommitIO)) else None
    val intReadPorts = Vec(RenameWidth, Vec(2, new RatReadPort))
    val intRenamePorts = Vec(RenameWidth, Input(new RatWritePort))
    val fpReadPorts = Vec(RenameWidth, Vec(3, new RatReadPort))
    val fpRenamePorts = Vec(RenameWidth, Input(new RatWritePort))
    val vecReadPorts = Vec(RenameWidth, Vec(numVecRatPorts, new RatReadPort))
    val vecRenamePorts = Vec(RenameWidth, Input(new RatWritePort))
    val v0ReadPorts = Vec(RenameWidth, new RatReadPort)
    val v0RenamePorts = Vec(RenameWidth, Input(new RatWritePort))
    val vlReadPorts = Vec(RenameWidth, new RatReadPort)
    val vlRenamePorts = Vec(RenameWidth, Input(new RatWritePort))

    val int_old_pdest = Vec(RabCommitWidth, Output(UInt(PhyRegIdxWidth.W)))
    val fp_old_pdest = Vec(RabCommitWidth, Output(UInt(PhyRegIdxWidth.W)))
    val vec_old_pdest = Vec(RabCommitWidth, Output(UInt(PhyRegIdxWidth.W)))
    val v0_old_pdest = Vec(RabCommitWidth, Output(UInt(PhyRegIdxWidth.W)))
    val vl_old_pdest = Vec(RabCommitWidth, Output(UInt(PhyRegIdxWidth.W)))
    val int_need_free = Vec(RabCommitWidth, Output(Bool()))
    val snpt = Input(new SnapshotPort)

    // for debug printing
    val debug_int_rat     = if (backendParams.debugEn) Some(Vec(32, Output(UInt(PhyRegIdxWidth.W)))) else None
    val debug_fp_rat      = if (backendParams.debugEn) Some(Vec(32, Output(UInt(PhyRegIdxWidth.W)))) else None
    val debug_vec_rat     = if (backendParams.debugEn) Some(Vec(31, Output(UInt(PhyRegIdxWidth.W)))) else None
    val debug_v0_rat      = if (backendParams.debugEn) Some(Vec(1,Output(UInt(PhyRegIdxWidth.W)))) else None
    val debug_vl_rat      = if (backendParams.debugEn) Some(Vec(1,Output(UInt(PhyRegIdxWidth.W)))) else None

    val diff_int_rat     = if (backendParams.debugEn) Some(Vec(32, Output(UInt(PhyRegIdxWidth.W)))) else None
    val diff_fp_rat      = if (backendParams.debugEn) Some(Vec(32, Output(UInt(PhyRegIdxWidth.W)))) else None
    val diff_vec_rat     = if (backendParams.debugEn) Some(Vec(31, Output(UInt(PhyRegIdxWidth.W)))) else None
    val diff_v0_rat      = if (backendParams.debugEn) Some(Vec(1,Output(UInt(PhyRegIdxWidth.W)))) else None
    val diff_vl_rat      = if (backendParams.debugEn) Some(Vec(1,Output(UInt(PhyRegIdxWidth.W)))) else None
  })

  val intRat = Module(new RenameTable(Reg_I))
  val fpRat  = Module(new RenameTable(Reg_F))
  val vecRat = Module(new RenameTable(Reg_V))
  val v0Rat  = Module(new RenameTable(Reg_V0))
  val vlRat  = Module(new RenameTable(Reg_Vl))

  io.debug_int_rat .foreach(_ := intRat.io.debug_rdata.get)
  io.diff_int_rat  .foreach(_ := intRat.io.diff_rdata.get)
  intRat.io.readPorts <> io.intReadPorts.flatten
  intRat.io.redirect := io.redirect
  intRat.io.snpt := io.snpt
  io.int_old_pdest := intRat.io.old_pdest
  io.int_need_free := intRat.io.need_free
  val intDestValid = io.rabCommits.info.map(_.rfWen)
  for ((arch, i) <- intRat.io.archWritePorts.zipWithIndex) {
    arch.wen  := io.rabCommits.isCommit && io.rabCommits.commitValid(i) && intDestValid(i)
    arch.addr := io.rabCommits.info(i).ldest
    arch.data := io.rabCommits.info(i).pdest
    XSError(arch.wen && arch.addr === 0.U && arch.data =/= 0.U, "pdest for $0 should be 0\n")
  }
  for ((spec, i) <- intRat.io.specWritePorts.zipWithIndex) {
    spec.wen  := io.rabCommits.isWalk && io.rabCommits.walkValid(i) && intDestValid(i)
    spec.addr := io.rabCommits.info(i).ldest
    spec.data := io.rabCommits.info(i).pdest
    XSError(spec.wen && spec.addr === 0.U && spec.data =/= 0.U, "pdest for $0 should be 0\n")
  }
  for ((spec, rename) <- intRat.io.specWritePorts.zip(io.intRenamePorts)) {
    when (rename.wen) {
      spec.wen  := true.B
      spec.addr := rename.addr
      spec.data := rename.data
    }
  }
  if (backendParams.debugEn) {
    for ((diff, i) <- intRat.io.diffWritePorts.get.zipWithIndex) {
      diff.wen := io.diffCommits.get.isCommit && io.diffCommits.get.commitValid(i) && io.diffCommits.get.info(i).rfWen
      diff.addr := io.diffCommits.get.info(i).ldest
      diff.data := io.diffCommits.get.info(i).pdest
    }
  }

  // debug read ports for difftest
  io.debug_fp_rat.foreach(_ := fpRat.io.debug_rdata.get)
  io.diff_fp_rat .foreach(_ := fpRat.io.diff_rdata.get)
  fpRat.io.readPorts <> io.fpReadPorts.flatten
  fpRat.io.redirect := io.redirect
  fpRat.io.snpt := io.snpt
  io.fp_old_pdest := fpRat.io.old_pdest

  for ((arch, i) <- fpRat.io.archWritePorts.zipWithIndex) {
    arch.wen  := io.rabCommits.isCommit && io.rabCommits.commitValid(i) && io.rabCommits.info(i).fpWen
    arch.addr := io.rabCommits.info(i).ldest
    arch.data := io.rabCommits.info(i).pdest
  }
  for ((spec, i) <- fpRat.io.specWritePorts.zipWithIndex) {
    spec.wen  := io.rabCommits.isWalk && io.rabCommits.walkValid(i) && io.rabCommits.info(i).fpWen
    spec.addr := io.rabCommits.info(i).ldest
    spec.data := io.rabCommits.info(i).pdest
  }
  for ((spec, rename) <- fpRat.io.specWritePorts.zip(io.fpRenamePorts)) {
    when (rename.wen) {
      spec.wen  := true.B
      spec.addr := rename.addr
      spec.data := rename.data
    }
  }
  if (backendParams.debugEn) {
    for ((diff, i) <- fpRat.io.diffWritePorts.get.zipWithIndex) {
      diff.wen := io.diffCommits.get.isCommit && io.diffCommits.get.commitValid(i) && io.diffCommits.get.info(i).fpWen
      diff.addr := io.diffCommits.get.info(i).ldest
      diff.data := io.diffCommits.get.info(i).pdest
    }
  }

  // debug read ports for difftest
  io.debug_vec_rat    .foreach(_ := vecRat.io.debug_rdata.get)
  io.diff_vec_rat     .foreach(_ := vecRat.io.diff_rdata.get)
  vecRat.io.readPorts <> io.vecReadPorts.flatten
  vecRat.io.redirect := io.redirect
  vecRat.io.snpt := io.snpt
  io.vec_old_pdest := vecRat.io.old_pdest

  //TODO: RM the donTouch
  if(backendParams.debugEn) {
    dontTouch(vecRat.io)
  }
  for ((arch, i) <- vecRat.io.archWritePorts.zipWithIndex) {
    arch.wen  := io.rabCommits.isCommit && io.rabCommits.commitValid(i) && io.rabCommits.info(i).vecWen
    arch.addr := io.rabCommits.info(i).ldest
    arch.data := io.rabCommits.info(i).pdest
  }
  for ((spec, i) <- vecRat.io.specWritePorts.zipWithIndex) {
    spec.wen  := io.rabCommits.isWalk && io.rabCommits.walkValid(i) && io.rabCommits.info(i).vecWen
    spec.addr := io.rabCommits.info(i).ldest
    spec.data := io.rabCommits.info(i).pdest
  }
  for ((spec, rename) <- vecRat.io.specWritePorts.zip(io.vecRenamePorts)) {
    when (rename.wen) {
      spec.wen  := true.B
      spec.addr := rename.addr
      spec.data := rename.data
    }
  }
  if (backendParams.debugEn) {
    for ((diff, i) <- vecRat.io.diffWritePorts.get.zipWithIndex) {
      diff.wen := io.diffCommits.get.isCommit && io.diffCommits.get.commitValid(i) && io.diffCommits.get.info(i).vecWen
      diff.addr := io.diffCommits.get.info(i).ldest
      diff.data := io.diffCommits.get.info(i).pdest
    }
  }

  // debug read ports for difftest
  io.debug_v0_rat.foreach(_ := v0Rat.io.debug_rdata.get)
  io.diff_v0_rat.foreach(_ := v0Rat.io.diff_rdata.get)
  v0Rat.io.readPorts <> io.v0ReadPorts
  v0Rat.io.redirect := io.redirect
  v0Rat.io.snpt := io.snpt
  io.v0_old_pdest := v0Rat.io.old_pdest

  if (backendParams.debugEn) {
    dontTouch(v0Rat.io)
  }
  for ((arch, i) <- v0Rat.io.archWritePorts.zipWithIndex) {
    arch.wen := io.rabCommits.isCommit && io.rabCommits.commitValid(i) && io.rabCommits.info(i).v0Wen
    arch.addr := io.rabCommits.info(i).ldest
    arch.data := io.rabCommits.info(i).pdest
  }
  for ((spec, i) <- v0Rat.io.specWritePorts.zipWithIndex) {
    spec.wen := io.rabCommits.isWalk && io.rabCommits.walkValid(i) && io.rabCommits.info(i).v0Wen
    spec.addr := io.rabCommits.info(i).ldest
    spec.data := io.rabCommits.info(i).pdest
  }
  for ((spec, rename) <- v0Rat.io.specWritePorts.zip(io.v0RenamePorts)) {
    when(rename.wen) {
      spec.wen := true.B
      spec.addr := rename.addr
      spec.data := rename.data
    }
  }
  if (backendParams.debugEn) {
    for ((diff, i) <- v0Rat.io.diffWritePorts.get.zipWithIndex) {
      diff.wen := io.diffCommits.get.isCommit && io.diffCommits.get.commitValid(i) && io.diffCommits.get.info(i).v0Wen
      diff.addr := io.diffCommits.get.info(i).ldest
      diff.data := io.diffCommits.get.info(i).pdest
    }
  }

  // debug read ports for difftest
  io.debug_vl_rat.foreach(_ := vlRat.io.debug_rdata.get)
  io.diff_vl_rat.foreach(_ := vlRat.io.diff_rdata.get)
  vlRat.io.readPorts <> io.vlReadPorts
  vlRat.io.redirect := io.redirect
  vlRat.io.snpt := io.snpt
  io.vl_old_pdest := vlRat.io.old_pdest

  if (backendParams.debugEn) {
    dontTouch(vlRat.io)
  }
  for ((arch, i) <- vlRat.io.archWritePorts.zipWithIndex) {
    arch.wen := io.rabCommits.isCommit && io.rabCommits.commitValid(i) && io.rabCommits.info(i).vlWen
    arch.addr := io.rabCommits.info(i).ldest
    arch.data := io.rabCommits.info(i).pdest
  }
  for ((spec, i) <- vlRat.io.specWritePorts.zipWithIndex) {
    spec.wen := io.rabCommits.isWalk && io.rabCommits.walkValid(i) && io.rabCommits.info(i).vlWen
    spec.addr := io.rabCommits.info(i).ldest
    spec.data := io.rabCommits.info(i).pdest
  }
  for ((spec, rename) <- vlRat.io.specWritePorts.zip(io.vlRenamePorts)) {
    when(rename.wen) {
      spec.wen := true.B
      spec.addr := rename.addr
      spec.data := rename.data
    }
  }
  if (backendParams.debugEn) {
    for ((diff, i) <- vlRat.io.diffWritePorts.get.zipWithIndex) {
      diff.wen := io.diffCommits.get.isCommit && io.diffCommits.get.commitValid(i) && io.diffCommits.get.info(i).vlWen
      diff.addr := io.diffCommits.get.info(i).ldest
      diff.data := io.diffCommits.get.info(i).pdest
    }
  }
}
