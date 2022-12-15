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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.{ParallelPriorityMux, XSError}
import xiangshan._

abstract class RegType
case object Reg_I extends RegType
case object Reg_F extends RegType
case object Reg_V extends RegType

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

class RenameTable(reg_t: RegType)(implicit p: Parameters) extends XSModule {
  val readPortsNum = reg_t match {
    case Reg_I => 3
    case Reg_F => 4
    case Reg_V => 5
  }
  val io = IO(new Bundle {
    val redirect = Input(Bool())
    val readPorts = Vec(readPortsNum * RenameWidth, new RatReadPort)
    val specWritePorts = Vec(CommitWidth, Input(new RatWritePort))
    val archWritePorts = Vec(CommitWidth, Input(new RatWritePort))
    val debug_rdata = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_vconfig = reg_t match { // vconfig is implemented as int reg[32]
      case Reg_I => Some(Output(UInt(PhyRegIdxWidth.W)))
      case _     => None
    }
  })

  // speculative rename table
  // fp and vec share the same free list, so the first init value of vecRAT is 32
  val rename_table_init = reg_t match {
    case Reg_I => VecInit.fill    (33)(0.U(PhyRegIdxWidth.W))
    case Reg_F => VecInit.tabulate(32)(_.U(PhyRegIdxWidth.W))
    case Reg_V => VecInit.tabulate(32)(x => (x + 32).U(PhyRegIdxWidth.W))
  }
  val spec_table = RegInit(rename_table_init)
  val spec_table_next = WireInit(spec_table)
  // arch state rename table
  val arch_table = RegInit(rename_table_init)
  val arch_table_next = WireDefault(arch_table)

  // For better timing, we optimize reading and writing to RenameTable as follows:
  // (1) Writing at T0 will be actually processed at T1.
  // (2) Reading is synchronous now.
  // (3) RAddr at T0 will be used to access the table and get data at T0.
  // (4) WData at T0 is bypassed to RData at T1.
  val t1_redirect = RegNext(io.redirect, false.B)
  val t1_rdata = io.readPorts.map(p => RegNext(Mux(p.hold, p.data, spec_table_next(p.addr))))
  val t1_raddr = io.readPorts.map(p => RegEnable(p.addr, !p.hold))
  val t1_wSpec = RegNext(Mux(io.redirect, 0.U.asTypeOf(io.specWritePorts), io.specWritePorts))

  // WRITE: when instruction commits or walking
  val t1_wSpec_addr = t1_wSpec.map(w => Mux(w.wen, UIntToOH(w.addr), 0.U))
  for ((next, i) <- spec_table_next.zipWithIndex) {
    val matchVec = t1_wSpec_addr.map(w => w(i))
    val wMatch = ParallelPriorityMux(matchVec.reverse, t1_wSpec.map(_.data).reverse)
    // When there's a flush, we use arch_table to update spec_table.
    next := Mux(t1_redirect, arch_table(i), Mux(VecInit(matchVec).asUInt.orR, wMatch, spec_table(i)))
  }
  spec_table := spec_table_next

  // READ: decode-rename stage
  for ((r, i) <- io.readPorts.zipWithIndex) {
    // We use two comparisons here because r.hold has bad timing but addrs have better timing.
    val t0_bypass = io.specWritePorts.map(w => w.wen && Mux(r.hold, w.addr === t1_raddr(i), w.addr === r.addr))
    val t1_bypass = RegNext(Mux(io.redirect, 0.U.asTypeOf(VecInit(t0_bypass)), VecInit(t0_bypass)))
    val bypass_data = ParallelPriorityMux(t1_bypass.reverse, t1_wSpec.map(_.data).reverse)
    r.data := Mux(t1_bypass.asUInt.orR, bypass_data, t1_rdata(i))
  }

  for (w <- io.archWritePorts) {
    when (w.wen) {
      arch_table_next(w.addr) := w.data
    }
  }
  arch_table := arch_table_next

  io.debug_rdata := arch_table.take(32)
  io.debug_vconfig match {
    case None => Unit
    case x    => x.get := arch_table.last
  }
}

class RenameTableWrapper(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Input(Bool())
    val robCommits = Input(new RobCommitIO)
    val intReadPorts = Vec(RenameWidth, Vec(3, new RatReadPort))
    val intRenamePorts = Vec(RenameWidth, Input(new RatWritePort))
    val fpReadPorts = Vec(RenameWidth, Vec(4, new RatReadPort))
    val fpRenamePorts = Vec(RenameWidth, Input(new RatWritePort))
    val vecReadPorts = Vec(RenameWidth, Vec(5, new RatReadPort))
    val vecRenamePorts = Vec(RenameWidth, Input(new RatWritePort))
    // for debug printing
    val debug_int_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_vec_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_vconfig_rat = Output(UInt(PhyRegIdxWidth.W))
  })

  val intRat = Module(new RenameTable(Reg_I))
  val fpRat  = Module(new RenameTable(Reg_F))
  val vecRat = Module(new RenameTable(Reg_V))

  io.debug_int_rat := intRat.io.debug_rdata
  io.debug_vconfig_rat := intRat.io.debug_vconfig.get
  intRat.io.readPorts <> io.intReadPorts.flatten
  intRat.io.redirect := io.redirect
  val intDestValid = io.robCommits.info.map(_.rfWen)
  for ((arch, i) <- intRat.io.archWritePorts.zipWithIndex) {
    arch.wen  := io.robCommits.isCommit && io.robCommits.commitValid(i) && intDestValid(i)
    arch.addr := io.robCommits.info(i).ldest
    arch.data := io.robCommits.info(i).pdest
    XSError(arch.wen && arch.addr === 0.U && arch.data =/= 0.U, "pdest for $0 should be 0\n")
  }
  for ((spec, i) <- intRat.io.specWritePorts.zipWithIndex) {
    spec.wen  := io.robCommits.isWalk && io.robCommits.walkValid(i) && intDestValid(i)
    spec.addr := io.robCommits.info(i).ldest
    spec.data := io.robCommits.info(i).pdest
    XSError(spec.wen && spec.addr === 0.U && spec.data =/= 0.U, "pdest for $0 should be 0\n")
  }
  for ((spec, rename) <- intRat.io.specWritePorts.zip(io.intRenamePorts)) {
    when (rename.wen) {
      spec.wen  := true.B
      spec.addr := rename.addr
      spec.data := rename.data
    }
  }

  // debug read ports for difftest
  io.debug_fp_rat := fpRat.io.debug_rdata
  fpRat.io.readPorts <> io.fpReadPorts.flatten
  fpRat.io.redirect := io.redirect
  for ((arch, i) <- fpRat.io.archWritePorts.zipWithIndex) {
    arch.wen  := io.robCommits.isCommit && io.robCommits.commitValid(i) && io.robCommits.info(i).fpWen
    arch.addr := io.robCommits.info(i).ldest
    arch.data := io.robCommits.info(i).pdest
  }
  for ((spec, i) <- fpRat.io.specWritePorts.zipWithIndex) {
    spec.wen  := io.robCommits.isWalk && io.robCommits.walkValid(i) && io.robCommits.info(i).fpWen
    spec.addr := io.robCommits.info(i).ldest
    spec.data := io.robCommits.info(i).pdest
  }
  for ((spec, rename) <- fpRat.io.specWritePorts.zip(io.fpRenamePorts)) {
    when (rename.wen) {
      spec.wen  := true.B
      spec.addr := rename.addr
      spec.data := rename.data
    }
  }

  // debug read ports for difftest
  io.debug_vec_rat := vecRat.io.debug_rdata
  vecRat.io.readPorts <> io.vecReadPorts.flatten
  vecRat.io.redirect := io.redirect
  for ((arch, i) <- vecRat.io.archWritePorts.zipWithIndex) {
    arch.wen  := io.robCommits.isCommit && io.robCommits.commitValid(i) && io.robCommits.info(i).vecWen
    arch.addr := io.robCommits.info(i).ldest
    arch.data := io.robCommits.info(i).pdest
  }
  for ((spec, i) <- vecRat.io.specWritePorts.zipWithIndex) {
    spec.wen  := io.robCommits.isWalk && io.robCommits.walkValid(i) && io.robCommits.info(i).vecWen
    spec.addr := io.robCommits.info(i).ldest
    spec.data := io.robCommits.info(i).pdest
  }
  for ((spec, rename) <- vecRat.io.specWritePorts.zip(io.vecRenamePorts)) {
    when (rename.wen) {
      spec.wen  := true.B
      spec.addr := rename.addr
      spec.data := rename.data
    }
  }

}
