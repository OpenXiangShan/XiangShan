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

class RatReadPort(implicit p: Parameters) extends XSBundle {
  val hold = Input(Bool())
  val addr = Input(UInt(5.W))
  val data = Output(UInt(PhyRegIdxWidth.W))
}

class RatWritePort(implicit p: Parameters) extends XSBundle {
  val wen = Bool()
  val addr = UInt(5.W)
  val data = UInt(PhyRegIdxWidth.W)
}

class RenameTable(float: Boolean)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val readPorts = Vec({if(float) 4 else 3} * RenameWidth, new RatReadPort)
    val specWritePorts = Vec(CommitWidth, Input(new RatWritePort))
    val archWritePorts = Vec(CommitWidth, Input(new RatWritePort))
    val debug_rdata = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
  })

  // speculative rename table
  val spec_table = RegInit(VecInit(Seq.tabulate(32)(i => i.U(PhyRegIdxWidth.W))))
  val spec_table_next = WireInit(spec_table)
  // arch state rename table
  val arch_table = RegInit(VecInit(Seq.tabulate(32)(i => i.U(PhyRegIdxWidth.W))))

  // For better timing, we optimize reading and writing to RenameTable as follows:
  // (1) Writing at T0 will be actually processed at T1.
  // (2) Reading is synchronous now.
  // (3) RAddr at T0 will be used to access the table and get data at T0.
  // (4) WData at T0 is bypassed to RData at T1.
  val t1_rdata = io.readPorts.map(p => RegNext(Mux(p.hold, p.data, spec_table_next(p.addr))))
  val t1_raddr = io.readPorts.map(p => RegEnable(p.addr, !p.hold))
  val t1_wSpec = RegNext(io.specWritePorts)

  // WRITE: when instruction commits or walking
  val t1_wSpec_addr = t1_wSpec.map(w => Mux(w.wen, UIntToOH(w.addr), 0.U))
  for ((next, i) <- spec_table_next.zipWithIndex) {
    val matchVec = t1_wSpec_addr.map(w => w(i))
    val wMatch = ParallelPriorityMux(matchVec.reverse, t1_wSpec.map(_.data).reverse)
    // When there's a flush, we use arch_table to update spec_table.
    next := Mux(VecInit(matchVec).asUInt.orR, wMatch, spec_table(i))
  }
  spec_table := spec_table_next

  // READ: decode-rename stage
  for ((r, i) <- io.readPorts.zipWithIndex) {
    // We use two comparisons here because r.hold has bad timing but addrs have better timing.
    val t0_bypass = io.specWritePorts.map(w => w.wen && Mux(r.hold, w.addr === t1_raddr(i), w.addr === r.addr))
    val t1_bypass = RegNext(VecInit(t0_bypass))
    val bypass_data = ParallelPriorityMux(t1_bypass.reverse, t1_wSpec.map(_.data).reverse)
    r.data := Mux(t1_bypass.asUInt.orR, bypass_data, t1_rdata(i))
  }

  for (w <- io.archWritePorts) {
    when (w.wen) {
      arch_table(w.addr) := w.data
    }
  }

  io.debug_rdata := arch_table
}

class RenameTableWrapper(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val robCommits = Flipped(new RobCommitIO)
    val intReadPorts = Vec(RenameWidth, Vec(3, new RatReadPort))
    val intRenamePorts = Vec(RenameWidth, Input(new RatWritePort))
    val fpReadPorts = Vec(RenameWidth, Vec(4, new RatReadPort))
    val fpRenamePorts = Vec(RenameWidth, Input(new RatWritePort))
    // for debug printing
    val debug_int_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
    val debug_fp_rat = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
  })

  val intRat = Module(new RenameTable(float = false))
  val fpRat = Module(new RenameTable(float = true))

  intRat.io.debug_rdata <> io.debug_int_rat
  intRat.io.readPorts <> io.intReadPorts.flatten
  val intDestValid = io.robCommits.info.map(_.rfWen)
  for ((arch, i) <- intRat.io.archWritePorts.zipWithIndex) {
    arch.wen  := !io.robCommits.isWalk && io.robCommits.valid(i) && intDestValid(i)
    arch.addr := io.robCommits.info(i).ldest
    arch.data := io.robCommits.info(i).pdest
    XSError(arch.wen && arch.addr === 0.U && arch.data =/= 0.U, "pdest for $0 should be 0\n")
  }
  for ((spec, i) <- intRat.io.specWritePorts.zipWithIndex) {
    spec.wen  := io.robCommits.isWalk && io.robCommits.valid(i) && intDestValid(i)
    spec.addr := io.robCommits.info(i).ldest
    spec.data := io.robCommits.info(i).old_pdest
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
  fpRat.io.debug_rdata <> io.debug_fp_rat
  fpRat.io.readPorts <> io.fpReadPorts.flatten
  for ((arch, i) <- fpRat.io.archWritePorts.zipWithIndex) {
    arch.wen  := !io.robCommits.isWalk && io.robCommits.valid(i) && io.robCommits.info(i).fpWen
    arch.addr := io.robCommits.info(i).ldest
    arch.data := io.robCommits.info(i).pdest
  }
  for ((spec, i) <- fpRat.io.specWritePorts.zipWithIndex) {
    spec.wen  := io.robCommits.isWalk && io.robCommits.valid(i) && io.robCommits.info(i).fpWen
    spec.addr := io.robCommits.info(i).ldest
    spec.data := io.robCommits.info(i).old_pdest
  }
  for ((spec, rename) <- fpRat.io.specWritePorts.zip(io.fpRenamePorts)) {
    when (rename.wen) {
      spec.wen  := true.B
      spec.addr := rename.addr
      spec.data := rename.data
    }
  }

}
