/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
import xiangshan._

class RatReadPort(implicit p: Parameters) extends XSBundle {
  val addr = Input(UInt(5.W))
  val rdata = Output(UInt(PhyRegIdxWidth.W))
}

class RatWritePort(implicit p: Parameters) extends XSBundle {
  val wen = Input(Bool())
  val addr = Input(UInt(5.W))
  val wdata = Input(UInt(PhyRegIdxWidth.W))
}

class RenameTable(float: Boolean)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val redirect = Input(Bool())
    val flush = Input(Bool())
    val walkWen = Input(Bool())
    val readPorts = Vec({if(float) 4 else 3} * RenameWidth, new RatReadPort)
    val specWritePorts = Vec(CommitWidth, new RatWritePort)
    val archWritePorts = Vec(CommitWidth, new RatWritePort)
    val debug_rdata = Vec(32, Output(UInt(PhyRegIdxWidth.W)))
  })

  // speculative rename table
  val spec_table = RegInit(VecInit(Seq.tabulate(32)(i => i.U(PhyRegIdxWidth.W))))

  // arch state rename table
  val arch_table = RegInit(VecInit(Seq.tabulate(32)(i => i.U(PhyRegIdxWidth.W))))

  // When redirect happens (mis-prediction), don't update the rename table
  // However, when mis-prediction and walk happens at the same time, rename table needs to be updated
  for (w <- io.specWritePorts){
    when (w.wen && (!(io.redirect || io.flush) || io.walkWen)) {
      spec_table(w.addr) := w.wdata
    }
  }

  for((r, i) <- io.readPorts.zipWithIndex){
    r.rdata := spec_table(r.addr)
  }

  for(w <- io.archWritePorts){
    when(w.wen){ arch_table(w.addr) := w.wdata }
  }

  when (io.flush) {
    spec_table := arch_table
    // spec table needs to be updated when flushPipe
    for (w <- io.archWritePorts) {
      when(w.wen){ spec_table(w.addr) := w.wdata }
    }
  }

  io.debug_rdata := arch_table
}
