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
import xiangshan._
import utils.XSDebug

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

  // handle duplicate arch/spec write requirements
  val arch_cmp = Wire(MixedVec(List.tabulate(CommitWidth-1)(i => UInt((i+1).W))))
  val spec_cmp = Wire(MixedVec(List.tabulate(CommitWidth-1)(i => UInt((i+1).W))))

  for (i <- 1 until CommitWidth) {
    // compare archWritePorts and specWritePorts
    arch_cmp(i - 1) := Cat((0 until i).map(j => {
      io.archWritePorts(i).wen && io.archWritePorts(j).wen && io.archWritePorts(i).addr === io.archWritePorts(j).addr
    }))
    spec_cmp(i - 1) := Cat((0 until i).map(j => {
      io.specWritePorts(i).wen && io.specWritePorts(j).wen && io.specWritePorts(i).addr === io.specWritePorts(j).addr
    }))
  }

  // TODO this method is duplicate, which is also defined in AlternativeFreeList.scala
  def getCompareResult(m: MixedVec[UInt]): Vec[Bool] = {
    WireInit(VecInit(Seq.tabulate(CommitWidth){
      case last if (last == CommitWidth - 1) => true.B
      case i => !(Cat((i until (CommitWidth - 1)).map(j => m(j)(i))).orR)
    }))
  }

  val arch_is_last = getCompareResult(arch_cmp)
  val spec_is_last = getCompareResult(spec_cmp)

  val archUpdate = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))
  val specUpdate = WireInit(VecInit(Seq.fill(CommitWidth)(false.B)))

  // When redirect happens (mis-prediction), don't update the rename table
  // However, when mis-prediction and walk happens at the same time, rename table needs to be updated
  // spec table needs to be updated when flushPipe
  for ((w, i) <- io.specWritePorts.zipWithIndex){
    when (w.wen && (!(io.redirect || io.flush) || io.walkWen || io.flush)) {
      // spec_table(w.addr) := w.wdata
      specUpdate(i) := true.B
    }
  }

  // SPECulative table Write
  when (io.flush) {
    spec_table := arch_table
  }

  for (reg <- 0 until 32) {
    specUpdate.zipWithIndex.foreach { case (update, i) =>
      when (update && reg.U === io.specWritePorts(i).addr) {
        spec_table(io.specWritePorts(i).addr) := io.specWritePorts(i).wdata
      }
    }
  }

  // SPECulative table Read
  for(r <- io.readPorts){
    r.rdata := spec_table(r.addr)
  }

  // ARCHitecture table Write
  for((w, i) <- io.archWritePorts.zipWithIndex){
    when(w.wen){ 
      // arch_table(w.addr) := w.wdata 
      archUpdate(i) := true.B
    }
  }

  for (reg <- 0 until 32) {
    archUpdate.zipWithIndex.foreach { case (update, i) =>
      when (update && reg.U === io.archWritePorts(i).addr) {
        arch_table(io.archWritePorts(i).addr) := io.archWritePorts(i).wdata
      }
    }
  }

  // ARCHitecture table Read
  io.debug_rdata := arch_table

  XSDebug(p"redirect:${io.redirect},flush:${io.flush},walkWen:${io.walkWen}\n")
  XSDebug(p"[spec]WritePorts:" + io.specWritePorts.zipWithIndex.map{ 
    case (wp, idx) => p"($idx)wen:${wp.wen},addr:${Mux(wp.wen, wp.addr, 0.U)},data:${Mux(wp.wen, wp.wdata, 0.U)} " 
  }.reduceLeft(_ + _) + p"\n")
  XSDebug(p"[arch]WritePorts:" + io.archWritePorts.zipWithIndex.map{ 
    case (wp, idx) => p"($idx)wen:${wp.wen},addr:${Mux(wp.wen, wp.addr, 0.U)},data:${Mux(wp.wen, wp.wdata, 0.U)} " 
  }.reduceLeft(_ + _) + p"\n")
}
