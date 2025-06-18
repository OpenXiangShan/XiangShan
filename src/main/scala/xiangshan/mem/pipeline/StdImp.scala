/***************************************************************************************
* Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.Bundles.{MemExuInput, MemExuOutput}
import xiangshan.backend.datapath.{NewPipelineConnect, NewPipelineConnectPipe}
import xiangshan.mem.Bundles._

class StdIO()(implicit p: Parameters, params: MemUnitParams) extends MemUnitIO with HasMemBlockParameters {
  // from
  val fromBackend = new Bundle() {
    val issue = Flipped(DecoupledIO(new LsPipelineBundle))
  }

  // to
  val toBackend = new Bundle() {
    val writeback = DecoupledIO(new LsPipelineBundle)
  }
}

class StdImp(override val wrapper: MemUnit)(implicit p: Parameters, params: MemUnitParams) extends MemUnitImp(wrapper) {
  /**
    * --------------------------------------------------------------------
    * IOs
    * --------------------------------------------------------------------
    */
  io.suggestName("none")
  override lazy val io = IO(new StdIO).suggestName("io")

  NewPipelineConnect.connect(
    left = io.fromBackend.issue,
    right = io.toBackend.writeback,
    rightOutFire = io.toBackend.writeback.fire,
    isFlush = false.B // ignore isFlush is OK ?
  )
}
