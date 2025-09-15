// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSBundle
import xiangshan.XSModule
import xiangshan.frontend.PrunedAddr

abstract class BpuBundle(implicit p: Parameters) extends XSBundle with HasBpuParameters

abstract class BpuModule(implicit p: Parameters) extends XSModule with HasBpuParameters

abstract class BasePredictor(implicit p: Parameters) extends BpuModule {
  val io: BasePredictorIO
}

abstract class BasePredictorIO(implicit p: Parameters) extends BpuBundle {
  // control
  val enable: Bool = Input(Bool())
  // predict stage control
  val stageCtrl: StageCtrl = Input(new StageCtrl)
  // predict request
  val startVAddr: PrunedAddr = Input(PrunedAddr(VAddrBits))
  // train
  val train: Valid[Train] = Input(Valid(new Train))

  val resetDone: Bool = Output(Bool())
}

// The abstract class is used to abstract the setIdx and tag from write requests for updating write buffer entries
abstract class WriteReqBundle(implicit p: Parameters) extends BpuBundle {
  val setIdx: UInt
  def tag: Option[UInt]            = None
  def cnt: Option[SaturateCounter] = None
}
