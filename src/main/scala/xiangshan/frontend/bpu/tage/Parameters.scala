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

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import xiangshan.frontend.bpu.HasBpuParameters

case class TageParameters(
    TableInfos: Seq[Tuple3[Int, Int, Int]] = Seq(
      // Table size, history length, NumWay
      (1024, 4, 3),
      (1024, 9, 3),
      (1024, 17, 3),
      (1024, 31, 3),
      (1024, 58, 3),
      (1024, 109, 3),
      (1024, 211, 3),
      (1024, 407, 3)
    ),
    NumInternalBanks: Int = 2,
    TagWidth:         Int = 13,
    CtrWidth:         Int = 3,
    UsefulWidth:      Int = 2,
    WriteBufferSize:  Int = 4
) {}

trait HasTageParameters extends HasBpuParameters {
  def tageParameters: TageParameters = bpuParameters.tageParameters

  def TableInfos:       Seq[Tuple3[Int, Int, Int]] = tageParameters.TableInfos
  def NumInternalBanks: Int                        = tageParameters.NumInternalBanks
  def TagWidth:         Int                        = tageParameters.TagWidth
  def CtrWidth:         Int                        = tageParameters.CtrWidth
  def UsefulWidth:      Int                        = tageParameters.UsefulWidth
  def WriteBufferSize:  Int                        = tageParameters.WriteBufferSize

  def NumTables: Int = TableInfos.length
}
