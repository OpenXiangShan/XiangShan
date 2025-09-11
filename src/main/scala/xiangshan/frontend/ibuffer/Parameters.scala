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

package xiangshan.frontend.ibuffer

import xiangshan.frontend.HasFrontendParameters

// For users: these are default IBuffer parameters set by dev, do not change them here,
// use top-level Parameters.scala instead.
case class IBufferParameters(
    Size:         Int = 48,
    NumWriteBank: Int = 4, // should divide Size, used in pre-alignment in Ifu for better timing
    NumReadBank:  Int = 8  // should divide Size, also should >= DecodeWidth
) {
  require(Size % NumWriteBank == 0, s"IBuffer NumWriteBank($NumWriteBank) should divide Size($Size)")
  require(Size % NumReadBank == 0, s"IBuffer NumReadBank($NumReadBank) should divide Size($Size)")
}

trait HasIBufferParameters extends HasFrontendParameters {
  def ibufferParameters: IBufferParameters = frontendParameters.ibufferParameters

  def Size:         Int = ibufferParameters.Size
  def NumWriteBank: Int = ibufferParameters.NumWriteBank
  def NumReadBank:  Int = ibufferParameters.NumReadBank

  def ReadBankSize:  Int = Size / NumReadBank
  def WriteBankSize: Int = Size / NumWriteBank

  def MaxBypassNum: Int = DecodeWidth + NumWriteBank

  // IBufferEnqueueWidth inherited from HasFrontendParameters
  def EnqueueWidth: Int = IBufferEnqueueWidth
}
