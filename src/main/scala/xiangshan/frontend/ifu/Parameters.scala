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

package xiangshan.frontend.ifu
import chisel3._
import chisel3.util._
import xiangshan.frontend.HasFrontendParameters

case class IfuParameters(
    PcCutPoint: Option[Int] = None // default cut at lower VAddrBits/4
) {}

trait HasIfuParameters extends HasFrontendParameters {
  def ifuParameters: IfuParameters = frontendParameters.ifuParameters

  def ICacheLineBytes: Int = frontendParameters.icacheParameters.blockBytes
  // equal lower_result overflow bit
  def PcCutPoint:      Int = ifuParameters.PcCutPoint.getOrElse((VAddrBits / 4) - 1)
  def IfuAlignWidth:   Int = frontendParameters.ibufferParameters.NumWriteBank
  def FastFetchWidth:  Int = 4                 // 表示参与快速取指的范围
  def FastFetchMaxNum: Int = IfuAlignWidth - 1 // 表示快速取指的最大数量
  def FastFetchBytes:  Int = FastFetchWidth * 2

  require(
    FastFetchWidth == IfuAlignWidth,
    s"FastFetchWidth($FastFetchWidth) should equal IfuAlignWidth($IfuAlignWidth)"
  )
  require(
    ICacheLineBytes >= FastFetchWidth * 2 && ICacheLineBytes % (FastFetchWidth * 2) == 0,
    s"ICacheLineBytes($ICacheLineBytes) should be a multiple of ${FastFetchWidth * 2}"
  )

  require(PcCutPoint > 0 && PcCutPoint < VAddrBits, s"PcCutPoint($PcCutPoint) must be in range (0, $VAddrBits)")
}
