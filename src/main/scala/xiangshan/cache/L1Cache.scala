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

// See LICENSE.SiFive for license details.

package xiangshan.cache

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.{HasXSParameter, XSBundle, XSModule}

// this file contains common building blocks that can be shared by ICache and DCache
// this is the common parameter base for L1 ICache and L1 DCache
trait L1CacheParameters {
  def nSets:         Int
  def nWays:         Int
  def rowBits:       Int
  def blockBytes:    Int
}

trait HasL1CacheParameters extends HasXSParameter
  with MemoryOpConstants {
  val cacheParams: L1CacheParameters

  def nSets = cacheParams.nSets
  def nWays = cacheParams.nWays
  def blockBytes = cacheParams.blockBytes
  def blockBits = blockBytes * 8

  def idxBits = log2Up(cacheParams.nSets)
  def wayBits = log2Up(nWays)
  def blockOffBits = log2Up(cacheParams.blockBytes)

  def untagBits = blockOffBits + idxBits
  // 4K page
  def pgIdxBits = 12
  def pgUntagBits = untagBits min pgIdxBits
  def tagBits = PAddrBits - pgUntagBits

  // the basic unit at which we store contents
  // SRAM bank width
  def rowBits = cacheParams.rowBits
  def rowBytes = rowBits/8
  def rowOffBits = log2Up(rowBytes)
  // the number of rows in a block
  def blockRows = blockBytes / rowBytes

  // outer bus width
  def beatBits = l1BusDataWidth
  def beatBytes = beatBits / 8
  def refillCycles = blockBytes / beatBytes
  def beatOffBits = log2Up(beatBytes)

  // inner bus width(determined by XLEN)
  def wordBits = DataBits
  def wordBytes = wordBits / 8
  def wordOffBits = log2Up(wordBytes)
  // the number of words in a block
  def blockWords = blockBytes / wordBytes

  def idxMSB = untagBits-1
  def idxLSB = blockOffBits
  def offsetmsb = idxLSB-1
  def offsetlsb = wordOffBits

  def get_tag(addr: UInt) = (addr >> untagBits).asUInt()
  def get_idx(addr: UInt) = addr(untagBits-1, blockOffBits)
  def get_block(addr: UInt) = addr >> blockOffBits
  def get_block_addr(addr: UInt) = (addr >> blockOffBits) << blockOffBits

  def get_beat(addr: UInt) = addr(blockOffBits - 1, beatOffBits)
  def get_row(addr: UInt) = addr(blockOffBits - 1, rowOffBits)
  def get_word(addr: UInt) = addr(blockOffBits - 1, wordOffBits)

  def beatRows = beatBits/rowBits
  def rowWords = rowBits/wordBits

  def full_divide(a: Int, b: Int) = a >= b && isPow2(a / b)
}

abstract class L1CacheModule(implicit p: Parameters) extends XSModule
  with HasL1CacheParameters

abstract class L1CacheBundle(implicit p: Parameters) extends XSBundle
  with HasL1CacheParameters
