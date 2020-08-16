// See LICENSE.SiFive for license details.

package xiangshan.cache

import chisel3._
import chisel3.util._
import xiangshan.{HasXSParameter, XSBundle, XSModule}

// this file contains common building blocks that can be shared by ICache and DCache
// this is the common parameter base for L1 ICache and L1 DCache
trait L1CacheParameters {
  def nSets:         Int
  def nWays:         Int
  def rowBits:       Int
  def nTLBEntries:   Int
  def blockBytes:    Int
}

trait HasL1CacheParameters extends HasXSParameter
  with MemoryOpConstants {
  val cacheParams: L1CacheParameters

  def nSets = cacheParams.nSets
  def blockOffBits = log2Up(cacheParams.blockBytes)
  def idxBits = log2Up(cacheParams.nSets)
  def untagBits = blockOffBits + idxBits
  // 4K page
  def pgIdxBits = 12
  def pgUntagBits = untagBits min pgIdxBits

  // L1 cache are all physically tagged cache
  def tagBits = PAddrBits - pgUntagBits
  def nWays = cacheParams.nWays
  def wayBits = log2Up(nWays)
  def rowBits = cacheParams.rowBits
  def rowBytes = rowBits/8
  def rowOffBits = log2Up(rowBytes)
  def nTLBEntries = cacheParams.nTLBEntries

  def cacheDataBits = l1BusDataWidth
  def cacheDataBytes = cacheDataBits / 8
  def cacheDataBeats = (cacheParams.blockBytes * 8) / cacheDataBits
  def refillCycles = cacheDataBeats
}

abstract class L1CacheModule extends XSModule
  with HasL1CacheParameters

abstract class L1CacheBundle extends XSBundle
  with HasL1CacheParameters
