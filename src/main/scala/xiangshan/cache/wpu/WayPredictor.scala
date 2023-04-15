//package xiangshan.cache
//
//import chipsalliance.rocketchip.config.Parameters
//import chisel3._
//import chisel3.util._
//import xiangshan._
//
//abstract class BaseWayPredictor(isIcache: Boolean = false)(implicit p:Parameters) extends XSModule {
//  def apply[T <: Data]
//  def pred(vaddr: UInt, en: Bool)
//  def update(vaddr: UInt, data: UInt,en: Bool)
//
//  val cacheParam:HasL1CacheParameters = if(isIcache) HasICacheParameters else HasDCacheParameters
//
//  val setSize = if (isIcache) cacheParam.nSets/2 else cacheParam.nSets
//
//  def get_wpu_idx(addr: UInt): UInt = {
//    if (isIcache) {
//      // NOTE: in icache, set[0] indicates which bank to choose
//      addr(cacheParam.untagBits - 1, cacheParam.blockOffBits + 1)
//    } else {
//      addr(untagBits - 1, blockOffBits)
//    }
//  }
//}
//
//class Mru[T <: L1CacheParameters] extends BaseWayPredictor[T]{
//  override def apply[T <: Data]: Unit = ???
//
//  override def pred(vaddr: UInt, en: Bool): T = ???
//
//  override def update(vaddr: UInt, data: T, en: Bool): Unit = ???
//}
