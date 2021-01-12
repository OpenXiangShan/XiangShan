package cache.L1DTest

import cache.TLCTest.{TLCCallerTrans, TLCTrans}
import xiangshan.cache.MemoryOpConstants

class LitDCacheWordReq(
                        val cmd: BigInt,
                        val addr: BigInt,
                        val mask: BigInt,
                      ) {
}

class LitDCacheWordResp(
                         val data: BigInt,
                         val miss: Boolean,
                         val replay: Boolean,
                       ) {

}

class LitDCacheLineReq(
                        val cmd: BigInt,
                        val addr: BigInt,
                        val data: BigInt,
                        val mask: BigInt,
                        var id: BigInt = 0
                      ) {
}

class LitDCacheLineResp(
                         val data: BigInt,
                         val paddr: BigInt,
                         val id: BigInt,
                       ) {

}

trait LitMemOp {
  val M_XRD: BigInt = MemoryOpConstants.M_XRD.litValue()
  val M_XWR: BigInt = MemoryOpConstants.M_XWR.litValue()
}

class DCacheLoadTrans extends TLCTrans with LitMemOp {
  var req: Option[LitDCacheWordReq] = None
  var resp: Option[LitDCacheWordResp] = None
  var lsqResp: Option[LitDCacheLineResp] = None
}

class DCacheLoadCallerTrans extends DCacheLoadTrans with TLCCallerTrans {
  var reqIssued: Option[Boolean] = None

  def prepareLoad(addr: BigInt, mask: BigInt): Unit = {
    req = Some(
      new LitDCacheWordReq(
        cmd = M_XRD,
        addr = addr,
        mask = mask,
      )
    )
    reqIssued = Some(false)
  }

  def issueReq(): LitDCacheWordReq = {
    reqIssued = Some(true)
    req.get
  }

  def replay(): Unit = {
    reqIssued = Some(false)
  }

  def pairResp(inResp: LitDCacheWordResp): Unit = {
    resp = Some(inResp)
  }

  def pairLsqResp(inResp: LitDCacheLineResp): Unit = {
    lsqResp = Some(inResp)
  }
}

class DCacheStoreTrans extends TLCTrans with LitMemOp {
  var req: Option[LitDCacheLineReq] = None
  var resp: Option[LitDCacheLineResp] = None
}

class DCacheStoreCallerTrans extends DCacheStoreTrans with TLCCallerTrans {
  var reqIssued: Option[Boolean] = None

  def prepareStore(addr: BigInt, data: BigInt, mask: BigInt): Unit = {
    req = Some(
      new LitDCacheLineReq(
        cmd = M_XWR,
        addr = addr,
        data = data,
        mask = mask,
      )
    )
    reqIssued = Some(false)
  }

  def issueReq(allocId: BigInt): LitDCacheLineReq = {
    req.get.id = allocId
    reqIssued = Some(true)
    req.get
  }

  def pairResp(inResp: LitDCacheLineResp): Unit = {
    resp = Some(inResp)
  }

}

