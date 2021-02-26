package cache.L1DTest

import cache.TLCTest.{TLCCallerTrans, TLCTrans}
import xiangshan.cache.MemoryOpConstants

class LitDCacheWordReq(
                        val cmd: BigInt,
                        val addr: BigInt,
                        var data: BigInt = 0,
                        val mask: BigInt,
                        var id: BigInt = 0,
                      ) {
}

class LitDCacheWordResp(
                         val data: BigInt,
                         val miss: Boolean,
                         val replay: Boolean,
                         var id: BigInt = 0,
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
  val M_XA_SWAP: BigInt = MemoryOpConstants.M_XA_SWAP.litValue()
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

  def issueLoadReq(): LitDCacheWordReq = {
    reqIssued = Some(true)
    startTimer()
    req.get
  }

  def replayLoad(): Unit = {
    reqIssued = Some(false)
    resetTimer()
  }

  def pairLoadResp(inResp: LitDCacheWordResp): Unit = {
    resp = Some(inResp)
    resetTimer()
    if (inResp.miss && !inResp.replay) { //if it will be placed into lsq
      startTimer()
    }
  }

  def pairLsqResp(inResp: LitDCacheLineResp): Unit = {
    lsqResp = Some(inResp)
    resetTimer()
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

  def issueStoreReq(allocId: BigInt): LitDCacheLineReq = {
    req.get.id = allocId
    reqIssued = Some(true)
    startTimer()
    req.get
  }

  def pairStoreResp(inResp: LitDCacheLineResp): Unit = {
    resp = Some(inResp)
    resetTimer()
  }

}

class DCacheAMOTrans extends TLCTrans with LitMemOp {
  var req: Option[LitDCacheWordReq] = None
  var resp: Option[LitDCacheWordResp] = None
}

class DCacheAMOCallerTrans extends DCacheAMOTrans with TLCCallerTrans {
  var reqIssued: Option[Boolean] = None

  def prepareAMOSwap(addr: BigInt, data: BigInt, mask: BigInt): Unit = {
    req = Some(
      new LitDCacheWordReq(
        cmd = M_XA_SWAP,
        addr = addr,
        data = data,
        mask = mask,
      )
    )
    reqIssued = Some(false)
  }

  def issueReq(allodId: BigInt = 0): LitDCacheWordReq = {
    req.get.id = allodId
    reqIssued = Some(true)
    startTimer()
    req.get
  }

  def pairResp(inResp: LitDCacheWordResp): Unit = {
    resp = Some(inResp)
    resetTimer()
  }
}

