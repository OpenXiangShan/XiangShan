package cache.L1DTest

import cache.TLCTest.{AddrState, BigIntExtract, ScoreboardData, TLCAgent, TLCCallerTrans, TLCTrans, RandomSampleUtil}
import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import utils.{DebugIdentityNode, HoldUnless, XSDebug}
import xiangshan.HasXSLog
import xiangshan.cache.{DCache, DCacheLineReq, DCacheWordReq, MemoryOpConstants}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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
  val M_XRD = MemoryOpConstants.M_XRD.litValue()
  val M_XWR = MemoryOpConstants.M_XWR.litValue()
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


class CoreAgent(ID: Int, name: String, addrStateMap: mutable.Map[BigInt, AddrState], serialList: ArrayBuffer[(Int, TLCTrans)]
                , scoreboard: mutable.Map[BigInt, ScoreboardData], portNum: Int = 2)
               (implicit p: Parameters) extends TLCAgent(ID, name, addrStateMap, serialList, scoreboard) {
  private val loadPortsReqMessage = ArrayBuffer.fill[Option[LitDCacheWordReq]](portNum)(None)
  private val s0_loadTrans = ArrayBuffer.fill[Option[DCacheLoadCallerTrans]](portNum)(None)
  private val s1_loadTrans = ArrayBuffer.fill[Option[DCacheLoadCallerTrans]](portNum)(None)
  private val s2_loadTrans = ArrayBuffer.fill[Option[DCacheLoadCallerTrans]](portNum)(None)
  private var storePortReqMessage: Option[LitDCacheLineReq] = None

  val outerLoad: ListBuffer[DCacheLoadCallerTrans] = ListBuffer()
  val outerStore: ListBuffer[DCacheStoreCallerTrans] = ListBuffer()

  private val lsqWaiting: mutable.Queue[DCacheLoadCallerTrans] = mutable.Queue[DCacheLoadCallerTrans]()
  private val storeIdMap: mutable.Map[BigInt, DCacheStoreCallerTrans] = mutable.Map[BigInt, DCacheStoreCallerTrans]()

  def issueLoadReq(): Unit = {
    for (i <- 0 until portNum) {
      if (loadPortsReqMessage(i).isEmpty) {
        val nextLoad = outerLoad.find(l => !l.reqIssued.getOrElse(true))
        if (nextLoad.isDefined) {
          //alloc & issue
          s0_loadTrans(i) = nextLoad
          loadPortsReqMessage(i) = Some(nextLoad.get.issueReq())
        }
      }
    }
  }

  def peekLoadReq(i: Int): Option[LitDCacheWordReq] = {
    loadPortsReqMessage(i)
  }

  def fireLoadReq(i: Int): Unit = {
    loadPortsReqMessage(i) = None
  }

  def killPort(i: Int): Unit = {
    if (s1_loadTrans(i).isDefined) {
      val loadT = s1_loadTrans(i).get
      //drop killed store
      outerLoad -= loadT
    }
    s1_loadTrans(i) = None
  }

  def fireLoadResp(i: Int, resp: LitDCacheWordResp): Unit = {
    val loadT = s2_loadTrans(i).get
    val loadAddr = loadT.req.get.addr
    loadT.pairResp(resp)
    if (resp.miss == false) {
      val wc = wordInBlock(loadAddr)
      insertMaskedRead(loadAddr, dataConcatWord(0, resp.data, wc), genWordMaskInBlock(loadAddr, loadT.req.get.mask))
    }
    else if (resp.replay == true) {
      outerLoad -= loadT //drop it
      loadT.replay() //mark replay
      outerLoad.append(loadT) //pushpack
    }
    else {
      lsqWaiting.enqueue(loadT)
    }

  }

  def s1Paddr(i: Int): BigInt = {
    val l = s1_loadTrans(i)
    if (l.isDefined)
      l.get.req.get.addr
    else
      BigInt(0)
  }

  def fireLsqResp(resp: LitDCacheLineResp): Unit = {
    val respAddr = resp.paddr
    lsqWaiting.dequeueAll { q =>
      val loadAddr = q.req.get.addr
      val alignAddr = addrAlignBlock(loadAddr)
      if (alignAddr == respAddr) {
        q.pairLsqResp(resp)
        insertMaskedRead(loadAddr, resp.data, genWordMaskInBlock(loadAddr, q.req.get.mask))
        outerLoad -= q
        true
      }
      else
        false
    }
  }

  def issueStoreReq(): Unit = {
    if (storePortReqMessage.isEmpty) {
      val nextStore = outerStore.find(s => !s.reqIssued.getOrElse(true))
      if (nextStore.isDefined) {
        val allocId = (0 to 255).find(i => !storeIdMap.contains(BigInt(i)))
        if (allocId.isDefined) {
          //alloc & issue
          storePortReqMessage = Some(nextStore.get.issueReq(BigInt(allocId.get)))
          storeIdMap(allocId) = nextStore.get
        } else
          debugPrintln("cann't alloc ID for core store")
      }
    }
  }

  def peekStoreReq(): Option[LitDCacheLineReq] = {
    storePortReqMessage
  }

  def fireStoreReq(): Unit = {
    //free store req port
    storePortReqMessage = None
  }

  def fireStoreResp(resp: LitDCacheLineResp): Unit = {
    val storeId = resp.id
    val storeTrans = storeIdMap(storeId)
    storeTrans.pairResp(resp)
    //free resource
    storeIdMap.remove(storeId)
    //drop finished store
    outerStore -= storeTrans
    //confirm data write
    insertMaskedWrite(storeTrans.req.get.addr, resp.data, storeTrans.req.get.mask)
  }

  override def step(): Unit = {
    for (i <- 0 until portNum) {
      s2_loadTrans(i) = s1_loadTrans(i)
      s1_loadTrans(i) = s0_loadTrans(i)
      s0_loadTrans(i) = None
    }
    clock += 1
  }

  def addLoad(addr: BigInt): Unit = {
    //addr is aligned to block
    val loadT = new DCacheLoadCallerTrans()

    val wordCnt = rand.nextInt(8)

    val lgSize = rand.nextInt(4)
    val rsize = 1 << lgSize
    // addr must be aligned to size
    val offset = (rand.nextInt(8) >> lgSize) << lgSize
    val laddr = addr + wordCnt*8 + offset
    // generate mask from  raddr and rsize
    val mask = (BigInt(1) << rsize) - 1
    val wmask = mask << offset
    loadT.prepareLoad(addr,wmask)
    outerLoad.append(loadT)
  }

  def addStore(addr: BigInt): Unit = {
    //addr is aligned to block
    val storeT = new DCacheStoreCallerTrans()
    val blockMask = (BigInt(rand.nextInt().toLong) << 32) | BigInt(rand.nextInt().toLong)
    storeT.prepareStore(addr, randomBlockData(), blockMask)
    outerStore.append(storeT)
  }

}