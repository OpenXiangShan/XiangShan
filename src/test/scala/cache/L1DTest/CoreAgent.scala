package cache.L1DTest

import cache.TLCTest.{AddrState, ScoreboardData, TLCAgent, TLCTrans}
import chipsalliance.rocketchip.config.Parameters

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

//!!! for coreAgent, load resp must be fired before store resp
class CoreAgent(ID: Int, name: String, addrStateMap: mutable.Map[BigInt, AddrState], serialList: ArrayBuffer[(Int, TLCTrans)]
                , scoreboard: mutable.Map[BigInt, ScoreboardData], portNum: Int = 2)
               (implicit p: Parameters) extends TLCAgent(ID, name, addrStateMap, serialList, scoreboard) with LitMemOp {
  private val loadPortsReqMessage = ArrayBuffer.fill[Option[LitDCacheWordReq]](portNum)(None)
  private val s0_loadTrans = ArrayBuffer.fill[Option[DCacheLoadCallerTrans]](portNum)(None)
  private val s1_loadTrans = ArrayBuffer.fill[Option[DCacheLoadCallerTrans]](portNum)(None)
  private val s2_loadTrans = ArrayBuffer.fill[Option[DCacheLoadCallerTrans]](portNum)(None)
  private var storePortReqMessage: Option[LitDCacheLineReq] = None
  private var amoPortReqMessage: Option[LitDCacheWordReq] = None

  val outerLoad: ListBuffer[DCacheLoadCallerTrans] = ListBuffer()
  val outerStore: ListBuffer[DCacheStoreCallerTrans] = ListBuffer()
  val outerAMO: ListBuffer[DCacheAMOCallerTrans] = ListBuffer()

  override def transStep(): Unit = {
    outerLoad.foreach(_.step())
    outerStore.foreach(_.step())
    outerAMO.foreach(_.step())
  }

  private val maxStoreId = 255
  private val maxAMOId = 0

  private val lsqWaiting: mutable.Queue[DCacheLoadCallerTrans] = mutable.Queue[DCacheLoadCallerTrans]()
  private val storeIdMap: mutable.Map[BigInt, DCacheStoreCallerTrans] = mutable.Map[BigInt, DCacheStoreCallerTrans]()
  private val amoIdMap: mutable.Map[BigInt, DCacheAMOCallerTrans] = mutable.Map[BigInt, DCacheAMOCallerTrans]()

  def issueLoadReq(): Unit = {
    for (i <- 0 until portNum) {
      if (loadPortsReqMessage(i).isEmpty) {
        val nextLoad = outerLoad.find(l => !l.reqIssued.getOrElse(true))
        if (nextLoad.isDefined) {
          //alloc & issue
          s0_loadTrans(i) = nextLoad
          loadPortsReqMessage(i) = Some(nextLoad.get.issueLoadReq())
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
    loadT.pairLoadResp(resp)
    if (!resp.miss) {
      //search store list for addr conflict
      val alignLoadAddr = addrAlignBlock(loadAddr)
      val conflictMask = storeIdMap.foldLeft(BigInt(0))((b, kv) =>
        if (kv._2.req.get.addr == alignLoadAddr) {
          b | kv._2.req.get.mask
        }
        else
          b
      )
      val conflictWordMask = maskOutOfWord(conflictMask, wordInBlock(loadAddr))
      insertMaskedWordRead(loadAddr, resp.data, cleanMask(loadT.req.get.mask, conflictWordMask))
      outerLoad -= loadT
    }
    else if (resp.replay) {
      outerLoad -= loadT //drop it
      loadT.replayLoad() //mark replay
      outerLoad.append(loadT) //pushpack
    }
    else {
      lsqWaiting.enqueue(loadT)
    }

  }

  def killS1(i: Int): Unit = {
    val l = s1_loadTrans(i)
    if (l.isDefined) {
      outerLoad -= l.get
      s1_loadTrans(i) = None
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
        val conflictMask = storeIdMap.foldLeft(BigInt(0))((b, kv) =>
          if (kv._2.req.get.addr == alignAddr) {
            b | kv._2.req.get.mask
          }
          else
            b
        )
        val loadBlockMask = genWordMaskInBlock(loadAddr, q.req.get.mask)
        insertMaskedReadSnap(alignAddr, resp.data, insertVersionRead(loadAddr, 0),
          cleanMask(loadBlockMask, conflictMask))
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
        val allocId = (0 to maxStoreId).find(i => !storeIdMap.contains(BigInt(i)))
        if (allocId.isDefined) {
          //alloc & issue
          storePortReqMessage = Some(nextStore.get.issueStoreReq(BigInt(allocId.get)))
          storeIdMap(BigInt(allocId.get)) = nextStore.get
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
    val storeReq = storeTrans.req.get
    storeTrans.pairStoreResp(resp)
    //free resource
    storeIdMap.remove(storeId)
    //drop finished store
    outerStore -= storeTrans
    //confirm data write
    insertMaskedWrite(storeReq.addr, storeReq.data, storeReq.mask)
  }

  def issueAMOReq(): Unit = {
    if (amoPortReqMessage.isEmpty) {
      val nextAMO = outerAMO.find(a => !a.reqIssued.getOrElse(true))
      if (nextAMO.isDefined) {
        val allocId = (0 to maxAMOId).find(i => !amoIdMap.contains(BigInt(i)))
        if (allocId.isDefined) {
          val aid = allocId.get
          //alloc & issue
          amoPortReqMessage = Some(nextAMO.get.issueReq(BigInt(aid)))
          amoIdMap(BigInt(aid)) = nextAMO.get
        }
      }
    }
  }

  def peekAMOReq(): Option[LitDCacheWordReq] = {
    amoPortReqMessage
  }

  def fireAMOReq(): Unit = {
    amoPortReqMessage = None
  }

  def fireAMOResp(resp: LitDCacheWordResp): Unit = {
    val aid = resp.id
    val amoT = amoIdMap(aid)
    val amoReq = amoT.req.get
    amoT.pairResp(resp)
    amoIdMap.remove(aid)
    outerAMO -= amoT
    if (amoReq.cmd == M_XA_SWAP) {
      insertMaskedWordRead(amoReq.addr, resp.data, amoReq.mask)
      insertMaskedWordWrite(amoReq.addr, amoReq.data, amoReq.mask)
    }
  }

  override def step(): Unit = {
    for (i <- 0 until portNum) {
      s2_loadTrans(i) = s1_loadTrans(i)
      if (loadPortsReqMessage(i).isEmpty) { //if fired
        s1_loadTrans(i) = s0_loadTrans(i)
        s0_loadTrans(i) = None
      }
      else {
        s1_loadTrans(i) = None
      }
    }
    super.step()
  }

  def addLoad(addr: BigInt): Unit = {
    //addr is aligned to block
    val loadT = new DCacheLoadCallerTrans()

    val wordCnt = rand.nextInt(8)

    val lgSize = rand.nextInt(4)
    val rsize = 1 << lgSize
    // addr must be aligned to size
    val offset = (rand.nextInt(8) >> lgSize) << lgSize
    val laddr = addr + wordCnt * 8 + offset
    // generate mask from  raddr and rsize
    val mask = (BigInt(1) << rsize) - 1
    val wmask = mask << offset
    loadT.prepareLoad(laddr, wmask)
    outerLoad.append(loadT)
  }

  def addStore(addr: BigInt): Unit = {
    //addr is aligned to block
    val storeT = new DCacheStoreCallerTrans()
    val blockMask = (0 until 4).foldLeft(BigInt(0))(
      (d, _) => (d << 16) | BigInt(rand.nextInt(0xffff))
    )
    storeT.prepareStore(addr, randomBlockData(), blockMask)
    outerStore.append(storeT)
  }

  def addAMO(addr: BigInt): Unit = {
    val amoT = new DCacheAMOCallerTrans()

    val wordCnt = rand.nextInt(8)

    val lgSize = rand.nextInt(4)
    val rsize = 1 << lgSize
    // addr must be aligned to size
    val offset = (rand.nextInt(8) >> lgSize) << lgSize
    val laddr = addr + wordCnt * 8 + offset
    // generate mask from  raddr and rsize
    val mask = (BigInt(1) << rsize) - 1
    val wmask = mask << offset
    val wdata = (0 until 4).foldLeft(BigInt(0))(
      (d, _) => (d << 16) | BigInt(rand.nextInt(0xffff))
    )
    //TODO: only swap for now
    amoT.prepareAMOSwap(addr, wdata, wmask)
    outerAMO.append(amoT)
  }

}