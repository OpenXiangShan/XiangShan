package cache.L1DTest

import cache.TLCTest.{AddrState, ScoreboardData, TLCAgent, TLCTrans}
import chipsalliance.rocketchip.config.Parameters

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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
    if (!resp.miss) {
      val wc = wordInBlock(loadAddr)
      insertMaskedRead(loadAddr, dataConcatWord(0, resp.data, wc), genWordMaskInBlock(loadAddr, loadT.req.get.mask))
    }
    else if (resp.replay) {
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
      if (loadPortsReqMessage(i).isEmpty) { //if fired
        s1_loadTrans(i) = s0_loadTrans(i)
        s0_loadTrans(i) = None
      }
      else {
        s1_loadTrans(i) = None
      }
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

}