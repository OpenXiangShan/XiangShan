package cache

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map, Queue, Seq, ArrayBuffer}

class AddrState extends TLCOp
{
  val callerTrans: ListBuffer[TLCCallerTrans] = ListBuffer()
  val calleeTrans: ListBuffer[TLCCalleeTrans] = ListBuffer()
  var masterPerm : BigInt = nothing
  var myPerm : BigInt = trunk
  var data : BigInt = 0

  var pendingGrant = false
  var pendingGrantAck = false
  var pendingReleaseAck = false
  var pendingProbeAck = false

  def masterUpdatePendingGrant() : Unit = {
    pendingGrant = callerTrans.foldLeft(false)((res,caller) => {
      res || {
        caller match {
          case acq : AcquireCallerTrans => acq.grantPending.getOrElse(false)
          case _ => false
        }
      }
    })
  }
  def slaveUpdatePendingGrantAck() : Unit = {
    pendingGrantAck = calleeTrans.foldLeft(false)((res,callee) => {
      res || {
        callee match {
          case acq : AcquireCalleeTrans => acq.grantAckPending.getOrElse(false)
          case _ => false
        }
      }
    })
  }
  def masterUpdatePendingReleaseAck() : Unit = {
    pendingReleaseAck = callerTrans.foldLeft(false)((res,caller) => {
      res || {
        caller match {
          case rel : ReleaseCallerTrans => rel.releaseAckPending.getOrElse(false)
          case _ => false
        }
      }
    })
  }
  def slaveUpdatePendingProbeAck() : Unit = {
    pendingProbeAck = calleeTrans.foldLeft(false)((res,callee) => {
      res || {
        callee match {
          case pro : ProbeCallerTrans => pro.probeAckPending.getOrElse(false)
          case _ => false
        }
      }
    })
  }

  def blockInnerAcquire() : Boolean = {
    pendingGrantAck || pendingProbeAck || pendingGrant || pendingReleaseAck
  }
  def blockOuterProbe() : Boolean = {
    pendingGrantAck || pendingProbeAck || pendingReleaseAck
  }
  def blockInnerRelease() : Boolean = {
    pendingReleaseAck
  }

}

class FireQueue[T <: TLCScalaMessage](){
  val q = mutable.Queue[(T,Int)]()
  var headCnt = 0
  var beatCnt = 0
  def enqMessage( message:T , cnt: Int = 1) : Unit = {
    q.enqueue((message, cnt))
    if (q.size == 1){
      headCnt = cnt
      beatCnt = 0
    }
  }
  def fireHead() : Unit = {
    beatCnt+=1
    if (beatCnt == headCnt) {
      q.dequeue()
      beatCnt = 0
      if (!q.isEmpty){
        headCnt = q.head._2
      }
    }
  }
}

trait BigIntExtract {
  val prefix = Array(0.toByte)
  def extract256Bit(n:BigInt, index:Int) : BigInt = {
    val mask256 = BigInt( prefix ++ Array.fill(32)(0xff.toByte))
    (n >> (index*256)) & mask256
  }
  def replaceNBytes(n:BigInt, in:BigInt, start: Int, len: Int): BigInt = {
    val inArray = in.toByteArray
    val nArray = n.toByteArray
    require(inArray.size <= len,"given insert value longer than len")
    if(nArray.size <= start){
      BigInt( prefix ++ inArray ++ Array.fill(start-nArray.size)(0.toByte) ++ nArray )
    }
    else{
      BigInt( prefix ++ nArray.dropRight(start + len) ++ Array.fill(len - inArray.size)(0.toByte) ++ inArray ++ nArray.takeRight(start) )
    }
  }
  def extractBitField(i:BigInt) : BigInt = {
    0
  }
}

class TLCAgent() extends TLCOp with BigIntExtract with PermissionTransition {
  val beatNum = TLCCacheTestKey.default.get.blockBytes/TLCCacheTestKey.default.get.beatBytes
  val beatBits = TLCCacheTestKey.default.get.beatBytes * 8
}
class TLCSlaveAgent(val maxSink:Int, addrStateMap: Map[BigInt, AddrState], serialList: ListBuffer[TLCTrans]) extends TLCAgent {
  val innerAcquire = ListBuffer[AcquireCalleeTrans]()
  val innerRelease = ListBuffer[ReleaseCalleeTrans]()
  val innerProbe = ListBuffer[ProbeCallerTrans]()

  val sinkIdMap = Map[BigInt, AcquireCalleeTrans]()

  val aList = ListBuffer[TLCScalaA]()
  val cList = ListBuffer[TLCScalaC]()
  var tmpC:TLCScalaC
  var c_cnt = 0
  //E will never block
  val bQueue = new FireQueue[TLCScalaB]()
  val dQueue = new FireQueue[TLCScalaD]()

  def banIssueGrant(addr:BigInt) : Boolean = {
    val addrState = addrStateMap.get(addr)
    if(addrState.isEmpty)
      false
    else
      addrState.get.pendingProbeAck
  }
  def banIssueProbe(addr:BigInt) : Boolean = {
    val addrState = addrStateMap.get(addr)
    if(addrState.isEmpty)
      false
    else
      addrState.get.pendingGrantAck || addrState.get.pendingProbeAck
  }

  //Due to no backup pressure in channel E, the handle function is integrated into tick function
  def tickE(inE:TLCScalaE ): Unit = {
    assert(sinkIdMap.contains(inE.sink),"no sinkId for GrantAck")
    val acq = sinkIdMap(inE.sink)
    acq.pairGrantAck(inE)
    val addr = acq.a.get.address
    val state = addrStateMap(addr)
    //update state
    state.slaveUpdatePendingGrantAck()
    //free sinkId
    sinkIdMap.remove(inE.sink)
    //remove from addrList and agentList
    state.calleeTrans -= acq
    innerAcquire -= acq
  }
  def issueD() : Unit = {
    //search ReleaseAck to issue
    innerRelease --= innerRelease.filter{ r =>
      if (r.releaseAckIssued.getOrElse(true)){
        false
      }
      else {
        dQueue.enqMessage(r.issueReleaseAck())
        //when releaseAck is issued, the meta & data will be changed
        val addr = r.c.get.address
        val state = addrStateMap(addr)
        state.masterPerm = shrinkTarget(r.c.get.param)
        if (r.c.get.opcode == ReleaseData)
          state.data = r.c.get.data
        //serialization point
        serialList.append(r)
        //remove from addrList and agentList
        state.calleeTrans -= r
        true//condition to remove from agent list safetly
      }
    }
    if(sinkIdMap.size < maxSink){
      val sinkQ = Queue() ++ List.tabulate(maxSink)(a => BigInt(a)).filterNot(k => sinkIdMap.contains(k))
      //search Grant to issue
      innerAcquire.foreach{ acq =>
        //TODO:check recursive trans completion before issue
        if (!acq.grantIssued.getOrElse(true)){//need issue
          if (!sinkQ.isEmpty){//has empty sinkid
            val a_acq = acq.a.get
            val addr = a_acq.address
            if (!banIssueGrant(addr)){//ok to issue
              val allocId = sinkQ.dequeue()
              val state = addrStateMap(addr)
              if (a_acq.opcode == AcquirePerm) {
                dQueue.enqMessage(acq.issueGrant(allocId))
              }
              else { //is AcquireBlock
                if(a_acq.param == BtoT)
                  dQueue.enqMessage(acq.issueGrant(allocId))
                else
                  dQueue.enqMessage(acq.issueGrantData(allocId,state.data))
              }
              //update state
              state.masterPerm = growTarget(a_acq.param)
              state.slaveUpdatePendingGrantAck()
              //mark Id allocated
              sinkIdMap(allocId) = acq
            }
          }
        }
      }
    }
  }
  //Check if there is any message to fire
  def peekD() : Option[TLCScalaD] = {
    if (dQueue.q.isEmpty){
      None
    }
    else{
      val headD = dQueue.q.head._1
      Some(headD.copy(newData = extract256Bit(headD.data,dQueue.beatCnt)))
    }
  }
  //Notify the agent this channel has been fired
  def fireD() : Unit = {
    dQueue.fireHead()
  }

  def fireC(inC:TLCScalaC ) : Unit = {
    if (inC.opcode == ReleaseData || inC.opcode == ProbeAckData){
      if(c_cnt==0) {//start burst
        tmpC = inC.copy()
        c_cnt += 1
      }
      else {//burst beat
        tmpC.data = tmpC.data | inC.data<<(beatBits * c_cnt)
        c_cnt += 1
        if (c_cnt == beatNum)
          handleC(tmpC)
      }
    }
    else
      handleC(inC)
  }

  def handleC(c:TLCScalaC):Unit = {
    c.opcode match {
      case ProbeAck => {
        val state = addrStateMap(c.address)
        val probeT = innerProbe.filter(p => p.probeAckPending.getOrElse(false)).filter(p => p.b.get.address == c.address && p.b.get.source == c.source).head
        //pair ProbeAck
        probeT.pairProbeAck(c)
        //update state
        assert(state.masterPerm == shrinkFrom(c.param))
        state.masterPerm = shrinkTarget(c.param)
        state.slaveUpdatePendingProbeAck()
        //serialization point
        serialList.append(probeT)
        //remove from addrList and agentList
        state.callerTrans -= probeT
        innerProbe -= probeT
        //TODO:update father trans call list after completion
      }
      case ProbeAckData => {
        val state = addrStateMap(c.address)
        val probeT = innerProbe.filter(p => p.probeAckPending.getOrElse(false)).filter(p => p.b.get.address == c.address && p.b.get.source == c.source).head
        //pair ProbeAck
        probeT.pairProbeAck(c)
        //update state
        assert(state.masterPerm == shrinkFrom(c.param))
        state.masterPerm = shrinkTarget(c.param)
        state.data = c.data
        state.slaveUpdatePendingProbeAck()
        //serialization point
        serialList.append(probeT)
        //remove from addrList and agentList
        state.callerTrans -= probeT
        innerProbe -= probeT
        //TODO:update father trans call list after completion
      }
      case Release => {
        val addr = c.address
        val state = addrStateMap(addr)
        val acq_list = state.calleeTrans.filter(_.isInstanceOf[AcquireCalleeTrans])
        assert(acq_list.filterNot(a => a.asInstanceOf[AcquireCalleeTrans].grantIssued.getOrElse(true)).isEmpty,"Detect master issue Release when pending Grant")
        //TODO:only support one master for now
        cList.append(c)
      }
      case ReleaseData => {
        val addr = c.address
        val state = addrStateMap(addr)
        val acq_list = state.calleeTrans.filter(_.isInstanceOf[AcquireCalleeTrans])
        assert(acq_list.filterNot(a => a.asInstanceOf[AcquireCalleeTrans].grantIssued.getOrElse(true)).isEmpty,"Detect master issue Release when pending Grant")
        //TODO:only support one master for now
        cList.append(c)
      }
    }
  }
  def tickC(): Unit = {
    cList --= cList.filter{ c =>
      val addr = c.address
      val state = addrStateMap(addr)
      if (state.blockInnerRelease()){
        false
      }
      else {
        val rel = ReleaseCalleeTrans()
        rel.pairRelease(c)
        //add to addr list and agent list
        innerRelease.append(rel)
        state.calleeTrans.append(rel)
        true//condition to remove from cList
      }
    }
  }

  def issueB() : Unit = {
    innerProbe.foreach{ p =>
      if(!p.probeIssued.getOrElse(true)){
        val addr = p.b.get.address
        //TODO:check recursive trans completion before issue
        if (!banIssueProbe(addr)){//ok to issue
          val state = addrStateMap(addr)
          bQueue.enqMessage(p.issueProbe())
          //update state
          state.slaveUpdatePendingProbeAck()
        }
      }
    }
  }
  def peekB() : Option[TLCScalaB] = {
    if (bQueue.q.isEmpty){
      None
    }
    else{
      Some(bQueue.q.head._1.copy(newData = extract256Bit(bQueue.q.head._1.data,bQueue.beatCnt)))
    }
  }
  def fireB() : Unit = {
    bQueue.fireHead()
  }

  def fireA(inA:TLCScalaA) : Unit = {
    aList.append(inA)
  }
  def tickA() : Unit = {
    aList --= aList.filter{a =>
      val addr = a.address
      val state = addrStateMap.getOrElse(addr,new AddrState())
      if (!addrStateMap.contains(addr)){//alloc new state if need
        addrStateMap += (addr -> state)
      }
      if (state.blockInnerAcquire()){//blocking
        false
      }
      else{//not blocking
        val transA = AcquireCalleeTrans()
        transA.pairAcquire(a)
        //serilization point
        serialList.append(transA)
        //add to addr list and agent list
        innerAcquire.append(transA)
        state.calleeTrans.append(transA)
        true
      }
    }
  }

}
class TLCMasterAgent(val maxSource:Int, addrStateMap: Map[BigInt, AddrState]) extends TLCAgent{
  val outerAcquire : ListBuffer[AcquireCallerTrans] = ListBuffer()
  val outerRelease : ListBuffer[ReleaseCallerTrans] = ListBuffer()
  val outerProbe : ListBuffer[ProbeCalleeTrans] = ListBuffer()

}