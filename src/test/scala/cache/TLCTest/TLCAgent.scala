package cache.TLCTest

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Map, Queue}

class AddrState extends TLCOp {
  val callerTrans: ListBuffer[TLCCallerTrans] = ListBuffer()
  val calleeTrans: ListBuffer[TLCCalleeTrans] = ListBuffer()
  var masterPerm: BigInt = nothing
  var myPerm: BigInt = trunk
  var data: BigInt = 0
  var dirty: Boolean = false

  //for validation only
  var version: Int = 0

  var pendingGrant = false
  var pendingGrantAck = false
  var pendingReleaseAck = false
  var pendingProbeAck = false

  def masterUpdatePendingGrant(): Unit = {
    pendingGrant = callerTrans.foldLeft(false)((res, caller) => {
      res || {
        caller match {
          case acq: AcquireCallerTrans => acq.grantPending.getOrElse(false)
          case _ => false
        }
      }
    })
  }

  def slaveUpdatePendingGrantAck(): Unit = {
    pendingGrantAck = calleeTrans.foldLeft(false)((res, callee) => {
      res || {
        callee match {
          case acq: AcquireCalleeTrans => acq.grantAckPending.getOrElse(false)
          case _ => false
        }
      }
    })
  }

  def masterUpdatePendingReleaseAck(): Unit = {
    pendingReleaseAck = callerTrans.foldLeft(false)((res, caller) => {
      res || {
        caller match {
          case rel: ReleaseCallerTrans => rel.releaseAckPending.getOrElse(false)
          case _ => false
        }
      }
    })
  }

  def slaveUpdatePendingProbeAck(): Unit = {
    pendingProbeAck = calleeTrans.foldLeft(false)((res, callee) => {
      res || {
        callee match {
          case pro: ProbeCallerTrans => pro.probeAckPending.getOrElse(false)
          case _ => false
        }
      }
    })
  }

  def blockInnerAcquire(): Boolean = {
    pendingGrantAck || pendingProbeAck || pendingGrant || pendingReleaseAck
  }

  def blockOuterProbe(): Boolean = {
    pendingGrantAck || pendingProbeAck || pendingReleaseAck
  }

  def blockInnerRelease(): Boolean = {
    pendingReleaseAck
  }

}

class FireQueue[T <: TLCScalaMessage]() {
  val q: mutable.Queue[(T, Int)] = mutable.Queue[(T, Int)]()
  var headCnt = 0
  var beatCnt = 0

  def enqMessage(message: T, cnt: Int = 1): Unit = {
    q.enqueue((message, cnt))
    if (q.size == 1) {
      headCnt = cnt
      beatCnt = 0
    }
  }

  def fireHead(): Unit = {
    beatCnt += 1
    if (beatCnt == headCnt) {
      q.dequeue()
      beatCnt = 0
      if (q.nonEmpty) {
        headCnt = q.head._2
      }
    }
  }
}

trait BigIntExtract {
  val prefix: Array[Byte] = Array(0.toByte)

  def extract256Bit(n: BigInt, index: Int): BigInt = {
    val mask256 = BigInt(prefix ++ Array.fill(32)(0xff.toByte))
    (n >> (index * 256)) & mask256
  }

  def replaceNBytes(n: BigInt, in: BigInt, start: Int, len: Int): BigInt = {
    val inArray = in.toByteArray
    val nArray = n.toByteArray
    require(inArray.size <= len, "given insert value longer than len")
    if (nArray.size <= start) {
      BigInt(prefix ++ inArray ++ Array.fill(start - nArray.size)(0.toByte) ++ nArray)
    }
    else {
      BigInt(prefix ++ nArray.dropRight(start + len) ++ Array.fill(len - inArray.size)(0.toByte) ++ inArray ++ nArray.takeRight(start))
    }
  }

  def extractByte(n: BigInt, start: Int, len: Int): BigInt = {
    val mask = BigInt(prefix ++ Array.fill(len)(0xff.toByte))
    (n >> (start * 8)) & mask
  }
}

class TLCAgent(ID: Int, addrStateMap: mutable.Map[BigInt, AddrState], serialList: ArrayBuffer[(Int, TLCTrans)])
  extends TLCOp with BigIntExtract with PermissionTransition {
  val beatNum = TLCCacheTestKey.default.get.blockBytes / TLCCacheTestKey.default.get.beatBytes
  val beatBits = TLCCacheTestKey.default.get.beatBytes * 8

  def getState(addr: BigInt): AddrState = {
    val state = addrStateMap.getOrElse(addr, new AddrState())
    if (!addrStateMap.contains(addr)) { //alloc new state if need
      addrStateMap += (addr -> state)
    }
    state
  }

  def appendSerial(t: TLCTrans): Unit = {
    serialList.synchronized {
      serialList.append((ID, t))
    }
  }

  def insertWrite(addr: BigInt): Unit = {
    val addrState = getState(addr)
    val fakew = new FakeWriteTrans(addr)
    fakew.data = addrState.data
    //check last data
    val lastWrite = extractByte(fakew.data, ID * 16 + addrState.version % 16, 1)
    require(lastWrite == addrState.version, s"agent $ID data has been changed, Addr: $addr, version: ${addrState.version}, data: ${addrState.data}")
    //new version
    addrState.version = (addrState.version + 1) % 256
    addrState.data = replaceNBytes(addrState.data, BigInt(addrState.version), ID * 16 + addrState.version % 16, 1)
    addrState.dirty = true
    fakew.newData = addrState.data
    //append to serial list
    appendSerial(fakew)
  }

  def insertRead(addr: BigInt): Unit = {
    val addrState = getState(addr)
    val faker = new FakeReadTrans(addr)
    faker.data = addrState.data
    //check last data
    val lastWrite = extractByte(faker.data, ID * 16 + addrState.version % 16, 1)
    require(lastWrite == addrState.version, s"agent $ID data has been changed, Addr: $addr, version: ${addrState.version}, data: ${addrState.data}")
    //append to serial list
    appendSerial(faker)
  }

}

class TLCSlaveAgent(ID: Int, val maxSink: Int, addrStateMap: mutable.Map[BigInt, AddrState], serialList: ArrayBuffer[(Int, TLCTrans)])
  extends TLCAgent(ID, addrStateMap, serialList) {
  val innerAcquire = ListBuffer[AcquireCalleeTrans]()
  val innerRelease = ListBuffer[ReleaseCalleeTrans]()
  val innerProbe = ListBuffer[ProbeCallerTrans]()

  val sinkIdMap = mutable.Map[BigInt, AcquireCalleeTrans]()

  val aList = ListBuffer[TLCScalaA]()
  val cList = ListBuffer[TLCScalaC]()
  var tmpC = new TLCScalaC()
  var c_cnt = 0
  //E will never block
  val bQueue = new FireQueue[TLCScalaB]()
  val dQueue = new FireQueue[TLCScalaD]()

  def banIssueGrant(addr: BigInt): Boolean = {
    val addrState = getState(addr)
    addrState.pendingProbeAck
  }

  def banIssueProbe(addr: BigInt): Boolean = {
    val addrState = getState(addr)
    addrState.pendingGrantAck || addrState.pendingProbeAck
  }

  //Due to no backup pressure in channel E, the handle function is integrated into fire function
  def fireE(inE: TLCScalaE): Unit = {
    require(sinkIdMap.contains(inE.sink), "no sinkId for GrantAck")
    val acq = sinkIdMap(inE.sink)
    acq.pairGrantAck(inE)
    val addr = acq.a.get.address
    val state = getState(addr)
    //update state
    state.slaveUpdatePendingGrantAck()
    //free sinkId
    sinkIdMap.remove(inE.sink)
    //remove from addrList and agentList
    state.calleeTrans -= acq
    innerAcquire -= acq
  }

  def issueD(): Unit = {
    //search ReleaseAck to issue
    innerRelease --= innerRelease.filter { r =>
      if (r.releaseAckIssued.getOrElse(true)) {
        false
      }
      else {
        dQueue.enqMessage(r.issueReleaseAck())
        //when releaseAck is issued, the meta & data will be changed
        val addr = r.c.get.address
        val state = getState(addr)
        state.masterPerm = shrinkTarget(r.c.get.param)
        if (r.c.get.opcode == ReleaseData) {
          state.data = r.c.get.data
          if (state.masterPerm == nothing){
            insertWrite(addr)//modify data when master is invalid
          }
          else {
            insertRead(addr)
          }
        }
        else {
          if (state.masterPerm == nothing){
            insertWrite(addr)//modify data when master is invalid
          }
        }
        //serialization point
        appendSerial(r)
        //remove from addrList and agentList
        state.calleeTrans -= r
        true //condition to remove from agent list safely
      }
    }
    if (sinkIdMap.size < maxSink) { //fast check available ID
      val sinkQ = mutable.Queue() ++ List.tabulate(maxSink)(a => BigInt(a)).filterNot(k => sinkIdMap.contains(k))
      //search Grant to issue
      innerAcquire.foreach { acq =>
        //TODO:check recursive trans completion before issue
        if (!acq.grantIssued.getOrElse(true)) { //need issue
          val a_acq = acq.a.get
          val addr = a_acq.address
          val state = getState(addr)
          if (sinkQ.nonEmpty && !banIssueGrant(addr)) { //has empty sinkid and ok to issue
            val allocId = sinkQ.dequeue()
            if (a_acq.opcode == AcquirePerm) {
              dQueue.enqMessage(acq.issueGrant(allocId))
            }
            else { //is AcquireBlock
              if (a_acq.param == BtoT) {
                dQueue.enqMessage(acq.issueGrant(allocId))
              }
              else {
                dQueue.enqMessage(acq.issueGrantData(allocId, state.data), cnt = beatNum)
              }
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

  //Check if there is any message to fire
  def peekD(): Option[TLCScalaD] = {
    if (dQueue.q.isEmpty) {
      None
    }
    else {
      val headD = dQueue.q.head._1
      Some(headD.copy(newData = extract256Bit(headD.data, dQueue.beatCnt)))
    }
  }

  //Notify the agent this channel has been fired
  def fireD(): Unit = {
    dQueue.fireHead()
  }

  def fireC(inC: TLCScalaC): Unit = {
    if (inC.opcode == ReleaseData || inC.opcode == ProbeAckData) {
      if (c_cnt == 0) { //start burst
        tmpC = inC.copy()
        c_cnt += 1
      }
      else { //burst beat
        tmpC.data = tmpC.data | inC.data << (beatBits * c_cnt)
        c_cnt += 1
        if (c_cnt == beatNum) {
          handleC(tmpC)
        }
      }
    }
    else {
      handleC(inC)
    }
  }

  def handleC(c: TLCScalaC): Unit = {
    c.opcode match {
      case ProbeAck => {
        val addr = c.address
        val state = getState(addr)
        val probeT = innerProbe.filter(p => p.probeAckPending.getOrElse(false)).filter(p => p.b.get.address == addr && p.b.get.source == c.source).head
        //pair ProbeAck
        probeT.pairProbeAck(c)
        //update state
        assert(state.masterPerm == shrinkFrom(c.param))
        state.masterPerm = shrinkTarget(c.param)
        state.slaveUpdatePendingProbeAck()
        if (state.masterPerm == nothing){
          insertWrite(addr)//modify data when master is invalid
        }
        else {
          insertRead(addr)
        }
        //serialization point
        appendSerial(probeT)
        //remove from addrList and agentList
        state.callerTrans -= probeT
        innerProbe -= probeT
        //TODO:update father trans call list after completion
      }
      case ProbeAckData => {
        val addr = c.address
        val state = getState(addr)
        val probeT = innerProbe.filter(p => p.probeAckPending.getOrElse(false)).filter(p => p.b.get.address == addr && p.b.get.source == c.source).head
        //pair ProbeAck
        probeT.pairProbeAck(c)
        //update state
        assert(state.masterPerm == shrinkFrom(c.param))
        state.masterPerm = shrinkTarget(c.param)
        state.data = c.data
        state.slaveUpdatePendingProbeAck()
        if (state.masterPerm == nothing){
          insertWrite(addr)//modify data when master is invalid
        }
        else {
          insertRead(addr)
        }
        //serialization point
        appendSerial(probeT)
        //remove from addrList and agentList
        state.callerTrans -= probeT
        innerProbe -= probeT
        //TODO:update father trans call list after completion
      }
      case Release => {
        val addr = c.address
        val state = getState(addr)
        val acq_list = state.calleeTrans.filter(_.isInstanceOf[AcquireCalleeTrans])
        assert(acq_list.forall(a => a.asInstanceOf[AcquireCalleeTrans].grantIssued.getOrElse(true)), "Detect master issue Release when pending Grant")
        //TODO:only support one master for now
        cList.append(c)
      }
      case ReleaseData => {
        val addr = c.address
        val state = getState(addr)
        val acq_list = state.calleeTrans.filter(_.isInstanceOf[AcquireCalleeTrans])
        assert(acq_list.forall(a => a.asInstanceOf[AcquireCalleeTrans].grantIssued.getOrElse(true)), "Detect master issue Release when pending Grant")
        //TODO:only support one master for now
        cList.append(c)
      }
    }
  }

  def tickC(): Unit = {
    cList --= cList.filter { c =>
      val addr = c.address
      val state = getState(addr)
      if (state.blockInnerRelease()) {
        false
      }
      else {
        val rel = ReleaseCalleeTrans()
        rel.pairRelease(c)
        //add to addr list and agent list
        innerRelease.append(rel)
        state.calleeTrans.append(rel)
        true //condition to remove from cList
      }
    }
  }

  def issueB(): Unit = {
    innerProbe.foreach { p =>
      if (!p.probeIssued.getOrElse(true)) {
        val addr = p.b.get.address
        val state = getState(addr)
        //TODO:check recursive trans completion before issue
        if (!banIssueProbe(addr)) { //ok to issue
          bQueue.enqMessage(p.issueProbe())
          //append to addr caller list
          state.callerTrans.append(p)
          //update state
          state.slaveUpdatePendingProbeAck()
        }
      }
    }
  }

  def peekB(): Option[TLCScalaB] = {
    if (bQueue.q.isEmpty) {
      None
    }
    else {
      Some(bQueue.q.head._1.copy(newData = extract256Bit(bQueue.q.head._1.data, bQueue.beatCnt)))
    }
  }

  def fireB(): Unit = {
    bQueue.fireHead()
  }

  def fireA(inA: TLCScalaA): Unit = {
    aList.append(inA)
  }

  def tickA(): Unit = {
    aList --= aList.filter { a =>
      val addr = a.address
      val state = getState(addr)
      if (state.blockInnerAcquire()) { //blocking
        false
      }
      else { //not blocking
        val transA = AcquireCalleeTrans()
        transA.pairAcquire(a)
        //serialization point
        appendSerial(transA)
        //add to addr list and agent list
        innerAcquire.append(transA)
        state.calleeTrans.append(transA)
        true
      }
    }
  }

}

class TLCMasterAgent(ID: Int, val maxSource: Int, addrStateMap: mutable.Map[BigInt, AddrState], serialList: ArrayBuffer[(Int, TLCTrans)])
  extends TLCAgent(ID, addrStateMap, serialList) {
  val outerAcquire: ListBuffer[AcquireCallerTrans] = ListBuffer()
  val outerRelease: ListBuffer[ReleaseCallerTrans] = ListBuffer()
  val outerProbe: ListBuffer[ProbeCalleeTrans] = ListBuffer()

  val sourceAMap = mutable.Map[BigInt, AcquireCallerTrans]()
  val sourceCMap = mutable.Map[BigInt, ReleaseCallerTrans]()

  val bList = ListBuffer[TLCScalaB]()

  var tmpD = new TLCScalaD()
  var d_cnt = 0

  val aQueue = new FireQueue[TLCScalaA]()
  val cQueue = new FireQueue[TLCScalaC]()
  val eQueue = new FireQueue[TLCScalaE]()

  def banIssueAcquire(addr: BigInt): Boolean = {
    val addrState = getState(addr)
    addrState.pendingGrant || addrState.pendingReleaseAck
  }

  def banIssueRelease(addr: BigInt): Boolean = {
    val addrState = getState(addr)
    addrState.pendingGrant || addrState.pendingReleaseAck
  }

  def banIssueProbeAck(addr: BigInt): Boolean = {
    val addrState = getState(addr)
    addrState.pendingReleaseAck
  }

  //issue GrantAck in responding to grant, thus no need to define issueE
  def peekE(): Option[TLCScalaE] = {
    if (eQueue.q.isEmpty) {
      None
    }
    else {
      Some(eQueue.q.head._1)
    }
  }

  def fireE(): Unit = {
    eQueue.fireHead()
  }

  def fireD(inD: TLCScalaD): Unit = {
    if (inD.opcode == GrantData) {
      if (d_cnt == 0) { //start burst
        tmpD = inD.copy()
        d_cnt += 1
      }
      else {
        tmpD.data = tmpD.data | inD.data << (beatBits * d_cnt)
        d_cnt += 1
        if (d_cnt == beatNum) {
          handleD(tmpD)
        }
      }
    }
    else {
      handleD(inD)
    }
  }

  def handleD(d: TLCScalaD): Unit = {
    d.opcode match {
      case ReleaseAck => {
        require(sourceCMap.contains(d.source), "no sourceID for ReleaseAck")
        val rel = sourceCMap(d.source)
        rel.pairReleaseAck(d)
        //handle meta
        val addr = rel.c.get.address
        val state = getState(addr)
        state.myPerm = rel.targetPerm
        //update state
        state.masterUpdatePendingReleaseAck()
        //free sourceID
        sourceCMap.remove(d.source)
        //remove from addrList and agentList
        state.callerTrans -= rel
        outerRelease -= rel
        //TODO: notify father transaction here
      }
      case Grant => {
        require(sourceAMap.contains(d.source), "no sourceID for Grant")
        val acq = sourceAMap(d.source)
        acq.pairGrant(d)
        //handle meta
        val addr = acq.a.get.address
        val state = getState(addr)
        if (!d.denied) {
          state.myPerm = d.param
          if (state.myPerm == trunk) {
            insertWrite(addr)//modify data when trunk
          }
          else {
            insertRead(addr)
          }
        }
        //issue GrantAck
        eQueue.enqMessage(acq.issueGrantAck())
        //update state
        state.masterUpdatePendingGrant()
        //free sourceID
        sourceAMap.remove(d.source)
        //serialization point
        appendSerial(acq)
        //remove from addrList and agentList
        state.callerTrans -= acq
        outerAcquire -= acq
      }
      case GrantData => {
        require(sourceAMap.contains(d.source), "no sourceID for Grant")
        val acq = sourceAMap(d.source)
        acq.pairGrant(d)
        //handle meta & data
        val addr = acq.a.get.address
        val state = getState(addr)
        if (!d.denied) {
          state.myPerm = d.param
          state.data = d.data
          if (state.myPerm == trunk) {
            insertWrite(addr)//modify data when trunk
          }
          else {
            insertRead(addr)
          }
        }
        //issue GrantAck
        eQueue.enqMessage(acq.issueGrantAck())
        //update state
        state.masterUpdatePendingGrant()
        //free sourceID
        sourceAMap.remove(d.source)
        //serialization point
        appendSerial(acq)
        //remove from addrList and agentList
        state.callerTrans -= acq
        outerAcquire -= acq
      }
    }
  }

  //Because D messages won't be blocked, there is no tickD()

  def issueC(): Unit = {
    //search ProbeAck to issue
    outerProbe --= outerProbe.filter { p =>
      if (p.probeAckIssued.getOrElse(true)) {
        false
      }
      else {
        //TODO: check recursive call here
        val addr = p.b.get.address
        val state = getState(addr)
        if (banIssueProbeAck(addr)) {
          false
        }
        else { //ok to issue
          val myperm = state.myPerm
          var targetPerm = p.b.get.param
          if (targetPerm < myperm) { //if target is higher than me
            targetPerm = myperm
          }
          //assume all probe is ProbeBlock
          if (state.dirty) { //need write back
            cQueue.enqMessage(p.issueProbeAckData(myperm, targetPerm, state.data), cnt = beatNum)
            if (targetPerm != trunk) {
              state.dirty = false
            }
          }
          else {
            cQueue.enqMessage(p.issueProbeAck(myperm, targetPerm))
          }
          //remove from addr list and agent list
          state.calleeTrans -= p
          true //condition to remove from agent list
        }
      }
    }

    //search Release here
    if (sourceCMap.size < maxSource) { //fast check available ID
      val sourceQ = mutable.Queue() ++ List.tabulate(maxSource)(a => BigInt(a)).filterNot(k => sourceAMap.contains(k))
      outerRelease.foreach { r =>
        if (!r.releaseIssued.getOrElse(true)) { //haven't issue release
          val addr = r.c.get.address
          val state = getState(addr)
          if (sourceQ.nonEmpty && !banIssueRelease(addr)) { //has empty source ID and ok to issue
            val allocId = sourceQ.dequeue()
            //TODO: random decide to report or not when target perm is higher than me
            if (r.targetPerm < state.myPerm) { //if target is higher
              r.targetPerm = state.myPerm
            }
            if (state.dirty) {
              cQueue.enqMessage(r.issueReleaseData(allocId, state.myPerm, state.data), cnt = beatNum)
              state.dirty = false
            }
            else {
              cQueue.enqMessage(r.issueRelease(allocId, state.myPerm))
            }
            //serialization point
            appendSerial(r)
            //append to addr caller list
            state.callerTrans.append(r)
            //update state
            state.masterUpdatePendingReleaseAck()
            //mark ID allocated
            sourceCMap(allocId) = r
          }
        }
      }
    }
  }

  def peekC(): Option[TLCScalaC] = {
    if (cQueue.q.isEmpty) {
      None
    }
    else {
      val headC = cQueue.q.head._1
      Some(headC.copy(newData = extract256Bit(headC.data, cQueue.beatCnt)))
    }
  }

  def fireC(): Unit = {
    cQueue.fireHead()
  }

  def fireB(inB: TLCScalaB): Unit = {
    bList.append(inB)
  }

  def tickB(): Unit = {
    bList --= bList.filter { b =>
      val addr = b.address
      val state = getState(addr)
      //assume all of B message is probe
      require(b.opcode == Probe, "Only support probe message")
      if (state.blockOuterProbe()) {
        false
      }
      else {
        val pro = ProbeCalleeTrans()
        pro.pairProbe(b)
        //serialization point
        appendSerial(pro)
        //TODO: add recursive call when probed
        //add to addr list and agent list
        outerProbe.append(pro)
        state.calleeTrans.append(pro)
        true //condition to remove from bList
      }
    }
  }

  def issueA(): Unit = {
    val abandonList = ListBuffer[AcquireCallerTrans]()
    if (sourceAMap.size < maxSource) { //fast check available ID
      val sourceQ = mutable.Queue() ++ List.tabulate(maxSource)(a => BigInt(a)).filterNot(k => sourceAMap.contains(k))
      outerAcquire.foreach { acq =>
        if (!acq.acquireIssued.getOrElse(true)) { //haven't issue acquire
          val addr = acq.a.get.address
          val state = getState(addr)
          if (acq.checkNeedGrow(state.myPerm)) { //really need grow
            if (sourceQ.nonEmpty && !banIssueAcquire(addr)) { //has empty sourceid and ok to issue
              //TODO:decide to make full write here, use acqblock for now
              val allocId = sourceQ.dequeue()
              aQueue.enqMessage(acq.issueAcquireBlock(allocId, state.myPerm))
              //serialization point
              appendSerial(acq)
              //append to addr caller list
              state.callerTrans.append(acq)
              //update state
              state.masterUpdatePendingGrant()
              //mark ID allocated
              sourceAMap(allocId) = acq
            }
          }
          else { //no need to grow. Delete from acquire trans list
            abandonList += acq
            //TODO:check father here
          }
        }
      }
    }
    outerAcquire --= abandonList
  }

  def peekA(): Option[TLCScalaA] = {
    if (aQueue.q.isEmpty) {
      None
    }
    else {
      Some(aQueue.q.head._1)
    }
  }

  def fireA(): Unit = {
    aQueue.fireHead()
  }


}