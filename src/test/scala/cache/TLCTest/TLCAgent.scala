package cache.TLCTest

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import chipsalliance.rocketchip.config.Parameters
import scala.util.Random
import chisel3.util._

class ScoreboardData extends BigIntExtract {
  var mainData: BigInt = 0
  var mainVersion: BigInt = 0
  val dataMap: mutable.Map[BigInt, BigInt] = mutable.Map[BigInt, BigInt]()
  val refCntMap: mutable.Map[BigInt, Int] = mutable.Map[BigInt, Int]()

  def writeNewData(in: BigInt): Unit = {
    mainData = in
    mainVersion = 0
  }

  def getReadDataVersion(): BigInt = {
    if (!refCntMap.contains(mainVersion)) {
      val freeSlot = (1 to 7).map(BigInt(_)).find(!refCntMap.contains(_))
      mainVersion = freeSlot.get
      refCntMap(mainVersion) = 1
      dataMap(mainVersion) = mainData
    }
    else {
      refCntMap(mainVersion) = refCntMap(mainVersion) + 1
    }
    mainVersion
  }

  def peekMatchVersion(ver: BigInt): BigInt = {
    val cnt = refCntMap(ver)
    assert(cnt >= 1, "SB has more consumer than reference")
    val data = dataMap(ver)
    if (cnt == 1) {
      refCntMap.remove(ver)
      dataMap.remove(ver)
    }
    else {
      refCntMap(ver) = cnt - 1
    }
    data
  }
}

class AddrState extends TLCOp {
  val callerTrans: ListBuffer[TLCCallerTrans] = ListBuffer()
  val calleeTrans: ListBuffer[TLCCalleeTrans] = ListBuffer()
  var masterPerm: BigInt = nothing
  var myPerm: BigInt = nothing
  var data: BigInt = 0
  var dirty: Boolean = false

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
    pendingProbeAck = callerTrans.foldLeft(false)((res, caller) => {
      res || {
        caller match {
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
      val m = q.dequeue()
      if (m._1.trans.isDefined)
        m._1.trans.get.startTimer()
      beatCnt = 0
      if (q.nonEmpty) {
        headCnt = q.head._2
      }
    }
  }
}

class TLCAgent(ID: Int, name: String = "", addrStateMap: mutable.Map[BigInt, AddrState], serialList: ArrayBuffer[(Int, TLCTrans)],
               scoreboard: mutable.Map[BigInt, ScoreboardData])
              (implicit p: Parameters)
  extends TLCOp with BigIntExtract with PermissionTransition {
  val l2params = p(TLCCacheTestKey)
  val beatNum = l2params.blockBytes / l2params.beatBytes
  val beatBits = l2params.beatBytes * 8
  val blockWords = l2params.blockBytes / 8

  val wordBits = 64
  val wordBytes = 8
  val wordAddrBits = 3
  val blockAddrBits = log2Up(l2params.blockBytes)
  val beatAddrBits = log2Up(l2params.beatBytes)
  val fullBeatMask = BigInt(prefix ++ Array.fill(l2params.beatBytes)(0xff.toByte))
  val fullBlockMask = BigInt(prefix ++ Array.fill(l2params.blockBytes)(0xff.toByte))
  val offsetMask: Long = (1L << log2Up(l2params.blockBytes)) - 1

  val rand = new Random(0xdad)

  var clock = 0

  def transStep(): Unit = {
    Unit
  }

  def step(): Unit = {
    transStep()
    clock += 1
  }

  def debugPrefix(): String = {
    f"[DEBUG][time= $clock%19d] TLAgent$ID-$name: "
  }

  def debugPrintln(ins: String): Unit = {
    println(debugPrefix() ++ ins)
  }

  def getState(addr: BigInt): AddrState = {
    val state = addrStateMap.getOrElse(addr, new AddrState())
    if (!addrStateMap.contains(addr)) { //alloc new state if need
      addrStateMap += (addr -> state)
      if (!scoreboard.contains(addr)) { //alloc scoreboard if needed
        scoreboard += (addr -> new ScoreboardData())
      }
    }
    state
  }

  def countBeats(size: BigInt): Int = {
    if ((1 << size.toInt) <= l2params.beatBytes)
      1
    else
      ((1 << size.toInt) / l2params.beatBytes).toInt
  }

  def addrAlignBlock(addr: BigInt): BigInt = {
    (addr >> blockAddrBits) << blockAddrBits
  }

  def dataConcatBeat(oldData: BigInt, inData: BigInt, cnt: Int): BigInt = {
    oldData | (inData << (cnt * beatBits))
  }

  def dataConcatWord(oldData: BigInt, inData: BigInt, cnt: Int): BigInt = {
    oldData | (inData << (cnt * wordBits))
  }

  def dataOutOfBeat(inData: BigInt, cnt: Int): BigInt = {
    inData >> (cnt * beatBits)
  }

  def dataOutOfWord(inData: BigInt, cnt: Int): BigInt = {
    inData >> (cnt * wordBits)
  }

  def maskConcatBeat(oldMask: BigInt, inMask: BigInt, cnt: Int): BigInt = {
    oldMask | (inMask << (cnt * l2params.beatBytes))
  }

  def maskConcatWord(oldMask: BigInt, inMask: BigInt, cnt: Int): BigInt = {
    oldMask | (inMask << (cnt * wordBytes))
  }

  def maskOutOfWord(mask: BigInt, cnt: Int): BigInt = {
    mask >> (cnt * wordBytes)
  }

  def beatInBlock(addr: BigInt): Int = {
    ((addr & offsetMask) >> beatAddrBits).toInt
  }

  def wordInBlock(addr: BigInt): Int = {
    ((addr & offsetMask) >> wordAddrBits).toInt
  }

  def genWordMaskInBlock(addr: BigInt, wordMask: BigInt): BigInt = {
    maskConcatWord(0, wordMask, wordInBlock(addr))
  }

  def randomBlockData(): BigInt = {
    (0 until blockWords).foldLeft(BigInt(0))(
      (d, _) => (d << 64) | (rand.nextLong() & 0x7fffffffffffffffL)
    )
  }

  def appendSerial(t: TLCTrans): Unit = {
    /*serialList.synchronized {
      serialList.append((ID, t))
    }*/
  }

  //only for master Get
  def insertVersionRead(addr: BigInt, ver: BigInt): BigInt = {
    val alignAddr = addrAlignBlock(addr)
    if (ver == 0) //from l2, just read scoreboard
      scoreboardRead(alignAddr)
    else //from l3, need match version
      scoreboardPeekMatchData(alignAddr, ver)
  }

  def insertMaskedReadSnap(addr: BigInt, readData: BigInt, snapData: BigInt, byteMask: BigInt): Unit = {
    //addr and mask must be aligned to block
    val alignAddr = addrAlignBlock(addr)
    val start_beat = beatInBlock(addr)
    val alignData = dataConcatBeat(0, readData, start_beat)
    val alignMask = maskConcatBeat(0, byteMask, start_beat)
    val addrState = getState(alignAddr)
    addrState.data = writeMaskedData(addrState.data, alignData, alignMask)
    val sbData = snapData
    val checkWriteData = writeMaskedData(sbData, alignData, alignMask)
    debugPrintln(f"MaskedRead, Addr: $alignAddr%x , own data: $alignData%x , sbData:$sbData%x , mask:$alignMask%x")
    assert(sbData == checkWriteData, f"agent $ID data has been changed, Addr: $alignAddr%x, " +
      f"own data: $alignData%x , scoreboard data: $sbData%x , mask:$alignMask%x")
  }

  //core Agent always read latest version
  def insertMaskedWordRead(addr: BigInt, readWordData: BigInt, wordByteMask: BigInt): Unit = {
    //addr and mask must be aligned to block
    val alignAddr = addrAlignBlock(addr)
    val start_word = wordInBlock(addr)
    val alignData = dataConcatWord(0, readWordData, start_word)
    val alignMask = maskConcatWord(0, wordByteMask, start_word)
    val addrState = getState(alignAddr)
    addrState.data = writeMaskedData(addrState.data, alignData, alignMask)
    val sbData = insertVersionRead(addr, 0)
    val checkWriteData = writeMaskedData(sbData, alignData, alignMask)
    debugPrintln(f"MaskedRead, Addr: $alignAddr%x , own data: $alignData%x , sbData:$sbData%x , mask:$alignMask%x")
    assert(sbData == checkWriteData, f"agent $ID data has been changed, Addr: $alignAddr%x, " +
      f"own data: $alignData%x , scoreboard data: $sbData%x , mask:$alignMask%x")
  }

  //for Put
  def insertMaskedWrite(addr: BigInt, newData: BigInt, byteMask: BigInt): Unit = {
    //addr and mask must be aligned to block
    val alignAddr = addrAlignBlock(addr)
    val start_beat = beatInBlock(addr)
    val alignData = dataConcatBeat(0, newData, start_beat)
    val alignMask = maskConcatBeat(0, byteMask, start_beat)
    val addrState = getState(alignAddr)
    //new data
    val oldData = scoreboardRead(alignAddr)
    val res = writeMaskedData(oldData, alignData, alignMask)
    addrState.dirty = true
    addrState.data = res
    scoreboardWrite(alignAddr, res)
    debugPrintln(f"MaskedWrite, Addr: $alignAddr%x ,old sbData:$oldData%x , new sbData: $res%x , mask:$alignMask%x")
  }

  def insertMaskedWordWrite(addr: BigInt, newWordData: BigInt, wordByteMask: BigInt): Unit = {
    //addr and mask must be aligned to block
    val alignAddr = addrAlignBlock(addr)
    val start_word = wordInBlock(addr)
    val alignData = dataConcatWord(0, newWordData, start_word)
    val alignMask = maskConcatWord(0, wordByteMask, start_word)
    val addrState = getState(alignAddr)
    //new data
    val oldData = scoreboardRead(alignAddr)
    val res = writeMaskedData(oldData, alignData, alignMask)
    addrState.dirty = true
    addrState.data = res
    scoreboardWrite(alignAddr, res)
    debugPrintln(f"MaskedWrite, Addr: $alignAddr%x ,old sbData:$oldData%x , new sbData: $res%x , mask:$alignMask%x")
  }

  //full block read
  def insertRead(addr: BigInt, readData: BigInt): Unit = {
    //Do not call masked read for performance
    val addrState = getState(addr)
    addrState.data = readData
    val sbData = scoreboardRead(addr)
    debugPrintln(f"insertFullBlockRead, Addr: $addr%x ,own data: $readData%x")
    assert(readData == sbData, f"agent $ID data has been changed, Addr: $addr%x, " +
      f"own data: $readData%x , scoreboard data: $sbData%x , with full mask")
  }

  //read block with snapshot
  def insertReadSnap(addr: BigInt, readData: BigInt, snapData: BigInt): Unit = {
    val addrState = getState(addr)
    addrState.data = readData
    val sbData = snapData
    debugPrintln(f"insertFullBlockRead, Addr: $addr%x ,own data: $readData%x")
    assert(readData == sbData, f"agent $ID data has been changed, Addr: $addr%x, " +
      f"own data: $readData%x , scoreboard data: $sbData%x , with full mask")
  }

  //full block write, only write new data
  def insertFullWrite(addr: BigInt, newData: BigInt): Unit = {
    //Do not call masked write for performance
    val addrState = getState(addr)
    //new data
    addrState.dirty = true
    addrState.data = newData
    scoreboardWrite(addr, newData)
    debugPrintln(f"insertFullBlockWrite, Addr: $addr%x ,new sbData: $newData%x")
  }

  //full block read & write, check old data before write new data
  def insertReadWrite(addr: BigInt, readData: BigInt, newData: BigInt): Unit = {
    //check old data
    insertRead(addr, readData)
    //new data
    insertFullWrite(addr, newData)
  }

  def insertReadSnapWrite(addr: BigInt, readData: BigInt, snapData: BigInt, newData: BigInt): Unit = {
    //check old data
    insertReadSnap(addr, readData, snapData)
    //new data
    insertFullWrite(addr, newData)
  }

  def scoreboardRead(addr: BigInt): BigInt = {
    scoreboard.synchronized {
      if (!scoreboard.contains(addr))
        scoreboard(addr) = new ScoreboardData()
      scoreboard(addr).mainData
    }
  }

  //for master checking read data with version
  def scoreboardPeekMatchData(addr: BigInt, ver: BigInt): BigInt = {
    scoreboard.synchronized {
      scoreboard(addr).peekMatchVersion(ver)
    }
  }

  def scoreboardGetVer(addr: BigInt): BigInt = {
    scoreboard.synchronized {
      scoreboard(addr).getReadDataVersion()
    }
  }

  def scoreboardWrite(addr: BigInt, data: BigInt): Unit = {
    scoreboard.synchronized {
      scoreboard(addr).writeNewData(data)
    }
  }

}

class TLCSlaveAgent(ID: Int, name: String = "", val maxSink: Int, addrStateMap: mutable.Map[BigInt, AddrState], serialList: ArrayBuffer[(Int, TLCTrans)]
                    , scoreboard: mutable.Map[BigInt, ScoreboardData])
                   (implicit p: Parameters)
  extends TLCAgent(ID, name, addrStateMap, serialList, scoreboard) {
  val innerAcquire = ListBuffer[AcquireCalleeTrans]()
  val innerRelease = ListBuffer[ReleaseCalleeTrans]()
  val innerProbe = ListBuffer[ProbeCallerTrans]()

  val innerGet: mutable.Queue[GetCalleeTrans] = mutable.Queue[GetCalleeTrans]()
  val innerPut: mutable.Queue[PutCalleeTrans] = mutable.Queue[PutCalleeTrans]()

  val sinkIdMap = mutable.Map[BigInt, AcquireCalleeTrans]()
  val sinkFreeQueue = mutable.Queue[BigInt]()

  def freeSink(): Unit = {
    sinkFreeQueue.dequeueAll { ID =>
      sinkIdMap.remove(ID)
      true
    }
  }

  override def transStep(): Unit = {
    innerAcquire.foreach(_.step())
    innerRelease.foreach(_.step())
    innerProbe.foreach(_.step())
    innerPut.foreach(_.step())
    innerGet.foreach(_.step())
  }

  override def getState(addr: BigInt): AddrState = {
    val state = addrStateMap.getOrElse(addr, new AddrState())
    if (!addrStateMap.contains(addr)) { //alloc new state if need
      state.myPerm = trunk
      addrStateMap += (addr -> state)
      if (!scoreboard.contains(addr)) { //alloc scoreboard if needed
        scoreboard += (addr -> new ScoreboardData())
      }
    }
    state
  }

  def permAgainstMaster(masterPerm: BigInt): BigInt = {
    if (masterPerm == trunk)
      nothing
    else if (masterPerm == branch)
      branch
    else
      trunk
  }

  var tmpA = new TLCScalaA()
  var a_cnt = 0
  var a_cnt_end = 0

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
    //    sinkIdMap.remove(inE.sink)
    sinkFreeQueue.enqueue(inE.sink)
    //remove from addrList and agentList
    state.calleeTrans -= acq
    innerAcquire -= acq
  }

  def issueD(): Unit = {
    //serach AccessAck(Data) to issue
    innerGet.dequeueAll { g =>
      val addr = g.a.get.address
      val alignAddr = addrAlignBlock(addr)
      val state = getState(alignAddr)
      if (state.myPerm == nothing) {
        debugPrintln(f"can't handle Get at $addr%x")
        false
      }
      else {
        val ver = scoreboardGetVer(alignAddr)
        val start_beat = beatInBlock(addr)
        val targetData = dataOutOfBeat(state.data, start_beat)
        //        println(f"issue AccessAckData, addr:$addr%x, data:$targetData, size:${g.a.get.size}, " +
        //          f"beats:${countBeats(g.a.get.size)}, ver:$ver")
        dQueue.enqMessage(g.issueAccessAckData(targetData, ver), countBeats(g.a.get.size))
        true
      }
    }
    innerPut.dequeueAll { p =>
      val addr = p.a.get.address
      val alignAddr = addrAlignBlock(addr)
      val state = getState(alignAddr)
      if (state.myPerm != trunk) {
        debugPrintln(f"can't handle Put at $addr%x")
        false
      }
      else {
        insertMaskedWrite(p.a.get.address, p.a.get.data, p.a.get.mask)
        dQueue.enqMessage(p.issueAccessAck())
        true
      }
    }
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
        val c = r.c.get
        assert(permCmp(shrinkFrom(c.param), state.masterPerm) <= 0,
          f"addr: $addr%x, recorded master perm: ${state.masterPerm}, param:${c.param} , shrink from ${shrinkFrom(c.param)}")
        state.masterPerm = shrinkTarget(r.c.get.param)
        state.myPerm = permAgainstMaster(state.masterPerm)
        if (r.c.get.opcode == ReleaseData) {
          state.data = r.c.get.data
          if (state.masterPerm == nothing) {
            insertReadWrite(addr, r.c.get.data, randomBlockData()) //modify data when master is invalid
          }
          else {
            insertRead(addr, r.c.get.data)
          }
        }
        else {
          if (state.masterPerm == nothing) {
            insertReadWrite(addr, state.data, randomBlockData()) //modify data when master is invalid
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
              if (state.masterPerm == branch && growFrom(a_acq.param) == branch) { //grow from branch
                dQueue.enqMessage(acq.issueGrant(allocId))
              }
              else {
                dQueue.enqMessage(acq.issueGrantData(allocId, state.data), cnt = beatNum)
              }
            }
            //update state
            state.masterPerm = growTarget(a_acq.param)
            state.myPerm = permAgainstMaster(state.masterPerm)
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

  var sbDataSnapshot: BigInt = 0

  def fireC(inC: TLCScalaC): Unit = {
    if (inC.opcode == ReleaseData || inC.opcode == ProbeAckData) {
      if (c_cnt == 0) { //start burst
        sbDataSnapshot = scoreboardRead(inC.address)
        tmpC = inC.copy()
        c_cnt += 1
      }
      else { //burst beat
        tmpC.data = tmpC.data | inC.data << (beatBits * c_cnt)
        c_cnt += 1
        if (c_cnt == beatNum) {
          handleC(tmpC)
          c_cnt = 0
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
        //TODO: only one master for now, so no need to check source
        val probeT = innerProbe.filter(p => p.probeAckPending.getOrElse(false)).filter(p => p.b.get.address == addr).head
        //pair ProbeAck
        probeT.pairProbeAck(c)
        //update state
        assert(permCmp(shrinkFrom(c.param), state.masterPerm) <= 0, f"addr: $addr%x, recorded master perm: ${state.masterPerm}, param:${c.param} , shrink from ${shrinkFrom(c.param)}")
        state.masterPerm = shrinkTarget(c.param)
        state.myPerm = permAgainstMaster(state.masterPerm)
        state.slaveUpdatePendingProbeAck()
        if (state.myPerm == trunk) {
          insertReadWrite(addr, state.data, randomBlockData()) //modify data when master is invalid
        }
        else if (state.myPerm == branch) {
          insertRead(addr, state.data)
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
        //TODO: only one master for now, so no need to check source
        val probeT = innerProbe.filter(p => p.probeAckPending.getOrElse(false)).filter(p => p.b.get.address == addr).head//pair ProbeAck
        //pair ProbeAck
        probeT.pairProbeAck(c)
        //update state
        assert(permCmp(shrinkFrom(c.param), state.masterPerm) <= 0, f"addr: $addr%x, recorded master perm: ${state.masterPerm}, param:${c.param} , shrink from ${shrinkFrom(c.param)}")
        state.masterPerm = shrinkTarget(c.param)
        state.myPerm = permAgainstMaster(state.masterPerm)
        state.data = c.data
        state.slaveUpdatePendingProbeAck()
        if (state.myPerm == trunk) {
          insertReadSnapWrite(addr, c.data, sbDataSnapshot, randomBlockData()) //modify data when master is invalid
        }
        else if (state.myPerm == branch) {
          insertReadSnap(addr, c.data, sbDataSnapshot)
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
        val rel = new ReleaseCalleeTrans()
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
    if (inA.opcode == PutFullData || inA.opcode == PutPartialData) {
      if (a_cnt == 0) { //start burst
        a_cnt_end = countBeats(inA.size)
        tmpA = inA.copy()
        a_cnt += 1
      }
      else {
        tmpA.mask = maskConcatBeat(tmpA.mask, inA.mask, a_cnt)
        tmpA.data = dataConcatBeat(tmpA.data, inA.data, a_cnt)
        a_cnt += 1
      }
      if (a_cnt == a_cnt_end) {
        a_cnt = 0
        aList.append(tmpA)
      }
    }
    else
      aList.append(inA)
  }

  def tickA(): Unit = {
    aList --= aList.filter { a =>
      if (a.opcode == Get) {
        val getT = new GetCalleeTrans()
        val beats = countBeats(a.size)
        if (beats > 1) { //fill n mask if there are n beats
          a.mask = (0 until beats).foldLeft(BigInt(0))(
            (cmask, i) => maskConcatBeat(cmask, a.mask, i))
        }
        getT.pairGet(a)
        innerGet.enqueue(getT)
        true
      }
      else if (a.opcode == PutFullData || a.opcode == PutPartialData) {
        val putT = new PutCalleeTrans()
        putT.pairPut(a)
        innerPut.enqueue(putT)
        true
      }
      else {
        val addr = a.address
        val state = getState(addr)
        if (state.blockInnerAcquire()) { //blocking
          false
        }
        else { //not blocking
          val transA = new AcquireCalleeTrans()
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

  def addProbe(addr: BigInt, targetPerm: BigInt): Unit = {
    val pro = new ProbeCallerTrans()
    pro.prepareProbe(addr, targetPerm)
    innerProbe.append(pro)
  }

  override def step(): Unit = {
    freeSink()
    super.step()
  }
}

class TLCMasterAgent(ID: Int, name: String = "", val maxSource: Int, addrStateMap: mutable.Map[BigInt, AddrState], serialList: ArrayBuffer[(Int, TLCTrans)]
                     , scoreboard: mutable.Map[BigInt, ScoreboardData])
                    (implicit p: Parameters)
  extends TLCAgent(ID, name, addrStateMap, serialList, scoreboard) {
  val outerAcquire: ListBuffer[AcquireCallerTrans] = ListBuffer()
  val outerRelease: ListBuffer[ReleaseCallerTrans] = ListBuffer()
  val outerProbe: ListBuffer[ProbeCalleeTrans] = ListBuffer()

  val sourceAMap = mutable.Map[BigInt, AcquireCallerTrans]()
  val sourceCMap = mutable.Map[BigInt, ReleaseCallerTrans]()
  val sourceAFreeQueue = mutable.Queue[BigInt]()
  val sourceCFreeQueue = mutable.Queue[BigInt]()

  def freeSource(): Unit = {
    sourceAFreeQueue.dequeueAll { id =>
      sourceAMap.remove(id)
      true
    }
    sourceCFreeQueue.dequeueAll { id =>
      sourceCMap.remove(id)
      true
    }
  }

  override def transStep(): Unit = {
    outerAcquire.foreach(_.step())
    outerRelease.foreach(_.step())
    outerProbe.foreach(_.step())
  }

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
          d_cnt = 0
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
        //        sourceCMap.remove(d.source)
        sourceCFreeQueue.enqueue(d.source)
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
          if (acq.a.get.opcode == AcquireBlock) {
            if (state.myPerm == trunk) {
              insertReadWrite(addr, state.data, randomBlockData()) //modify data when trunk
            }
            else if (state.myPerm == branch) {
              insertRead(addr, state.data)
            }
          }
          else { //acquire permssion, used for full write
            if (state.myPerm == trunk) {
              insertFullWrite(addr, randomBlockData()) //modify data when trunk
            }
          }
        }
        //issue GrantAck
        eQueue.enqMessage(acq.issueGrantAck())
        //update state
        state.masterUpdatePendingGrant()
        //free sourceID
        //        sourceAMap.remove(d.source)
        sourceAFreeQueue.enqueue(d.source)
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
            insertReadWrite(addr, d.data, randomBlockData()) //modify data when trunk
          }
          else {
            insertRead(addr, d.data)
          }
        }
        //issue GrantAck
        eQueue.enqMessage(acq.issueGrantAck())
        //update state
        state.masterUpdatePendingGrant()
        //free sourceID
        //        sourceAMap.remove(d.source)
        sourceAFreeQueue.enqueue(d.source)
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
          //change state permission
          state.myPerm = targetPerm
          //remove from addr list and agent list
          state.calleeTrans -= p
          true //condition to remove from agent list
        }
      }
    }

    //search Release here
    val abandonList = ListBuffer[ReleaseCallerTrans]()
    if (sourceCMap.size < maxSource) { //fast check available ID
      val sourceQ = mutable.Queue() ++ List.tabulate(maxSource)(a => BigInt(a)).filterNot(k => sourceCMap.contains(k))
      outerRelease.foreach { r =>
        if (!r.releaseIssued.getOrElse(true)) { //haven't issue release
          val addr = r.c.get.address
          val state = getState(addr)
          if (state.myPerm != nothing) { //have some thing to report
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
          else { // no need to report/shrink. Delete from release trans list
            abandonList += r
            //TODO: check father here
          }
        }
      }
      outerRelease --= abandonList
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
      outerAcquire --= abandonList
    }
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

  def addAcquire(addr: BigInt, targetPerm: BigInt): Unit = {
    val acq = new AcquireCallerTrans()
    acq.prepareAcquire(addr, targetPerm)
    outerAcquire.append(acq)
  }

  def addRelease(addr: BigInt, targetPerm: BigInt): Unit = {
    val rel = new ReleaseCallerTrans()
    rel.prepareRelease(addr, targetPerm)
    outerRelease.append(rel)
  }

  override def step(): Unit = {
    freeSource()
    super.step()
  }

}