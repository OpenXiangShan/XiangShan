package cache.TLCTest

import freechips.rocketchip.tilelink.TLMessages

import scala.collection.mutable.{Map, Seq, ListBuffer}

class TLCScalaMessage {
  var trans: Option[TLCTrans] = None
}

class TLCScalaA
(
  var opcode: BigInt = 0,
  var param: BigInt = 0,
  var size: BigInt = 0,
  var source: BigInt = 0,
  var address: BigInt = 0,
  var mask: BigInt = 0,
  var data: BigInt = 0,
) extends TLCScalaMessage {
  def copy(
            newOpcode: BigInt = opcode,
            newParam: BigInt = param,
            newSize: BigInt = size,
            newSource: BigInt = source,
            newAddress: BigInt = address,
            newMask: BigInt = mask,
            newData: BigInt = data,
          ): TLCScalaA = new TLCScalaA(
    opcode = newOpcode,
    param = newParam,
    size = newSize,
    source = newSource,
    address = newAddress,
    mask = newMask,
    data = newData,
  )
}

class TLCScalaC
(
  var opcode: BigInt = 0,
  var param: BigInt = 0,
  var size: BigInt = 0,
  var source: BigInt = 0,
  var address: BigInt = 0,
  var data: BigInt = 0,
) extends TLCScalaMessage {
  def copy(
            newOpcode: BigInt = opcode,
            newParam: BigInt = param,
            newSize: BigInt = size,
            newSource: BigInt = source,
            newAddress: BigInt = address,
            newData: BigInt = data,
          ): TLCScalaC = new TLCScalaC(
    opcode = newOpcode,
    param = newParam,
    size = newSize,
    source = newSource,
    address = newAddress,
    data = newData,
  )
}

class TLCScalaE
(
  var sink: BigInt = 0,
) extends TLCScalaMessage {
  def copy(
            newSink: BigInt = sink
          ): TLCScalaE = new TLCScalaE(
    sink = newSink
  )
}

class TLCScalaB
(
  var opcode: BigInt = 0,
  var param: BigInt = 0,
  var size: BigInt = 0,
  var source: BigInt = 0,
  var address: BigInt = 0,
  var mask: BigInt = 0,
  var data: BigInt = 0,
) extends TLCScalaMessage {
  def copy(
            newOpcode: BigInt = opcode,
            newParam: BigInt = param,
            newSize: BigInt = size,
            newSource: BigInt = source,
            newAddress: BigInt = address,
            newMask: BigInt = mask,
            newData: BigInt = data,
          ): TLCScalaB = new TLCScalaB(
    opcode = newOpcode,
    param = newParam,
    size = newSize,
    source = newSource,
    address = newAddress,
    mask = newMask,
    data = newData,
  )
}

class TLCScalaD
(
  var opcode: BigInt = 0,
  var param: BigInt = 0,
  var size: BigInt = 0,
  var source: BigInt = 0,
  var sink: BigInt = 0,
  var denied: Boolean = false,
  var data: BigInt = 0,
) extends TLCScalaMessage {
  def copy(
            newOpcode: BigInt = opcode,
            newParam: BigInt = param,
            newSize: BigInt = size,
            newSource: BigInt = source,
            newSink: BigInt = sink,
            newDenied: Boolean = denied,
            newData: BigInt = data,
          ): TLCScalaD = new TLCScalaD(
    opcode = newOpcode,
    param = newParam,
    size = newSize,
    source = newSource,
    sink = newSink,
    denied = newDenied,
    data = newData,
  )
}

trait TLCOp {
  //make BigInt parameters
  val PutFullData = TLMessages.PutFullData.litValue()
  val PutPartialData = TLMessages.PutPartialData.litValue()
  val ArithmeticData = TLMessages.ArithmeticData.litValue()
  val LogicalData = TLMessages.LogicalData.litValue()
  val Get = TLMessages.Get.litValue()
  val Hint = TLMessages.Hint.litValue()
  val AcquireBlock = TLMessages.AcquireBlock.litValue()
  val AcquirePerm = TLMessages.AcquirePerm.litValue()
  val Probe = TLMessages.Probe.litValue()
  val AccessAck = TLMessages.AccessAck.litValue()
  val AccessAckData = TLMessages.AccessAckData.litValue()
  val HintAck = TLMessages.HintAck.litValue()
  val ProbeAck = TLMessages.ProbeAck.litValue()
  val ProbeAckData = TLMessages.ProbeAckData.litValue()
  val Release = TLMessages.Release.litValue()
  val ReleaseData = TLMessages.ReleaseData.litValue()
  val Grant = TLMessages.Grant.litValue()
  val GrantData = TLMessages.GrantData.litValue()
  val ReleaseAck = TLMessages.ReleaseAck.litValue()
  val GrantAck = TLMessages.GrantAck.litValue()

  val toT = BigInt(0)
  val toB = BigInt(1)
  val toN = BigInt(2)

  val NtoB = BigInt(0)
  val NtoT = BigInt(1)
  val BtoT = BigInt(2)

  val TtoB = BigInt(0)
  val TtoN = BigInt(1)
  val BtoN = BigInt(2)

  val TtoT = BigInt(3)
  val BtoB = BigInt(4)
  val NtoN = BigInt(5)

  val trunk = toT
  val branch = toB
  val nothing = toN
}


trait PermissionTransition extends TLCOp {
  def growTarget(param: BigInt): BigInt = {
    param match {
      case NtoB => toB
      case NtoT => toT
      case BtoT => toT
    }
  }

  def growFrom(param: BigInt): BigInt = {
    param match {
      case NtoB => nothing
      case NtoT => nothing
      case BtoT => branch
    }
  }

  def needGrow(from: BigInt, to: BigInt): Boolean = {
    from > to
  }

  def growParam(from: BigInt, to: BigInt): BigInt = {
    assert(from > to, "no need to grow")
    if (from == nothing) {
      if (to == branch)
        NtoB
      else
        NtoT
    }
    else
      BtoT
  }

  def shrinkTarget(param: BigInt): BigInt = {
    param match {
      case TtoB => toB
      case TtoN => toN
      case BtoN => toN
      case TtoT => toT
      case BtoB => toB
      case NtoN => toN
    }
  }

  def shrinkFrom(param: BigInt): BigInt = {
    param match {
      case TtoB => trunk
      case TtoN => trunk
      case BtoN => branch
      case TtoT => trunk
      case BtoB => branch
      case NtoN => nothing
    }
  }

  def shrinkParam(from: BigInt, to: BigInt): BigInt = {
    assert(from <= to, "shrink to higher permission")
    if (from == trunk) {
      if (to == trunk)
        TtoT
      else if (to == branch)
        TtoB
      else
        TtoN
    } else if (from == branch) {
      if (to == branch)
        BtoB
      else
        BtoN
    }
    else
      NtoN
  }

  //0 if same perm, -1 if b is higher, 1 if a is higer
  def permCmp(aPerm: BigInt, bPerm: BigInt): Int = {
    if (aPerm == bPerm)
      0
    else if (aPerm > bPerm)
      -1
    else
      1
  }
}

//Transaction meta data will hide in start message
abstract class TLCTrans extends TLCOp with PermissionTransition with BigIntExtract {
  val blockSizeL2 = BigInt(6)
  val beatFullMask = BigInt(prefix ++ Array.fill(4)(0xff.toByte))

  private var timer = 0
  private var timerRunning = false

  def step(): Unit = {
    if (timerRunning) {
      timer += 1
      assert(timer <= 1000, "transaction time out!")
    }
  }

  def startTimer(): Unit = {
    timer = 0
    timerRunning = true
  }

  def resetTimer(): Unit = {
    timer = 0
    timerRunning = false
  }
}

trait TLCCallerTrans extends TLCTrans {
  var transDepend: Option[TLCTrans] = None
}

trait TLCCalleeTrans extends TLCTrans {
  var recursiveTrans: Option[ListBuffer[TLCTrans]] = None
}

//Acquire
class AcquireTrans extends TLCTrans {
  var a: Option[TLCScalaA] = None
  var d: Option[TLCScalaD] = None
  var e: Option[TLCScalaE] = None

}

class AcquireCallerTrans() extends AcquireTrans with TLCCallerTrans {
  var acquireIssued: Option[Boolean] = None
  var grantPending: Option[Boolean] = None
  var grantAckIssued: Option[Boolean] = None

  var targetPerm: BigInt = nothing

  //record metaData in Acquire Message
  def prepareAcquire(reqAddr: BigInt, reqTargetPerm: BigInt): Unit = {
    val genA = new TLCScalaA(
      size = blockSizeL2,
      address = reqAddr,
      mask = beatFullMask,
    )
    a = Some(genA)
    acquireIssued = Some(false)
    targetPerm = reqTargetPerm
  }

  def checkNeedGrow(nowPerm: BigInt): Boolean = {
    needGrow(nowPerm, targetPerm)
  }

  def issueAcquirePerm(sourceMapId: BigInt, nowPerm: BigInt): TLCScalaA = {
    a.get.opcode = AcquirePerm
    a.get.source = sourceMapId
    a.get.param = growParam(nowPerm, targetPerm)
    acquireIssued = Some(true)
    grantPending = Some(true)
    a.get.trans = Some(this)
    a.get
  }

  def issueAcquireBlock(sourceMapId: BigInt, nowPerm: BigInt): TLCScalaA = {
    a.get.opcode = AcquireBlock
    a.get.source = sourceMapId
    a.get.param = growParam(nowPerm, targetPerm)
    acquireIssued = Some(true)
    grantPending = Some(true)
    a.get.trans = Some(this)
    a.get
  }

  def pairGrant(inD: TLCScalaD): Unit = {
    d = Some(inD)
    grantPending = Some(false)
    grantAckIssued = Some(false)
    resetTimer()
  }

  def issueGrantAck(): TLCScalaE = {
    assert(d.isDefined, "miss grant in AcquireTransaction")
    val genE = new TLCScalaE(sink = d.get.sink)
    e = Some(genE)
    grantAckIssued = Some(true)
    e.get
  }

}

class AcquireCalleeTrans() extends AcquireTrans with TLCCalleeTrans {
  var grantIssued: Option[Boolean] = None
  var grantAckPending: Option[Boolean] = None

  def pairAcquire(aIn: TLCScalaA): Unit = {
    a = Some(aIn)
    grantIssued = Some(false)
  }

  def issueGrant(sinkMapId: BigInt): TLCScalaD = {
    assert(a.isDefined, "miss acquire in AcquireTransaction")
    val genD = new TLCScalaD(
      opcode = Grant,
      param = growTarget(a.get.param),
      size = a.get.size,
      source = a.get.source,
      sink = sinkMapId,
      denied = false,
    )
    d = Some(genD)
    grantIssued = Some(true)
    grantAckPending = Some(true)
    d.get.trans = Some(this)
    d.get
  }

  def issueGrantData(sinkMapId: BigInt, inData: BigInt): TLCScalaD = {
    assert(a.isDefined, "miss acquire in AcquireTransaction")
    val genD = new TLCScalaD(
      opcode = GrantData,
      param = growTarget(a.get.param),
      size = a.get.size,
      source = a.get.source,
      sink = sinkMapId,
      denied = false,
      data = inData,
    )
    d = Some(genD)
    grantIssued = Some(true)
    grantAckPending = Some(true)
    d.get.trans = Some(this)
    d.get
  }

  def pairGrantAck(inE: TLCScalaE): Unit = {
    e = Some(inE)
    grantAckPending = Some(false)
    resetTimer()
  }

}

//probe
class ProbeTrans extends TLCTrans with PermissionTransition {
  var b: Option[TLCScalaB] = None
  var c: Option[TLCScalaC] = None
}

class ProbeCallerTrans() extends ProbeTrans with TLCCallerTrans {
  var probeIssued: Option[Boolean] = None
  var probeAckPending: Option[Boolean] = None

  //record metaData in Probe Message
  def prepareProbe(reqAddr: BigInt, reqTargetPerm: BigInt, targetSource: BigInt = 0): Unit = {
    val genB = new TLCScalaB(
      opcode = Probe,
      param = reqTargetPerm,
      size = blockSizeL2,
      source = targetSource,
      address = reqAddr,
      mask = beatFullMask,
    )
    b = Some(genB)
    probeIssued = Some(false)
  }

  def issueProbe(): TLCScalaB = {
    probeIssued = Some(true)
    probeAckPending = Some(true)
    b.get.trans = Some(this)
    b.get
  }

  def pairProbeAck(inC: TLCScalaC): Unit = {
    c = Some(inC)
    probeAckPending = Some(false)
    resetTimer()
  }

}

case class ProbeCalleeTrans() extends ProbeTrans with TLCCalleeTrans {
  var probeAckIssued: Option[Boolean] = None

  def pairProbe(bIn: TLCScalaB): Unit = {
    b = Some(bIn)
    probeAckIssued = Some(false)
  }

  def issueProbeAck(from: BigInt, to: BigInt): TLCScalaC = {
    assert(b.isDefined, "miss probe in ProbeTransaction")
    val genC = new TLCScalaC(
      opcode = ProbeAck,
      param = shrinkParam(from, to),
      size = b.get.size,
      source = b.get.source,
      address = b.get.address
    )
    c = Some(genC)
    probeAckIssued = Some(true)
    c.get
  }

  def issueProbeAckData(from: BigInt, to: BigInt, inData: BigInt): TLCScalaC = {
    assert(b.isDefined, "miss probe in ProbeTransaction")
    val genC = new TLCScalaC(
      opcode = ProbeAckData,
      param = shrinkParam(from, to),
      size = b.get.size,
      source = b.get.source,
      address = b.get.address,
      data = inData,
    )
    c = Some(genC)
    probeAckIssued = Some(true)
    c.get
  }
}


class ReleaseTrans extends TLCTrans with PermissionTransition {
  var c: Option[TLCScalaC] = None
  var d: Option[TLCScalaD] = None
}

class ReleaseCallerTrans() extends ReleaseTrans with TLCCallerTrans {
  var releaseIssued: Option[Boolean] = None
  var releaseAckPending: Option[Boolean] = None

  var targetPerm: BigInt = nothing

  //record metaData in Release Message
  def prepareRelease(reqAddr: BigInt, reqTargetPerm: BigInt): Unit = {
    val genC = new TLCScalaC(
      size = blockSizeL2,
      address = reqAddr,
    )
    c = Some(genC)
    releaseIssued = Some(false)
    targetPerm = reqTargetPerm
  }

  def issueRelease(sourceMapId: BigInt, nowPerm: BigInt): TLCScalaC = {
    c.get.opcode = Release
    c.get.param = shrinkParam(nowPerm, targetPerm)
    c.get.source = sourceMapId
    releaseIssued = Some(true)
    releaseAckPending = Some(true)
    c.get.trans = Some(this)
    c.get
  }

  def issueReleaseData(sourceMapId: BigInt, nowPerm: BigInt, inData: BigInt): TLCScalaC = {
    c.get.opcode = ReleaseData
    c.get.param = shrinkParam(nowPerm, targetPerm)
    c.get.source = sourceMapId
    c.get.data = inData
    releaseIssued = Some(true)
    releaseAckPending = Some(true)
    c.get.trans = Some(this)
    c.get
  }

  def pairReleaseAck(inD: TLCScalaD): Unit = {
    d = Some(inD)
    releaseAckPending = Some(false)
    resetTimer()
  }

}

class ReleaseCalleeTrans() extends ReleaseTrans with TLCCalleeTrans {
  var releaseAckIssued: Option[Boolean] = None

  def pairRelease(inC: TLCScalaC): Unit = {
    c = Some(inC)
    releaseAckIssued = Some(false)
  }

  def issueReleaseAck(): TLCScalaD = {
    assert(c.isDefined, "miss release in ReleaseTransaction")
    d = Some(new TLCScalaD(
      opcode = ReleaseAck,
      param = 0,
      size = c.get.size,
      source = c.get.source,
      denied = false,
    ))
    releaseAckIssued = Some(true)
    d.get
  }
}

class GetTrans() extends TLCTrans {
  var a: Option[TLCScalaA] = None
  var d: Option[TLCScalaD] = None
}

class GetCallerTrans() extends GetTrans with TLCCallerTrans {
  var accessAckDataPending: Option[Boolean] = None

  def pairGet(inA: TLCScalaA): Unit = {
    a = Some(inA)
    accessAckDataPending = Some(true)
    startTimer()
  }

  def pairAccessAckData(inD: TLCScalaD): Unit = {
    d = Some(inD)
    accessAckDataPending = Some(false)
    resetTimer()
  }
}

class GetCalleeTrans() extends GetTrans with TLCCalleeTrans {
  var accessAckDataIssued: Option[Boolean] = None

  def pairGet(inA: TLCScalaA): Unit = {
    a = Some(inA)
    accessAckDataIssued = Some(false)
  }

  //outD is concated
  def issueAccessAckData(inData: BigInt, param: BigInt): TLCScalaD = {
    d = Some(new TLCScalaD(
      opcode = AccessAckData,
      param = param,
      size = a.get.size,
      source = a.get.source,
      sink = 0,
      data = inData
    ))
    accessAckDataIssued = Some(true)
    d.get
  }
}

class PutTrans() extends TLCTrans {
  var a: Option[TLCScalaA] = None
  var d: Option[TLCScalaD] = None
}

class PutCallerTrans() extends GetTrans with TLCCallerTrans {
  var accessAckPending: Option[Boolean] = None

  //inA will be concat in fireQueue
  def pairPut(inA: TLCScalaA): Unit = {
    a = Some(inA)
    accessAckPending = Some(true)
    startTimer()
  }

  def pairAccessAck(inD: TLCScalaD): Unit = {
    d = Some(inD)
    accessAckPending = Some(false)
    resetTimer()
  }
}

class PutCalleeTrans() extends GetTrans with TLCCalleeTrans {
  var accessAckIssued: Option[Boolean] = None

  //inA will be concat in fireQueue
  def pairPut(inA: TLCScalaA): Unit = {
    a = Some(inA)
    accessAckIssued = Some(false)
  }

  def issueAccessAck(): TLCScalaD = {
    d = Some(new TLCScalaD(
      opcode = AccessAck,
      param = 1,
      size = a.get.size,
      source = a.get.source,
      sink = 0,
      data = 0,
    ))
    accessAckIssued = Some(true)
    d.get
  }
}