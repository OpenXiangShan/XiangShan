package oceanus.compactchi

import chisel3._
import chisel3.util._

class FlitEVTStripped extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by L1 parameter
  val SrcID = UInt(8.W) // TODO: configured by UpstreamNodeID_Width
  val TgtID = UInt(8.W) // TODO: configured by DownstreamNodeID_Width
  val Opcode = UInt(1.W)
  val NS = Bool()
  val WayValid = Bool()
  val Way = UInt(2.W) // TODO: configured by L2 way count
  val TraceTag = UInt(1.W)
}

class FlitEVT extends FlitEVTStripped {
  val Addr = UInt(48.W)
}

class FlitREQStripped extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by L1 parameter
  val SrcID = UInt(8.W) // TODO: configured by UpstreamNodeID_Width
  val TgtID = UInt(8.W) // TODO: configured by DownstreamNodeID_Width
  val Opcode = UInt(6.W) // TODO: variable width between different types of components
  val Size = UInt(3.W)
  val NS = Bool()
  val Order = UInt(2.W)
  val MemAttr = UInt(4.W)
  val Excl = Bool()
  val ExpCompData = Bool()
  def ExpCompStash = ExpCompData
  val WayValid = Bool()
  val Way = UInt(2.W) // TODO: configured by L2 way count
  val TraceTag = UInt(1.W)
}

class FlitREQ extends FlitREQStripped {
  val Addr = UInt(48.W)
  val alias = UInt(2.W)  // TODO: configurable by TagAlias_Width
}

class FlitSNPStripped extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by L2 parameter
  val SrcID = UInt(8.W) // TODO: configured by DownstreamNodeID_Width
  val TgtID = UInt(8.W) // TODO: configured by UpstreamNodeID_Width
  val Opcode = UInt(2.W) // TODO: variable width between different types of components
  val NS = Bool()
  val TraceTag = UInt(1.W)
}

class FlitSNP extends FlitSNPStripped {
  val Addr = UInt((48 - 3).W)
  val alias = UInt(2.W) // L2->L1 snoop locator; pprobe uses localMeta.alias, rprobe uses req.alias
}

class FlitDnRSP extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by L1 parameter
  val SrcID = UInt(8.W) // TODO: configured by DownstreamNodeID_Width
  val TgtID = UInt(8.W) // TODO: configured by UpstreamNodeID_Width
  val DBID = UInt(8.W) // TODO: configured by L2 parameter
  val Opcode = UInt(3.W) // TODO: variable width between different types of components
  val RespErr = UInt(2.W)
  val Resp = UInt(3.W)
  val CBusy = UInt(3.W)
  val WayValid = Bool()
  val Way = UInt(2.W) // TODO: configured by L2 way count
  val TraceTag = UInt(1.W)
}

class FlitUpRSP extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by L2 parameter
  val SrcID = UInt(8.W) // TODO: configured by UpstreamNodeID_Width
  val TgtID = UInt(8.W) // TODO: configured by DownstreamNodeID_Width
  val Opcode = UInt(1.W)
  val RespErr = UInt(2.W)
  val Resp = UInt(3.W)
  val TraceTag = UInt(1.W)
}

class FlitDnDATWithoutData extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by L1 parameter
  val SrcID = UInt(8.W) // TODO: configured by DownstreamNodeID_Width
  val TgtID = UInt(8.W) // TODO: configured by UpstreamNodeID_Width
  val DBID = UInt(8.W) // TODO: configured by L2 parameter
  val Opcode = UInt(1.W)
  val RespErr = UInt(2.W)
  val Resp = UInt(3.W)
  val DataID = UInt(2.W)
  val DataSource = UInt(5.W)
  val CBusy = UInt(3.W)
  val WayValid = Bool()
  val Way = UInt(2.W) // TODO: configured by L2 way count
  val TraceTag = UInt(1.W)
}

class FlitDnDAT extends FlitDnDATWithoutData {
  val Data = UInt(256.W)
}

class FlitUpDATWithoutData extends Bundle {
  val TxnID = UInt(8.W) // TODO: configured by maximum value of L1 parameter and L2 parameter
  val SrcID = UInt(8.W) // TODO: configured by UpstreamNodeID_Width
  val TgtID = UInt(8.W) // TODO: configured by DownstreamNodeID_Width
  val Opcode = UInt(2.W)
  val RespErr = UInt(2.W)
  val Resp = UInt(3.W)
  val DataID = UInt(2.W)
  val TraceTag = UInt(1.W)
}

class FlitUpDAT extends FlitUpDATWithoutData {
  val Data = UInt(256.W)
  val BE = UInt(32.W)
}
