// See LICENSE.SiFive for license details.

package bus.tilelink

import chisel3._
import chisel3.util._

import xiangshan.HasXSParameter

case class TLParameters(
  addressBits: Int = 64,
  dataBits: Int = 64,
  sourceBits: Int = 1,
  sinkBits: Int = 1,
  sizeBits: Int = 3,
  maxTransfer: Int = 64) {
    def beatBytes = dataBits / 8
    val maxLgSize = log2Ceil(maxTransfer)
  }


object TLMessages 
{
  // opcode width
  val width = 3 
  //                                  A    B    C    D    E
  def PutFullData    = 0.U(width.W) //     .    .                   => AccessAck
  def PutPartialData = 1.U(width.W) //     .    .                   => AccessAck
  def ArithmeticData = 2.U(width.W) //     .    .                   => AccessAckData
  def LogicalData    = 3.U(width.W) //     .    .                   => AccessAckData
  def Get            = 4.U(width.W) //     .    .                   => AccessAckData
  def Hint           = 5.U(width.W) //     .    .                   => HintAck
  def AcquireBlock   = 6.U(width.W) //     .                        => Grant[Data]
  def AcquirePerm    = 7.U(width.W) //     .                        => Grant[Data]
  def Probe          = 6.U(width.W) //          .                   => ProbeAck[Data]
  def AccessAck      = 0.U(width.W) //               .    .
  def AccessAckData  = 1.U(width.W) //               .    .
  def HintAck        = 2.U(width.W) //               .    .
  def ProbeAck       = 4.U(width.W) //               .
  def ProbeAckData   = 5.U(width.W) //               .
  def Release        = 6.U(width.W) //               .              => ReleaseAck
  def ReleaseData    = 7.U(width.W) //               .              => ReleaseAck
  def Grant          = 4.U(width.W) //                    .         => GrantAck
  def GrantData      = 5.U(width.W) //                    .         => GrantAck
  def ReleaseAck     = 6.U(width.W) //                    .
  def GrantAck       = 0.U(width.W) //                         .
 
  def isA(x: UInt) = x <= AcquirePerm
  def isB(x: UInt) = x <= Probe
  def isC(x: UInt) = x <= ReleaseData
  def isD(x: UInt) = x <= ReleaseAck

  def adResponse = VecInit(AccessAck, AccessAck, AccessAckData, AccessAckData, AccessAckData, HintAck, Grant, Grant)
  def bcResponse = VecInit(AccessAck, AccessAck, AccessAckData, AccessAckData, AccessAckData, HintAck, ProbeAck, ProbeAck)
  
  def a = Seq( ("PutFullData",TLPermissions.PermMsgReserved),
               ("PutPartialData",TLPermissions.PermMsgReserved),
               ("ArithmeticData",TLAtomics.ArithMsg),
               ("LogicalData",TLAtomics.LogicMsg),
               ("Get",TLPermissions.PermMsgReserved),
               ("Hint",TLHints.HintsMsg),
               ("AcquireBlock",TLPermissions.PermMsgGrow),
               ("AcquirePerm",TLPermissions.PermMsgGrow))

  def b = Seq( ("PutFullData",TLPermissions.PermMsgReserved),
               ("PutPartialData",TLPermissions.PermMsgReserved),
               ("ArithmeticData",TLAtomics.ArithMsg),
               ("LogicalData",TLAtomics.LogicMsg),
               ("Get",TLPermissions.PermMsgReserved),
               ("Hint",TLHints.HintsMsg),
               ("Probe",TLPermissions.PermMsgCap))

  def c = Seq( ("AccessAck",TLPermissions.PermMsgReserved),
               ("AccessAckData",TLPermissions.PermMsgReserved),
               ("HintAck",TLPermissions.PermMsgReserved),
               ("Invalid Opcode",TLPermissions.PermMsgReserved),
               ("ProbeAck",TLPermissions.PermMsgReport),
               ("ProbeAckData",TLPermissions.PermMsgReport),
               ("Release",TLPermissions.PermMsgReport),
               ("ReleaseData",TLPermissions.PermMsgReport))

  def d = Seq( ("AccessAck",TLPermissions.PermMsgReserved),
               ("AccessAckData",TLPermissions.PermMsgReserved),
               ("HintAck",TLPermissions.PermMsgReserved),
               ("Invalid Opcode",TLPermissions.PermMsgReserved),
               ("Grant",TLPermissions.PermMsgCap),
               ("GrantData",TLPermissions.PermMsgCap),
               ("ReleaseAck",TLPermissions.PermMsgReserved))

}

/**
  * The three primary TileLink permissions are:
  *   (T)runk: the agent is (or is on inwards path to) the global point of serialization.
  *   (B)ranch: the agent is on an outwards path to
  *   (N)one: 
  * These permissions are permuted by transfer operations in various ways.
  * Operations can cap permissions, request for them to be grown or shrunk,
  * or for a report on their current status.
  */
object TLPermissions
{
  val aWidth = 2
  val bdWidth = 2
  val cWidth = 3

  // Cap types (Grant = new permissions, Probe = permisions <= target)
  def toT = 0.U(bdWidth)
  def toB = 1.U(bdWidth)
  def toN = 2.U(bdWidth)
  def isCap(x: UInt) = x <= toN

  // Grow types (Acquire = permissions >= target)
  def NtoB = 0.U(aWidth)
  def NtoT = 1.U(aWidth)
  def BtoT = 2.U(aWidth)
  def isGrow(x: UInt) = x <= BtoT

  // Shrink types (ProbeAck, Release)
  def TtoB = 0.U(cWidth)
  def TtoN = 1.U(cWidth)
  def BtoN = 2.U(cWidth)
  def isShrink(x: UInt) = x <= BtoN

  // Report types (ProbeAck, Release)
  def TtoT = 3.U(cWidth)
  def BtoB = 4.U(cWidth)
  def NtoN = 5.U(cWidth)
  def isReport(x: UInt) = x <= NtoN

  def PermMsgGrow:Seq[String] = Seq("Grow NtoB", "Grow NtoT", "Grow BtoT")
  def PermMsgCap:Seq[String] = Seq("Cap toT", "Cap toB", "Cap toN")
  def PermMsgReport:Seq[String] = Seq("Shrink TtoB", "Shrink TtoN", "Shrink BtoN", "Report TotT", "Report BtoB", "Report NtoN")
  def PermMsgReserved:Seq[String] = Seq("Reserved") 
}

object TLAtomics
{
  val width = 3 

  // Arithmetic types
  def MIN  = 0.U(width)
  def MAX  = 1.U(width)
  def MINU = 2.U(width)
  def MAXU = 3.U(width)
  def ADD  = 4.U(width)
  def isArithmetic(x: UInt) = x <= ADD

  // Logical types
  def XOR  = 0.U(width)
  def OR   = 1.U(width)
  def AND  = 2.U(width)
  def SWAP = 3.U(width)
  def isLogical(x: UInt) = x <= SWAP

  def ArithMsg:Seq[String] = Seq("MIN", "MAX", "MINU", "MAXU", "ADD")
  def LogicMsg:Seq[String] = Seq("XOR", "OR", "AND", "SWAP")
}
 

object TLHints
{
  val width = 1

  def PREFETCH_READ  = 0.U(width)
  def PREFETCH_WRITE = 1.U(width)
  def isHints(x: UInt) = x <= PREFETCH_WRITE

  def HintsMsg:Seq[String] = Seq("PrefetchRead", "PrefetchWrite")
}

sealed trait TLChannel extends Bundle {
  val channelName: String
  val params: TLParameters
}

sealed trait TLDataChannel extends TLChannel
sealed trait TLAddrChannel extends TLDataChannel

class TLBundleA(override val params: TLParameters) extends TLAddrChannel
{
  val channelName = "'A' channel"
  val opcode  = UInt(3.W)
  val param   = UInt(3.W)
  val size    = UInt(params.sizeBits.W)
  val source  = UInt(params.sourceBits.W)
  val address = UInt(params.addressBits.W)
  val mask    = UInt((params.dataBits/8).W)
  val data    = UInt(params.dataBits.W)
  val corrupt = Bool()
}

class TLBundleB(override val params: TLParameters) extends TLAddrChannel
{
  val channelName = "'B' channel"
  val opcode  = UInt(3.W)
  val param   = UInt(3.W)
  val size    = UInt(params.sizeBits.W)
  val source  = UInt(params.sourceBits.W)
  val address = UInt(params.addressBits.W)
  val mask    = UInt((params.dataBits/8).W)
  val data    = UInt(params.dataBits.W)
  val corrupt = Bool()
}

class TLBundleC(override val params: TLParameters) extends TLAddrChannel
{
  val channelName = "'C' channel"
  val opcode  = UInt(3.W)
  val param   = UInt(3.W)
  val size    = UInt(params.sizeBits.W)
  val source  = UInt(params.sourceBits.W)
  val address = UInt(params.addressBits.W)
  val data    = UInt(params.dataBits.W)
  val corrupt = Bool()
}

class TLBundleD(override val params: TLParameters) extends TLDataChannel
{
  val channelName = "'D' channel"
  val opcode  = UInt(3.W)
  val param   = UInt(2.W)
  val size    = UInt(params.sizeBits.W)
  val source  = UInt(params.sourceBits.W)
  val sink    = UInt(params.sinkBits.W)
  val denied  = Bool()
  val data    = UInt(params.dataBits.W)
  val corrupt = Bool()
}

class TLBundleE(override val params: TLParameters) extends TLChannel
{
  val channelName = "'E' channel"
  val sink    = UInt(params.sinkBits.W)
}

// TL-UL and TL-UC
class TLUnCached(val params: TLParameters) extends Bundle {
  val a = Decoupled(new TLBundleA(params))
  val d = Flipped(Decoupled(new TLBundleD(params)))
}

// TL-C
class TLCached(override val params: TLParameters) extends TLUnCached(params) {
  val b = Flipped(Decoupled(new TLBundleB(params)))
  val c = Decoupled(new TLBundleC(params))
  val e = Decoupled(new TLBundleE(params))
}

object TLUnCached
{
  def apply(params: TLParameters) = new TLUnCached(params)
}

object TLCached
{
  def apply(params: TLParameters) = new TLCached(params)
}
