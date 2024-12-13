/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package utils

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink.{TLBundle, TLBundleA, TLBundleB, TLBundleC, TLBundleD, TLBundleE, TLChannel}
import utility.XSDebug

trait HasTLDump {

  implicit val p: Parameters

  implicit class TLDump(channel: TLChannel) {
    def dump(cond: Bool) = channel match {
      case a: TLBundleA =>
        printChannelA(a, cond)
      case b: TLBundleB =>
        printChannelB(b, cond)
      case c: TLBundleC =>
        printChannelC(c, cond)
      case d: TLBundleD =>
        printChannelD(d, cond)
      case e: TLBundleE =>
        printChannelE(e, cond)
    }
  }

  def printChannelA(a: TLBundleA, cond: Bool): Unit = {
    def APrintable(opStr: String, paramStr: String = ""): Printable = {
      a.channelName + " " + opStr + " " +
        (if (paramStr != "") paramStr else Printable.pack("param: %x", a.param)) +
        Printable.pack(" size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          a.size, a.source, a.address, a.mask, a.data, a.corrupt)
    }
    def ACond(opCode: UInt, param: Option[UInt] = None): Bool = {
      // skip param compare if not passed
      val paramComp = if (param.isDefined) a.param === param.get else true.B
      cond && a.opcode === opCode && paramComp
    }

    XSDebug(false, ACond(PutFullData), APrintable("PutFullData"))
    XSDebug(false, ACond(PutPartialData), APrintable("PutPartialData"))
    XSDebug(false, ACond(ArithmeticData), APrintable("ArithmeticData"))
    XSDebug(false, ACond(LogicalData), APrintable("LogicalData"))
    XSDebug(false, ACond(Get), APrintable("Get"))
    XSDebug(false, ACond(Hint), APrintable("Intent"))

    XSDebug(false, ACond(AcquireBlock, Some(NtoB)), APrintable("AcquireBlock", "NtoB"))
    XSDebug(false, ACond(AcquireBlock, Some(NtoT)), APrintable("AcquireBlock", "NtoT"))
    XSDebug(false, ACond(AcquireBlock, Some(BtoT)), APrintable("AcquireBlock", "BtoT"))

    XSDebug(false, ACond(AcquirePerm, Some(NtoB)), APrintable("AcquirePerm", "NtoB"))
    XSDebug(false, ACond(AcquirePerm, Some(NtoT)), APrintable("AcquirePerm", "NtoT"))
    XSDebug(false, ACond(AcquirePerm, Some(BtoT)), APrintable("AcquirePerm", "BtoT"))
  }

  def printChannelB(b: TLBundleB, cond: Bool): Unit = {
    def BPrintable(opStr: String, paramStr: String = ""): Printable = {
      b.channelName + " " + opStr + " " +
        (if (paramStr != "") paramStr else Printable.pack("param: %x", b.param)) +
        Printable.pack(" size: %x source: %d address: %x mask: %x data: %x corrupt: %b\n",
          b.size, b.source, b.address, b.mask, b.data, b.corrupt)
    }
    def BCond(opCode: UInt, param: Option[UInt] = None): Bool = {
      // skip param compare if not passed
      val paramComp = if (param.isDefined) b.param === param.get else true.B
      cond && b.opcode === opCode && paramComp
    }

    XSDebug(false, BCond(PutFullData), BPrintable("PutFullData"))
    XSDebug(false, BCond(PutPartialData), BPrintable("PutPartialData"))
    XSDebug(false, BCond(ArithmeticData), BPrintable("ArithmeticData"))
    XSDebug(false, BCond(LogicalData), BPrintable("LogicalData"))
    XSDebug(false, BCond(Get), BPrintable("Get"))
    XSDebug(false, BCond(Hint), BPrintable("Intent"))

    XSDebug(false, BCond(Probe, Some(toN)), BPrintable("Probe", "toN"))
    XSDebug(false, BCond(Probe, Some(toB)), BPrintable("Probe", "toB"))
    XSDebug(false, BCond(Probe, Some(toT)), BPrintable("Probe", "toT"))
  }

  def printChannelC(c: TLBundleC, cond: Bool): Unit = {
    def CPrintable(opStr: String, paramStr: String = ""): Printable = {
      c.channelName + " " + opStr + " " +
        (if (paramStr != "") paramStr else Printable.pack("param: %x", c.param)) +
        Printable.pack(" size: %x source: %d address: %x data: %x corrupt: %b\n",
          c.size, c.source, c.address, c.data, c.corrupt)
    }
    def CCond(opCode: UInt, param: Option[UInt] = None): Bool = {
      // skip param compare if not passed
      val paramComp = if (param.isDefined) c.param === param.get else true.B
      cond && c.opcode === opCode && paramComp
    }

    XSDebug(false, CCond(AccessAck), CPrintable("AccessAck"))
    XSDebug(false, CCond(AccessAckData), CPrintable("AccessAckData"))
    XSDebug(false, CCond(HintAck), CPrintable("HintAck"))

    XSDebug(false, CCond(ProbeAck, Some(TtoB)), CPrintable("ProbeAck", "TtoB"))
    XSDebug(false, CCond(ProbeAck, Some(TtoN)), CPrintable("ProbeAck", "TtoN"))
    XSDebug(false, CCond(ProbeAck, Some(BtoN)), CPrintable("ProbeAck", "BtoN"))
    XSDebug(false, CCond(ProbeAck, Some(TtoT)), CPrintable("ProbeAck", "TtoT"))
    XSDebug(false, CCond(ProbeAck, Some(BtoB)), CPrintable("ProbeAck", "BtoB"))
    XSDebug(false, CCond(ProbeAck, Some(NtoN)), CPrintable("ProbeAck", "NtoN"))

    XSDebug(false, CCond(ProbeAckData, Some(TtoB)), CPrintable("ProbeAckData", "TtoB"))
    XSDebug(false, CCond(ProbeAckData, Some(TtoN)), CPrintable("ProbeAckData", "TtoN"))
    XSDebug(false, CCond(ProbeAckData, Some(BtoN)), CPrintable("ProbeAckData", "BtoN"))
    XSDebug(false, CCond(ProbeAckData, Some(TtoT)), CPrintable("ProbeAckData", "TtoT"))
    XSDebug(false, CCond(ProbeAckData, Some(BtoB)), CPrintable("ProbeAckData", "BtoB"))
    XSDebug(false, CCond(ProbeAckData, Some(NtoN)), CPrintable("ProbeAckData", "NtoN"))

    XSDebug(false, CCond(Release, Some(TtoB)), CPrintable("Release", "TtoB"))
    XSDebug(false, CCond(Release, Some(TtoN)), CPrintable("Release", "TtoN"))
    XSDebug(false, CCond(Release, Some(BtoN)), CPrintable("Release", "BtoN"))
    XSDebug(false, CCond(Release, Some(TtoT)), CPrintable("Release", "TtoT"))
    XSDebug(false, CCond(Release, Some(BtoB)), CPrintable("Release", "BtoB"))
    XSDebug(false, CCond(Release, Some(NtoN)), CPrintable("Release", "NtoN"))

    XSDebug(false, CCond(ReleaseData, Some(TtoB)), CPrintable("ReleaseData", "TtoB"))
    XSDebug(false, CCond(ReleaseData, Some(TtoN)), CPrintable("ReleaseData", "TtoN"))
    XSDebug(false, CCond(ReleaseData, Some(BtoN)), CPrintable("ReleaseData", "BtoN"))
    XSDebug(false, CCond(ReleaseData, Some(TtoT)), CPrintable("ReleaseData", "TtoT"))
    XSDebug(false, CCond(ReleaseData, Some(BtoB)), CPrintable("ReleaseData", "BtoB"))
    XSDebug(false, CCond(ReleaseData, Some(NtoN)), CPrintable("ReleaseData", "NtoN"))
  }

  def printChannelD(d: TLBundleD, cond: Bool): Unit = {
    def DPrintable(opStr: String, paramStr: String = ""): Printable = {
      d.channelName + " " + opStr + " " +
        (if (paramStr != "") paramStr else Printable.pack("param: %x", d.param)) +
        Printable.pack(" size: %x source: %d sink: %d denied: %b data: %x corrupt: %b\n",
          d.size, d.source, d.sink, d.denied, d.data, d.corrupt)
    }
    def DCond(opCode: UInt, param: Option[UInt] = None): Bool = {
      // skip param compare if not passed
      val paramComp = if (param.isDefined) d.param === param.get else true.B
      cond && d.opcode === opCode && paramComp
    }

    XSDebug(false, DCond(AccessAck), DPrintable("AccessAck"))
    XSDebug(false, DCond(AccessAckData), DPrintable("AccessAckData"))
    XSDebug(false, DCond(HintAck), DPrintable("HintAck"))

    XSDebug(false, DCond(Grant, Some(toT)), DPrintable("Grant", "toT"))
    XSDebug(false, DCond(Grant, Some(toB)), DPrintable("Grant", "toB"))
    XSDebug(false, DCond(Grant, Some(toN)), DPrintable("Grant", "toN"))

    XSDebug(false, DCond(GrantData, Some(toT)), DPrintable("GrantData", "toT"))
    XSDebug(false, DCond(GrantData, Some(toB)), DPrintable("GrantData", "toB"))
    XSDebug(false, DCond(GrantData, Some(toN)), DPrintable("GrantData", "toN"))

    XSDebug(false, DCond(GrantData, Some(toT)), DPrintable("GrantData", "toT"))
    XSDebug(false, DCond(GrantData, Some(toB)), DPrintable("GrantData", "toB"))
    XSDebug(false, DCond(GrantData, Some(toN)), DPrintable("GrantData", "toN"))

    XSDebug(false, DCond(ReleaseAck), DPrintable("ReleaseAck"))
  }

  def printChannelE(e: TLBundleE, cond: Bool): Unit = {
    XSDebug(false, cond, e.channelName + "GrantAck sink: %d\n", e.sink)
  }

}
