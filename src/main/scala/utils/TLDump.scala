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
import utility.{XSDebug => XSSim}

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

    XSSim(false, ACond(PutFullData), APrintable("PutFullData"))
    XSSim(false, ACond(PutPartialData), APrintable("PutPartialData"))
    XSSim(false, ACond(ArithmeticData), APrintable("ArithmeticData"))
    XSSim(false, ACond(LogicalData), APrintable("LogicalData"))
    XSSim(false, ACond(Get), APrintable("Get"))
    XSSim(false, ACond(Hint), APrintable("Intent"))

    XSSim(false, ACond(AcquireBlock, Some(NtoB)), APrintable("AcquireBlock", "NtoB"))
    XSSim(false, ACond(AcquireBlock, Some(NtoT)), APrintable("AcquireBlock", "NtoT"))
    XSSim(false, ACond(AcquireBlock, Some(BtoT)), APrintable("AcquireBlock", "BtoT"))

    XSSim(false, ACond(AcquirePerm, Some(NtoB)), APrintable("AcquirePerm", "NtoB"))
    XSSim(false, ACond(AcquirePerm, Some(NtoT)), APrintable("AcquirePerm", "NtoT"))
    XSSim(false, ACond(AcquirePerm, Some(BtoT)), APrintable("AcquirePerm", "BtoT"))
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

    XSSim(false, BCond(PutFullData), BPrintable("PutFullData"))
    XSSim(false, BCond(PutPartialData), BPrintable("PutPartialData"))
    XSSim(false, BCond(ArithmeticData), BPrintable("ArithmeticData"))
    XSSim(false, BCond(LogicalData), BPrintable("LogicalData"))
    XSSim(false, BCond(Get), BPrintable("Get"))
    XSSim(false, BCond(Hint), BPrintable("Intent"))

    XSSim(false, BCond(Probe, Some(toN)), BPrintable("Probe", "toN"))
    XSSim(false, BCond(Probe, Some(toB)), BPrintable("Probe", "toB"))
    XSSim(false, BCond(Probe, Some(toT)), BPrintable("Probe", "toT"))
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

    XSSim(false, CCond(AccessAck), CPrintable("AccessAck"))
    XSSim(false, CCond(AccessAckData), CPrintable("AccessAckData"))
    XSSim(false, CCond(HintAck), CPrintable("HintAck"))

    XSSim(false, CCond(ProbeAck, Some(TtoB)), CPrintable("ProbeAck", "TtoB"))
    XSSim(false, CCond(ProbeAck, Some(TtoN)), CPrintable("ProbeAck", "TtoN"))
    XSSim(false, CCond(ProbeAck, Some(BtoN)), CPrintable("ProbeAck", "BtoN"))
    XSSim(false, CCond(ProbeAck, Some(TtoT)), CPrintable("ProbeAck", "TtoT"))
    XSSim(false, CCond(ProbeAck, Some(BtoB)), CPrintable("ProbeAck", "BtoB"))
    XSSim(false, CCond(ProbeAck, Some(NtoN)), CPrintable("ProbeAck", "NtoN"))

    XSSim(false, CCond(ProbeAckData, Some(TtoB)), CPrintable("ProbeAckData", "TtoB"))
    XSSim(false, CCond(ProbeAckData, Some(TtoN)), CPrintable("ProbeAckData", "TtoN"))
    XSSim(false, CCond(ProbeAckData, Some(BtoN)), CPrintable("ProbeAckData", "BtoN"))
    XSSim(false, CCond(ProbeAckData, Some(TtoT)), CPrintable("ProbeAckData", "TtoT"))
    XSSim(false, CCond(ProbeAckData, Some(BtoB)), CPrintable("ProbeAckData", "BtoB"))
    XSSim(false, CCond(ProbeAckData, Some(NtoN)), CPrintable("ProbeAckData", "NtoN"))

    XSSim(false, CCond(Release, Some(TtoB)), CPrintable("Release", "TtoB"))
    XSSim(false, CCond(Release, Some(TtoN)), CPrintable("Release", "TtoN"))
    XSSim(false, CCond(Release, Some(BtoN)), CPrintable("Release", "BtoN"))
    XSSim(false, CCond(Release, Some(TtoT)), CPrintable("Release", "TtoT"))
    XSSim(false, CCond(Release, Some(BtoB)), CPrintable("Release", "BtoB"))
    XSSim(false, CCond(Release, Some(NtoN)), CPrintable("Release", "NtoN"))

    XSSim(false, CCond(ReleaseData, Some(TtoB)), CPrintable("ReleaseData", "TtoB"))
    XSSim(false, CCond(ReleaseData, Some(TtoN)), CPrintable("ReleaseData", "TtoN"))
    XSSim(false, CCond(ReleaseData, Some(BtoN)), CPrintable("ReleaseData", "BtoN"))
    XSSim(false, CCond(ReleaseData, Some(TtoT)), CPrintable("ReleaseData", "TtoT"))
    XSSim(false, CCond(ReleaseData, Some(BtoB)), CPrintable("ReleaseData", "BtoB"))
    XSSim(false, CCond(ReleaseData, Some(NtoN)), CPrintable("ReleaseData", "NtoN"))
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

    XSSim(false, DCond(AccessAck), DPrintable("AccessAck"))
    XSSim(false, DCond(AccessAckData), DPrintable("AccessAckData"))
    XSSim(false, DCond(HintAck), DPrintable("HintAck"))

    XSSim(false, DCond(Grant, Some(toT)), DPrintable("Grant", "toT"))
    XSSim(false, DCond(Grant, Some(toB)), DPrintable("Grant", "toB"))
    XSSim(false, DCond(Grant, Some(toN)), DPrintable("Grant", "toN"))

    XSSim(false, DCond(GrantData, Some(toT)), DPrintable("GrantData", "toT"))
    XSSim(false, DCond(GrantData, Some(toB)), DPrintable("GrantData", "toB"))
    XSSim(false, DCond(GrantData, Some(toN)), DPrintable("GrantData", "toN"))

    XSSim(false, DCond(GrantData, Some(toT)), DPrintable("GrantData", "toT"))
    XSSim(false, DCond(GrantData, Some(toB)), DPrintable("GrantData", "toB"))
    XSSim(false, DCond(GrantData, Some(toN)), DPrintable("GrantData", "toN"))

    XSSim(false, DCond(ReleaseAck), DPrintable("ReleaseAck"))
  }

  def printChannelE(e: TLBundleE, cond: Bool): Unit = {
    XSSim(false, cond, e.channelName + "GrantAck sink: %d\n", e.sink)
  }

}
