/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

// See LICENSE.SiFive for license details.

package bus.tilelink

import chisel3._
import chisel3.util._
import utils.MaskGen
import utils.OneHot

object TLUtilities {
  // lgSize is the log2 of transfer size
  // you can directly put Tilelink channel's size here, since it's already log2.
  def isAligned(params: TLParameters, address: UInt, lgSize: UInt): Bool = {
    if (params.maxLgSize == 0) true.B else {
      val mask = OneHot.UIntToOH1(lgSize, params.maxLgSize)
      (address & mask) === 0.U
    }
  }

  def mask(params: TLParameters, address: UInt, lgSize: UInt): UInt =
    MaskGen(address, lgSize, params.beatBytes)

  def isRequest(x: TLChannel): Bool = {
    x match {
      case a: TLBundleA => true.B
      case b: TLBundleB => true.B
      case c: TLBundleC => c.opcode(2) && c.opcode(1)
        //    opcode === TLMessages.Release ||
        //    opcode === TLMessages.ReleaseData
      case d: TLBundleD => d.opcode(2) && !d.opcode(1)
        //    opcode === TLMessages.Grant     ||
        //    opcode === TLMessages.GrantData
      case e: TLBundleE => false.B
    }
  }

  def isResponse(x: TLChannel): Bool = {
    x match {
      case a: TLBundleA => false.B
      case b: TLBundleB => false.B
      case c: TLBundleC => !c.opcode(2) || !c.opcode(1)
        //    opcode =/= TLMessages.Release &&
        //    opcode =/= TLMessages.ReleaseData
      case d: TLBundleD => true.B // Grant isResponse + isRequest
      case e: TLBundleE => true.B
    }
  }

  def hasData(x: TLChannel): Bool = {
    val opdata = x match {
      case a: TLBundleA => !a.opcode(2)
        //    opcode === TLMessages.PutFullData    ||
        //    opcode === TLMessages.PutPartialData ||
        //    opcode === TLMessages.ArithmeticData ||
        //    opcode === TLMessages.LogicalData
      case b: TLBundleB => !b.opcode(2)
        //    opcode === TLMessages.PutFullData    ||
        //    opcode === TLMessages.PutPartialData ||
        //    opcode === TLMessages.ArithmeticData ||
        //    opcode === TLMessages.LogicalData
      case c: TLBundleC => c.opcode(0)
        //    opcode === TLMessages.AccessAckData ||
        //    opcode === TLMessages.ProbeAckData  ||
        //    opcode === TLMessages.ReleaseData
      case d: TLBundleD => d.opcode(0)
        //    opcode === TLMessages.AccessAckData ||
        //    opcode === TLMessages.GrantData
      case e: TLBundleE => false.B
    }
    opdata
  }

  def opcode(x: TLDataChannel): UInt = {
    x match {
      case a: TLBundleA => a.opcode
      case b: TLBundleB => b.opcode
      case c: TLBundleC => c.opcode
      case d: TLBundleD => d.opcode
    }
  }

  def param(x: TLDataChannel): UInt = {
    x match {
      case a: TLBundleA => a.param
      case b: TLBundleB => b.param
      case c: TLBundleC => c.param
      case d: TLBundleD => d.param
    }
  }

  def size(x: TLDataChannel): UInt = {
    x match {
      case a: TLBundleA => a.size
      case b: TLBundleB => b.size
      case c: TLBundleC => c.size
      case d: TLBundleD => d.size
    }
  }

  def data(x: TLDataChannel): UInt = {
    x match {
      case a: TLBundleA => a.data
      case b: TLBundleB => b.data
      case c: TLBundleC => c.data
      case d: TLBundleD => d.data
    }
  }

  def corrupt(x: TLDataChannel): Bool = {
    x match {
      case a: TLBundleA => a.corrupt
      case b: TLBundleB => b.corrupt
      case c: TLBundleC => c.corrupt
      case d: TLBundleD => d.corrupt
    }
  }

  def mask(x: TLAddrChannel): UInt = {
    x match {
      case a: TLBundleA => a.mask
      case b: TLBundleB => b.mask
      case c: TLBundleC => mask(c.params, c.address, c.size)
    }
  }

  def full_mask(x: TLAddrChannel): UInt = {
    x match {
      case a: TLBundleA => mask(a.params, a.address, a.size)
      case b: TLBundleB => mask(b.params, b.address, b.size)
      case c: TLBundleC => mask(c.params, c.address, c.size)
    }
  }

  def address(x: TLAddrChannel): UInt = {
    x match {
      case a: TLBundleA => a.address
      case b: TLBundleB => b.address
      case c: TLBundleC => c.address
    }
  }

  def source(x: TLDataChannel): UInt = {
    x match {
      case a: TLBundleA => a.source
      case b: TLBundleB => b.source
      case c: TLBundleC => c.source
      case d: TLBundleD => d.source
    }
  }

  def addr_hi(params: TLParameters, x: UInt): UInt = x >> log2Ceil(params.beatBytes)
  def addr_lo(params: TLParameters, x: UInt): UInt =
    if (params.beatBytes == 1) 0.U else x(log2Ceil(params.beatBytes)-1, 0)

  def addr_hi(x: TLAddrChannel): UInt = addr_hi(x.params, address(x))
  def addr_lo(x: TLAddrChannel): UInt = addr_lo(x.params, address(x))

  // if there are n beats, numBeats gives n
  def numBeats(x: TLChannel): UInt = {
    val params = x.params
    x match {
      case _: TLBundleE => 1.U
      case bundle: TLDataChannel => {
        val hasData = this.hasData(bundle)
        val size = this.size(bundle)
        val cutoff = log2Ceil(params.beatBytes)
        val small = if (params.maxTransfer <= params.beatBytes) true.B else size <= cutoff.U
        val decode = UIntToOH(size, params.maxLgSize+1) >> cutoff
        Mux(hasData, decode | small.asUInt, 1.U)
      }
    }
  }

  // if there are n beats, numBeats1 gives (n - 1)
  // so if you want to gather beats from 0 to n - 1
  // you better use this
  def numBeats1(x: TLChannel): UInt = {
    val params = x.params
    x match {
      case _: TLBundleE => 0.U
      case bundle: TLDataChannel => {
        if (params.maxLgSize == 0) {
          0.U
        } else {
          val decode = OneHot.UIntToOH1(size(bundle), params.maxLgSize) >> log2Ceil(params.beatBytes)
          Mux(hasData(bundle), decode, 0.U)
        }
      }
    }
  }

  // it may seems unnecessarily complex, but it's correct
  // count ticks from 0 to numBeats1
  def firstlastHelper(bits: TLChannel, fire: Bool): (Bool, Bool, Bool, UInt) = {
    val params = bits.params
    val beats1   = numBeats1(bits)
    val counter  = RegInit(0.U(log2Up(params.maxTransfer / params.beatBytes).W))
    val counter1 = counter - 1.U
    val first = counter === 0.U
    val last  = counter === 1.U || beats1 === 0.U
    val done  = last && fire
    val count = (beats1 & ~counter1)
    when (fire) {
      counter := Mux(first, beats1, counter1)
    }
    (first, last, done, count)
  }

  def first(bits: TLChannel, fire: Bool): Bool = firstlastHelper(bits, fire)._1
  def first(x: DecoupledIO[TLChannel]): Bool = first(x.bits, x.fire())
  def first(x: ValidIO[TLChannel]): Bool = first(x.bits, x.valid)

  def last(bits: TLChannel, fire: Bool): Bool = firstlastHelper(bits, fire)._2
  def last(x: DecoupledIO[TLChannel]): Bool = last(x.bits, x.fire())
  def last(x: ValidIO[TLChannel]): Bool = last(x.bits, x.valid)

  def done(bits: TLChannel, fire: Bool): Bool = firstlastHelper(bits, fire)._3
  def done(x: DecoupledIO[TLChannel]): Bool = done(x.bits, x.fire())
  def done(x: ValidIO[TLChannel]): Bool = done(x.bits, x.valid)

  def firstlast(bits: TLChannel, fire: Bool): (Bool, Bool, Bool) = {
    val r = firstlastHelper(bits, fire)
    (r._1, r._2, r._3)
  }
  def firstlast(x: DecoupledIO[TLChannel]): (Bool, Bool, Bool) = firstlast(x.bits, x.fire())
  def firstlast(x: ValidIO[TLChannel]): (Bool, Bool, Bool) = firstlast(x.bits, x.valid)

  def count(bits: TLChannel, fire: Bool): (Bool, Bool, Bool, UInt) = {
    val r = firstlastHelper(bits, fire)
    (r._1, r._2, r._3, r._4)
  }
  def count(x: DecoupledIO[TLChannel]): (Bool, Bool, Bool, UInt) = count(x.bits, x.fire())
  def count(x: ValidIO[TLChannel]): (Bool, Bool, Bool, UInt) = count(x.bits, x.valid)

  def addr_inc(bits: TLChannel, fire: Bool): (Bool, Bool, Bool, UInt) = {
    val params = bits.params
    val r = firstlastHelper(bits, fire)
    (r._1, r._2, r._3, r._4 << log2Ceil(params.beatBytes))
  }
  def addr_inc(x: DecoupledIO[TLChannel]): (Bool, Bool, Bool, UInt) = addr_inc(x.bits, x.fire())
  def addr_inc(x: ValidIO[TLChannel]): (Bool, Bool, Bool, UInt) = addr_inc(x.bits, x.valid)

  /*
  // This is a very expensive circuit; use only if you really mean it!
  def inFlight(x: TLBundle): (UInt, UInt) = {
    val flight = RegInit(0.U(log2Ceil(3*client.endSourceId+1).W))
    val bce = manager.anySupportAcquireB && client.anySupportProbe

    val (a_first, a_last, _) = firstlast(x.a)
    val (b_first, b_last, _) = firstlast(x.b)
    val (c_first, c_last, _) = firstlast(x.c)
    val (d_first, d_last, _) = firstlast(x.d)
    val (e_first, e_last, _) = firstlast(x.e)

    val (a_request, a_response) = (isRequest(x.a.bits), isResponse(x.a.bits))
    val (b_request, b_response) = (isRequest(x.b.bits), isResponse(x.b.bits))
    val (c_request, c_response) = (isRequest(x.c.bits), isResponse(x.c.bits))
    val (d_request, d_response) = (isRequest(x.d.bits), isResponse(x.d.bits))
    val (e_request, e_response) = (isRequest(x.e.bits), isResponse(x.e.bits))

    val a_inc = x.a.fire() && a_first && a_request
    val b_inc = x.b.fire() && b_first && b_request
    val c_inc = x.c.fire() && c_first && c_request
    val d_inc = x.d.fire() && d_first && d_request
    val e_inc = x.e.fire() && e_first && e_request
    val inc = Cat(Seq(a_inc, d_inc) ++ (if (bce) Seq(b_inc, c_inc, e_inc) else Nil))

    val a_dec = x.a.fire() && a_last && a_response
    val b_dec = x.b.fire() && b_last && b_response
    val c_dec = x.c.fire() && c_last && c_response
    val d_dec = x.d.fire() && d_last && d_response
    val e_dec = x.e.fire() && e_last && e_response
    val dec = Cat(Seq(a_dec, d_dec) ++ (if (bce) Seq(b_dec, c_dec, e_dec) else Nil))

    val next_flight = flight + PopCount(inc) - PopCount(dec)
    flight := next_flight

    (flight, next_flight)
  }

  def prettySourceMapping(context: String): String = {
    s"TL-Source mapping for $context:\n${(new TLSourceIdMap(client)).pretty}\n"
  }
  */
}

object TLMasterUtilities {
  // Transfers
  def AcquireBlock(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, growPermissions: UInt) = {
    // require (manager.anySupportAcquireB, s"TileLink: No managers visible from this edge support Acquires, but one of these clients would try to request one: ${client.clients}")
    // val legal = manager.supportsAcquireBFast(toAddress, lgSize)
    // 我们现在的tilelink的bundle里面只带了信号，没有像diplomacy那样，用上tilelink的backing memory的属性
    val legal = true.B
    val a = Wire(new TLBundleA(params))
    a.opcode  := TLMessages.AcquireBlock
    a.param   := growPermissions
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.mask    := TLUtilities.mask(params, toAddress, lgSize)
    a.data    := 0.U
    a.corrupt := false.B
    (legal, a)
  }

  def AcquirePerm(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, growPermissions: UInt) = {
    // require (manager.anySupportAcquireB, s"TileLink: No managers visible from this edge support Acquires, but one of these clients would try to request one: ${client.clients}")
    // val legal = manager.supportsAcquireBFast(toAddress, lgSize)
    val legal = true.B
    val a = Wire(new TLBundleA(params))
    a.opcode  := TLMessages.AcquirePerm
    a.param   := growPermissions
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.mask    := TLUtilities.mask(params, toAddress, lgSize)
    a.data    := 0.U
    a.corrupt := false.B
    (legal, a)
  }

  def Release(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, shrinkPermissions: UInt): (Bool, TLBundleC) = {
    // require (manager.anySupportAcquireB, s"TileLink: No managers visible from this edge support Acquires, but one of these clients would try to request one: ${client.clients}")
    // val legal = manager.supportsAcquireBFast(toAddress, lgSize)
    val legal = true.B
    val c = Wire(new TLBundleC(params))
    c.opcode  := TLMessages.Release
    c.param   := shrinkPermissions
    c.size    := lgSize
    c.source  := fromSource
    c.address := toAddress
    c.data    := 0.U
    c.corrupt := false.B
    (legal, c)
  }

  def Release(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, shrinkPermissions: UInt, data: UInt, corrupt: Bool): (Bool, TLBundleC) = {
    // require (manager.anySupportAcquireB, s"TileLink: No managers visible from this edge support Acquires, but one of these clients would try to request one: ${client.clients}")
    // val legal = manager.supportsAcquireBFast(toAddress, lgSize)
    val legal = true.B
    val c = Wire(new TLBundleC(params))
    c.opcode  := TLMessages.ReleaseData
    c.param   := shrinkPermissions
    c.size    := lgSize
    c.source  := fromSource
    c.address := toAddress
    c.data    := data
    c.corrupt := corrupt
    (legal, c)
  }

  def Release(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, shrinkPermissions: UInt, data: UInt): (Bool, TLBundleC) =
    Release(params, fromSource, toAddress, lgSize, shrinkPermissions, data, false.B)

  def ProbeAck(b: TLBundleB, reportPermissions: UInt): TLBundleC =
    ProbeAck(b.params, b.source, b.address, b.size, reportPermissions)

  def ProbeAck(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, reportPermissions: UInt): TLBundleC = {
    val c = Wire(new TLBundleC(params))
    c.opcode  := TLMessages.ProbeAck
    c.param   := reportPermissions
    c.size    := lgSize
    c.source  := fromSource
    c.address := toAddress
    c.data    := 0.U
    c.corrupt := false.B
    c
  }

  def ProbeAck(b: TLBundleB, reportPermissions: UInt, data: UInt): TLBundleC =
    ProbeAck(b.params, b.source, b.address, b.size, reportPermissions, data)

  def ProbeAck(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, reportPermissions: UInt, data: UInt, corrupt: Bool): TLBundleC = {
    val c = Wire(new TLBundleC(params))
    c.opcode  := TLMessages.ProbeAckData
    c.param   := reportPermissions
    c.size    := lgSize
    c.source  := fromSource
    c.address := toAddress
    c.data    := data
    c.corrupt := corrupt
    c
  }

  def ProbeAck(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, reportPermissions: UInt, data: UInt): TLBundleC =
    ProbeAck(params, fromSource, toAddress, lgSize, reportPermissions, data, false.B)

  def GrantAck(d: TLBundleD): TLBundleE = GrantAck(d. params, d.sink)
  def GrantAck(params: TLParameters, toSink: UInt): TLBundleE = {
    val e = Wire(new TLBundleE(params))
    e.sink := toSink
    e
  }

  // Accesses
  def Get(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt) = {
    // require (manager.anySupportGet, s"TileLink: No managers visible from this edge support Gets, but one of these clients would try to request one: ${client.clients}")
    // val legal = manager.supportsGetFast(toAddress, lgSize)
    val legal = true.B
    val a = Wire(new TLBundleA(params))
    a.opcode  := TLMessages.Get
    a.param   := 0.U
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.mask    := TLUtilities.mask(params, toAddress, lgSize)
    a.data    := 0.U
    a.corrupt := false.B
    (legal, a)
  }

  def Put(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt): (Bool, TLBundleA) =
    Put(params, fromSource, toAddress, lgSize, data, false.B)

  def Put(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt, corrupt: Bool): (Bool, TLBundleA) = {
    // require (manager.anySupportPutFull, s"TileLink: No managers visible from this edge support Puts, but one of these clients would try to request one: ${client.clients}")
    // val legal = manager.supportsPutFullFast(toAddress, lgSize)
    val legal = true.B
    val a = Wire(new TLBundleA(params))
    a.opcode  := TLMessages.PutFullData
    a.param   := 0.U
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.mask    := TLUtilities.mask(params, toAddress, lgSize)
    a.data    := data
    a.corrupt := corrupt
    (legal, a)
  }

  def Put(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt, mask: UInt): (Bool, TLBundleA) =
    Put(params, fromSource, toAddress, lgSize, data, mask, false.B)

  def Put(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt, mask: UInt, corrupt: Bool): (Bool, TLBundleA) = {
    // require (manager.anySupportPutPartial, s"TileLink: No managers visible from this edge support masked Puts, but one of these clients would try to request one: ${client.clients}")
    // val legal = manager.supportsPutPartialFast(toAddress, lgSize)
    val legal = true.B
    val a = Wire(new TLBundleA(params))
    a.opcode  := TLMessages.PutPartialData
    a.param   := 0.U
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.mask    := mask
    a.data    := data
    a.corrupt := corrupt
    (legal, a)
  }

  def Arithmetic(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt, atomic: UInt, corrupt: Bool = false.B): (Bool, TLBundleA) = {
    // require (manager.anySupportArithmetic, s"TileLink: No managers visible from this edge support arithmetic AMOs, but one of these clients would try to request one: ${client.clients}")
    // val legal = manager.supportsArithmeticFast(toAddress, lgSize)
    val legal = true.B
    val a = Wire(new TLBundleA(params))
    a.opcode  := TLMessages.ArithmeticData
    a.param   := atomic
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.mask    := TLUtilities.mask(params, toAddress, lgSize)
    a.data    := data
    a.corrupt := corrupt
    (legal, a)
  }

  def Logical(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt, atomic: UInt, corrupt: Bool = false.B) = {
    // require (manager.anySupportLogical, s"TileLink: No managers visible from this edge support logical AMOs, but one of these clients would try to request one: ${client.clients}")
    // val legal = manager.supportsLogicalFast(toAddress, lgSize)
    val legal = true.B
    val a = Wire(new TLBundleA(params))
    a.opcode  := TLMessages.LogicalData
    a.param   := atomic
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.mask    := TLUtilities.mask(params, toAddress, lgSize)
    a.data    := data
    a.corrupt := corrupt
    (legal, a)
  }

  def Hint(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, param: UInt) = {
    // require (manager.anySupportHint, s"TileLink: No managers visible from this edge support Hints, but one of these clients would try to request one: ${client.clients}")
    // val legal = manager.supportsHintFast(toAddress, lgSize)
    val legal = true.B
    val a = Wire(new TLBundleA(params))
    a.opcode  := TLMessages.Hint
    a.param   := param
    a.size    := lgSize
    a.source  := fromSource
    a.address := toAddress
    a.mask    := TLUtilities.mask(params, toAddress, lgSize)
    a.data    := 0.U
    a.corrupt := false.B
    (legal, a)
  }

  def AccessAck(b: TLBundleB): TLBundleC = AccessAck(b.params, b.source, TLUtilities.address(b), b.size)
  def AccessAck(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt) = {
    val c = Wire(new TLBundleC(params))
    c.opcode  := TLMessages.AccessAck
    c.param   := 0.U
    c.size    := lgSize
    c.source  := fromSource
    c.address := toAddress
    c.data    := 0.U
    c.corrupt := false.B
    c
  }

  def AccessAck(b: TLBundleB, data: UInt): TLBundleC = AccessAck(b.params, b.source, TLUtilities.address(b), b.size, data)
  def AccessAck(b: TLBundleB, data: UInt, corrupt: Bool): TLBundleC = AccessAck(b.params, b.source, TLUtilities.address(b), b.size, data, corrupt)
  def AccessAck(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt): TLBundleC = AccessAck(params, fromSource, toAddress, lgSize, data, false.B)
  def AccessAck(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt, data: UInt, corrupt: Bool) = {
    val c = Wire(new TLBundleC(params))
    c.opcode  := TLMessages.AccessAckData
    c.param   := 0.U
    c.size    := lgSize
    c.source  := fromSource
    c.address := toAddress
    c.data    := data
    c.corrupt := corrupt
    c
  }

  def HintAck(b: TLBundleB): TLBundleC = HintAck(b.params, b.source, TLUtilities.address(b), b.size)
  def HintAck(params: TLParameters, fromSource: UInt, toAddress: UInt, lgSize: UInt) = {
    val c = Wire(new TLBundleC(params))
    c.opcode  := TLMessages.HintAck
    c.param   := 0.U
    c.size    := lgSize
    c.source  := fromSource
    c.address := toAddress
    c.data    := 0.U
    c.corrupt := false.B
    c
  }
}

object TLSlaveUtilities {
  private def myTranspose[T](x: Seq[Seq[T]]): Seq[Seq[T]] = {
    val todo = x.filter(!_.isEmpty)
    val heads = todo.map(_.head)
    val tails = todo.map(_.tail)
    if (todo.isEmpty) Nil else { heads +: myTranspose(tails) }
  }

  // Transfers
  def Probe(params: TLParameters, fromAddress: UInt, toSource: UInt, lgSize: UInt, capPermissions: UInt) = {
    // require (client.anySupportProbe, s"TileLink: No clients visible from this edge support probes, but one of these managers tried to issue one: ${manager.managers}")
    // val legal = client.supportsProbe(toSource, lgSize)
    val legal = true.B
    val b = Wire(new TLBundleB(params))
    b.opcode  := TLMessages.Probe
    b.param   := capPermissions
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.mask    := TLUtilities.mask(params, fromAddress, lgSize)
    b.data    := 0.U
    b.corrupt := false.B
    (legal, b)
  }

  def Grant(params: TLParameters, fromSink: UInt, toSource: UInt, lgSize: UInt, capPermissions: UInt): TLBundleD = Grant(params, fromSink, toSource, lgSize, capPermissions, false.B)
  def Grant(params: TLParameters, fromSink: UInt, toSource: UInt, lgSize: UInt, capPermissions: UInt, denied: Bool) = {
    val d = Wire(new TLBundleD(params))
    d.opcode  := TLMessages.Grant
    d.param   := capPermissions
    d.size    := lgSize
    d.source  := toSource
    d.sink    := fromSink
    d.denied  := denied
    d.data    := 0.U
    d.corrupt := false.B
    d
  }

  def Grant(params: TLParameters, fromSink: UInt, toSource: UInt, lgSize: UInt, capPermissions: UInt, data: UInt): TLBundleD = Grant(params, fromSink, toSource, lgSize, capPermissions, data, false.B, false.B)
  def Grant(params: TLParameters, fromSink: UInt, toSource: UInt, lgSize: UInt, capPermissions: UInt, data: UInt, denied: Bool, corrupt: Bool) = {
    val d = Wire(new TLBundleD(params))
    d.opcode  := TLMessages.GrantData
    d.param   := capPermissions
    d.size    := lgSize
    d.source  := toSource
    d.sink    := fromSink
    d.denied  := denied
    d.data    := data
    d.corrupt := corrupt
    d
  }

  def ReleaseAck(c: TLBundleC): TLBundleD = ReleaseAck(c.params, c.source, c.size, false.B)
  def ReleaseAck(params: TLParameters, toSource: UInt, lgSize: UInt, denied: Bool): TLBundleD = {
    val d = Wire(new TLBundleD(params))
    d.opcode  := TLMessages.ReleaseAck
    d.param   := 0.U
    d.size    := lgSize
    d.source  := toSource
    d.sink    := 0.U
    d.denied  := denied
    d.data    := 0.U
    d.corrupt := false.B
    d
  }

  // Accesses
  def Get(params: TLParameters, fromAddress: UInt, toSource: UInt, lgSize: UInt) = {
    // require (client.anySupportGet, s"TileLink: No clients visible from this edge support Gets, but one of these managers would try to issue one: ${manager.managers}")
    // val legal = client.supportsGet(toSource, lgSize)
    val legal = true.B
    val b = Wire(new TLBundleB(params))
    b.opcode  := TLMessages.Get
    b.param   := 0.U
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.mask    := TLUtilities.mask(params, fromAddress, lgSize)
    b.data    := 0.U
    b.corrupt := false.B
    (legal, b)
  }

  def Put(params: TLParameters, fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt): (Bool, TLBundleB) =
    Put(params, fromAddress, toSource, lgSize, data, false.B)

  def Put(params: TLParameters, fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt, corrupt: Bool): (Bool, TLBundleB) = {
    // require (client.anySupportPutFull, s"TileLink: No clients visible from this edge support Puts, but one of these managers would try to issue one: ${manager.managers}")
    // val legal = client.supportsPutFull(toSource, lgSize)
    val legal = true.B
    val b = Wire(new TLBundleB(params))
    b.opcode  := TLMessages.PutFullData
    b.param   := 0.U
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.mask    := TLUtilities.mask(params, fromAddress, lgSize)
    b.data    := data
    b.corrupt := corrupt
    (legal, b)
  }

  def Put(params: TLParameters, fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt, mask: UInt): (Bool, TLBundleB) =
    Put(params, fromAddress, toSource, lgSize, data, mask, false.B)

  def Put(params: TLParameters, fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt, mask: UInt, corrupt: Bool): (Bool, TLBundleB) = {
    // require (client.anySupportPutPartial, s"TileLink: No clients visible from this edge support masked Puts, but one of these managers would try to request one: ${manager.managers}")
    // val legal = client.supportsPutPartial(toSource, lgSize)
    val legal = true.B
    val b = Wire(new TLBundleB(params))
    b.opcode  := TLMessages.PutPartialData
    b.param   := 0.U
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.mask    := mask
    b.data    := data
    b.corrupt := corrupt
    (legal, b)
  }

  def Arithmetic(params: TLParameters, fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt, atomic: UInt, corrupt: Bool = false.B) = {
    // require (client.anySupportArithmetic, s"TileLink: No clients visible from this edge support arithmetic AMOs, but one of these managers would try to request one: ${manager.managers}")
    // val legal = client.supportsArithmetic(toSource, lgSize)
    val legal = true.B
    val b = Wire(new TLBundleB(params))
    b.opcode  := TLMessages.ArithmeticData
    b.param   := atomic
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.mask    := TLUtilities.mask(params, fromAddress, lgSize)
    b.data    := data
    b.corrupt := corrupt
    (legal, b)
  }

  def Logical(params: TLParameters, fromAddress: UInt, toSource: UInt, lgSize: UInt, data: UInt, atomic: UInt, corrupt: Bool = false.B) = {
    // require (client.anySupportLogical, s"TileLink: No clients visible from this edge support logical AMOs, but one of these managers would try to request one: ${manager.managers}")
    // val legal = client.supportsLogical(toSource, lgSize)
    val legal = true.B
    val b = Wire(new TLBundleB(params))
    b.opcode  := TLMessages.LogicalData
    b.param   := atomic
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.mask    := TLUtilities.mask(params, fromAddress, lgSize)
    b.data    := data
    b.corrupt := corrupt
    (legal, b)
  }

  def Hint(params: TLParameters, fromAddress: UInt, toSource: UInt, lgSize: UInt, param: UInt) = {
    // require (client.anySupportHint, s"TileLink: No clients visible from this edge support Hints, but one of these managers would try to request one: ${manager.managers}")
    // val legal = client.supportsHint(toSource, lgSize)
    val legal = true.B
    val b = Wire(new TLBundleB(params))
    b.opcode  := TLMessages.Hint
    b.param   := param
    b.size    := lgSize
    b.source  := toSource
    b.address := fromAddress
    b.mask    := TLUtilities.mask(params, fromAddress, lgSize)
    b.data    := 0.U
    b.corrupt := false.B
    (legal, b)
  }

  def AccessAck(a: TLBundleA): TLBundleD = AccessAck(a.params, a.source, a.size)
  def AccessAck(a: TLBundleA, denied: Bool): TLBundleD = AccessAck(a.params, a.source, a.size, denied)
  def AccessAck(params: TLParameters, toSource: UInt, lgSize: UInt): TLBundleD = AccessAck(params, toSource, lgSize, false.B)
  def AccessAck(params: TLParameters, toSource: UInt, lgSize: UInt, denied: Bool) = {
    val d = Wire(new TLBundleD(params))
    d.opcode  := TLMessages.AccessAck
    d.param   := 0.U
    d.size    := lgSize
    d.source  := toSource
    d.sink    := 0.U
    d.denied  := denied
    d.data    := 0.U
    d.corrupt := false.B
    d
  }

  def AccessAck(a: TLBundleA, data: UInt): TLBundleD = AccessAck(a.params, a.source, a.size, data)
  def AccessAck(a: TLBundleA, data: UInt, denied: Bool, corrupt: Bool): TLBundleD = AccessAck(a.params, a.source, a.size, data, denied, corrupt)
  def AccessAck(params: TLParameters, toSource: UInt, lgSize: UInt, data: UInt): TLBundleD = AccessAck(params, toSource, lgSize, data, false.B, false.B)
  def AccessAck(params: TLParameters, toSource: UInt, lgSize: UInt, data: UInt, denied: Bool, corrupt: Bool) = {
    val d = Wire(new TLBundleD(params))
    d.opcode  := TLMessages.AccessAckData
    d.param   := 0.U
    d.size    := lgSize
    d.source  := toSource
    d.sink    := 0.U
    d.denied  := denied
    d.data    := data
    d.corrupt := corrupt
    d
  }

  def HintAck(a: TLBundleA): TLBundleD = HintAck(a, false.B)
  def HintAck(a: TLBundleA, denied: Bool): TLBundleD = HintAck(a.params, a.source, a.size, denied)
  def HintAck(params: TLParameters, toSource: UInt, lgSize: UInt): TLBundleD = HintAck(params, toSource, lgSize, false.B)
  def HintAck(params: TLParameters, toSource: UInt, lgSize: UInt, denied: Bool) = {
    val d = Wire(new TLBundleD(params))
    d.opcode  := TLMessages.HintAck
    d.param   := 0.U
    d.size    := lgSize
    d.source  := toSource
    d.sink    := 0.U
    d.denied  := denied
    d.data    := 0.U
    d.corrupt := false.B
    d
  }
}
