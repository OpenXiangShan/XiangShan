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

package cache.TLCTest

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.IDMapGenerator
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

object FixedLFSR64
{
  def apply(increment: Bool = Bool(true)): UInt =
  {
    val wide = 64
    val lfsr = Reg(UInt(value = 0xabcd4face1L ,width = wide)) // fixed initial value
    val xor = lfsr(0) ^ lfsr(1) ^ lfsr(3) ^ lfsr(4)
    when (increment) {
      lfsr := Mux(lfsr === UInt(0), UInt(1), Cat(xor, lfsr(wide-1,1)))
    }
    lfsr
  }
}

trait HasNoiseMakerIO
{
  val io = new Bundle {
    val inc = Bool(INPUT)
    val random = UInt(OUTPUT)
  }
}

class FixedLFSRNoiseMaker(wide: Int) extends Module with HasNoiseMakerIO
{
  val lfsrs = Seq.fill((wide+63)/64) { FixedLFSR64(io.inc) }
  io.random := Cat(lfsrs)(wide-1,0)
}

object FixedLFSRNoiseMaker {
  def apply(wide: Int, increment: Bool = Bool(true)): UInt = {
    val nm = Module(new FixedLFSRNoiseMaker(wide))
    nm.io.inc := increment
    nm.io.random
  }
}

/** TLFuzzer drives test traffic over TL2 links. It generates a sequence of randomized
  * requests, and issues legal ones into the DUT. TODO: Currently the fuzzer only generates
  * memory operations, not permissions transfers.
  * @param nOperations is the total number of operations that the fuzzer must complete for the test to pass (0 => run forever)
  * @param inFlight is the number of operations that can be in-flight to the DUT concurrently
  * @param noiseMaker is a function that supplies a random UInt of a given width every time inc is true
  */
class FixedBlockFuzzer(
  nOperations: Int,
  inFlight: Int = 32,
  noiseMaker: (Int, Bool, Int) => UInt = {
    (wide: Int, increment: Bool, abs_values: Int) =>
    FixedLFSRNoiseMaker(wide=wide, increment=increment)
  },
  noModify: Boolean = false,
  overrideAddress: Option[AddressSet] = None,
  nOrdered: Option[Int] = None)(implicit p: Parameters) extends LazyModule
{

  val clientParams = if (nOrdered.isDefined) {
    val n = nOrdered.get
    require(n > 0, s"nOrdered must be > 0, not $n")
    require((inFlight % n) == 0, s"inFlight (${inFlight}) must be evenly divisible by nOrdered (${nOrdered}).")
    Seq.tabulate(n) {i =>
      TLMasterParameters.v1(name =s"OrderedFuzzer$i",
        sourceId = IdRange(i * (inFlight/n),  (i + 1)*(inFlight/n)),
        requestFifo = true)
    }
  } else {
    Seq(TLMasterParameters.v1(
      name = "Fuzzer",
      sourceId = IdRange(0,inFlight)
    ))
  }

  val node = TLClientNode(Seq(TLMasterPortParameters.v1(clientParams)))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val finished = Bool(OUTPUT)
      val blockAddr = Input(UInt(64.W))
    })

    val (out, edge) = node.out(0)

    // Extract useful parameters from the TL edge
    val maxTransfer  = edge.manager.maxTransfer
    val beatBytes    = edge.manager.beatBytes
    val maxLgBeats   = log2Up(maxTransfer/beatBytes)
    val addressBits  = log2Up(overrideAddress.map(_.max).getOrElse(edge.manager.maxAddress))
    val sizeBits     = edge.bundle.sizeBits
    val dataBits     = edge.bundle.dataBits

    // Progress through operations
    val num_reqs = Reg(init = UInt(nOperations, log2Up(nOperations+1)))
    val num_resps = Reg(init = UInt(nOperations, log2Up(nOperations+1)))
    if (nOperations>0) {
      io.finished := num_resps === UInt(0)
    } else {
      io.finished := Bool(false)
    }

    // Progress within each operation
    val a = out.a.bits
    val (a_first, a_last, req_done) = edge.firstlast(out.a)

    val d = out.d.bits
    val (d_first, d_last, resp_done) = edge.firstlast(out.d)

    // Source ID generation
    val idMap = Module(new IDMapGenerator(inFlight))
    val src = idMap.io.alloc.bits holdUnless a_first
    // Increment random number generation for the following subfields
    val inc = Wire(Bool())
    val inc_beat = Wire(Bool())
    val blockAddrReg = RegEnable(io.blockAddr,0.U(64.W),inc)
    val arth_op_3 = noiseMaker(3, inc, 0)
    val arth_op   = Mux(arth_op_3 > UInt(4), UInt(4), arth_op_3)
    val log_op    = noiseMaker(2, inc, 0)
    val amo_size  = UInt(2) + noiseMaker(1, inc, 0) // word or dword
    val size      = noiseMaker(sizeBits, inc, 0)
    val rawAddr   = Cat(blockAddrReg(63,6),noiseMaker(addressBits, inc, 2)(5,0))
    val addr      = overrideAddress.map(_.legalize(rawAddr)).getOrElse(rawAddr) & ~UIntToOH1(size, addressBits)
    val mask      = noiseMaker(beatBytes, inc_beat, 2) & edge.mask(addr, size)
    val data      = noiseMaker(dataBits, inc_beat, 2)

    // Actually generate specific TL messages when it is legal to do so
    val (glegal,  gbits)  = edge.Get(src, addr, size)
    val (pflegal, pfbits) = if(edge.manager.anySupportPutFull) {
                              edge.Put(src, addr, size, data)
                            } else { (glegal, gbits) }
    val (pplegal, ppbits) = if(edge.manager.anySupportPutPartial) {
                              edge.Put(src, addr, size, data, mask)
                            } else { (glegal, gbits) }
//    val (alegal,  abits)  = if(edge.manager.anySupportArithmetic) {
//                              edge.Arithmetic(src, addr, size, data, arth_op)
//                            } else { (glegal, gbits) }
//    val (llegal,  lbits)  = if(edge.manager.anySupportLogical) {
//                             edge.Logical(src, addr, size, data, log_op)
//                            } else { (glegal, gbits) }
//    val (hlegal,  hbits)  = if(edge.manager.anySupportHint) {
//                              edge.Hint(src, addr, size, UInt(0))
//                            } else { (glegal, gbits) }

    val legal_dest = edge.manager.containsSafe(addr)

    // Pick a specific message to try to send
    val a_type_sel  = noiseMaker(2, inc, 0)

    val legal = legal_dest && MuxLookup(a_type_sel, glegal, Seq(
      UInt("b00") -> glegal,
      UInt("b01") -> (pflegal && !Bool(noModify)),
      UInt("b10") -> (pplegal && !Bool(noModify))
//      UInt("b011") -> (alegal && !Bool(noModify)),
//      UInt("b100") -> (llegal && !Bool(noModify)),
//      UInt("b101") -> hlegal
    ))

    val bits = MuxLookup(a_type_sel, gbits, Seq(
      UInt("b00") -> gbits,
      UInt("b01") -> pfbits,
      UInt("b10") -> ppbits,
//      UInt("b011") -> abits,
//      UInt("b100") -> lbits,
//      UInt("b101") -> hbits
    ))

    // Wire up Fuzzer flow control
    val a_gen = if (nOperations>0) num_reqs =/= UInt(0) else Bool(true)
    out.a.valid := !reset && a_gen && legal && (!a_first || idMap.io.alloc.valid)
    idMap.io.alloc.ready := a_gen && legal && a_first && out.a.ready
    idMap.io.free.valid := d_first && out.d.fire()
    idMap.io.free.bits := out.d.bits.source

    out.a.bits  := bits
    out.b.ready := Bool(true)
    out.c.valid := Bool(false)
    out.d.ready := Bool(true)
    out.e.valid := Bool(false)

    // Increment the various progress-tracking states
    inc := !legal || req_done
    inc_beat := !legal || out.a.fire()

    if (nOperations>0) {
      when (out.a.fire() && a_last) {
        num_reqs := num_reqs - UInt(1)
      }

      when (out.d.fire() && d_last) {
        num_resps := num_resps - UInt(1)
      }
    }
  }
}

object FixedBlockFuzzer
{
  def apply(
    nOperations: Int,
    inFlight: Int = 32,
    noiseMaker: (Int, Bool, Int) => UInt = {
      (wide: Int, increment: Bool, abs_values: Int) =>
      FixedLFSRNoiseMaker(wide=wide, increment=increment)
    },
    noModify: Boolean = false,
    overrideAddress: Option[AddressSet] = None,
    nOrdered: Option[Int] = None)(implicit p: Parameters): TLOutwardNode =
  {
    val fuzzer = LazyModule(new FixedBlockFuzzer(nOperations, inFlight, noiseMaker, noModify, overrideAddress, nOrdered))
    fuzzer.node
  }
}