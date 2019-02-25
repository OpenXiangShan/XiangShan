// See LICENSE.SiFive for license details.

package bus.axi4

import chisel3._
import chisel3.util._

object AXI4Parameters {
  // These are all fixed by the AXI4 standard:
  val lenBits   = 8
  val sizeBits  = 3
  val burstBits = 2
  val cacheBits = 4
  val protBits  = 3
  val qosBits   = 4
  val respBits  = 2

  // These are not fixed:
  val idBits    = 1
  val addrBits  = 32
  val dataBits  = 32
  val userBits  = 1

  def CACHE_RALLOCATE  = 8.U(cacheBits.W)
  def CACHE_WALLOCATE  = 4.U(cacheBits.W)
  def CACHE_MODIFIABLE = 2.U(cacheBits.W)
  def CACHE_BUFFERABLE = 1.U(cacheBits.W)

  def PROT_PRIVILEDGED = 1.U(protBits.W)
  def PROT_INSECURE    = 2.U(protBits.W)
  def PROT_INSTRUCTION = 4.U(protBits.W)

  def BURST_FIXED = 0.U(burstBits.W)
  def BURST_INCR  = 1.U(burstBits.W)
  def BURST_WRAP  = 2.U(burstBits.W)

  def RESP_OKAY   = 0.U(respBits.W)
  def RESP_EXOKAY = 1.U(respBits.W)
  def RESP_SLVERR = 2.U(respBits.W)
  def RESP_DECERR = 3.U(respBits.W)
}

abstract class AXI4BundleA extends Bundle {
  val id    = Output(UInt(AXI4Parameters.idBits.W))
  val addr  = Output(UInt(AXI4Parameters.addrBits.W))
  val len   = Output(UInt(AXI4Parameters.lenBits.W))  // number of beats - 1
  val size  = Output(UInt(AXI4Parameters.sizeBits.W)) // bytes in beat = 2^size
  val burst = Output(UInt(AXI4Parameters.burstBits.W))
  val lock  = Output(Bool())
  val cache = Output(UInt(AXI4Parameters.cacheBits.W))
  val prot  = Output(UInt(AXI4Parameters.protBits.W))
  val qos   = Output(UInt(AXI4Parameters.qosBits.W))  // 0=no QoS, bigger = higher priority
  val user  = Output(UInt(AXI4Parameters.userBits.W))
  // val region = UInt(width = 4) // optional

}

class AXI4BundleAW extends AXI4BundleA
class AXI4BundleAR extends AXI4BundleA

class AXI4BundleW extends Bundle {
  // id ... removed in AXI4
  val data = Output(UInt(AXI4Parameters.dataBits.W))
  val strb = Output(UInt((AXI4Parameters.dataBits/8).W))
  val last = Output(Bool())
}

class AXI4BundleB extends Bundle {
  val id   = Output(UInt(AXI4Parameters.idBits.W))
  val resp = Output(UInt(AXI4Parameters.respBits.W))
  val user = Output(UInt(AXI4Parameters.userBits.W))
}

class AXI4BundleR extends AXI4BundleB {
  val data = Output(UInt(AXI4Parameters.dataBits.W))
  val last = Output(Bool())
}

class AXI4 extends Bundle {
  val aw = Decoupled(new AXI4BundleAW)
  val w  = Decoupled(new AXI4BundleW)
  val b  = Flipped(Decoupled(new AXI4BundleB))
  val ar = Decoupled(new AXI4BundleAR)
  val r  = Flipped(Decoupled(new AXI4BundleR))
}
