package xiangshan.mem.cache

import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import chisel3.util.experimental.BoringUtils
import xiangshan.backend.decode.XSTrap
import xiangshan.mem._
import xiangshan.mem.pipeline._
import bus.simplebus._

trait DtlbConst extends HasXSParameter with HasMEMConst {
  val L1EntryNum = 32
  val Level = 3
  val L2EntryNum = 1024 // every layer has L2EntryNum entries

  val offLen  = 12
  val ppn0Len = 9
  val ppn1Len = 9
  val ppn2Len = PAddrBits - offLen - ppn0Len - ppn1Len
  val ppnLen  = ppn0Len + ppn1Len + ppn2Len
  val vpn0Len = 9
  val vpn1Len = 9
  val vpn2Len = VAddrBits - offLen - vpn0Len - vpn1Len
  val vpnLen  = vpn0Len + vpn1Len + vpn2Len

  val entryLen = XLEN
  val flagLen = 8
  val pteResLen = XLEN - ppnLen - 2 - flagLen

  def vaBundle = new Bundle {
    val vpn2 = UInt(vpn2Len.W)
    val vpn1 = UInt(vpn1Len.W)
    val vpn0 = UInt(vpn0Len.W)
    val off  = UInt( offLen.W)
  }

  def vaBundle2 = new Bundle {
    val vpn  = UInt(vpnLen.W)
    val off  = UInt(offLen.W)
  }

  def paBundle = new Bundle {
    val ppn2 = UInt(ppn2Len.W)
    val ppn1 = UInt(ppn1Len.W)
    val ppn0 = UInt(ppn0Len.W)
    val off  = UInt( offLen.W)
  }

  def paBundle2 = new Bundle {
    val ppn  = UInt(ppnLen.W)
    val off  = UInt(offLen.W)
  }

   def pteBundle = new Bundle {
    val reserved  = UInt(pteResLen.W)
    val ppn  = UInt(ppnLen.W)
    val rsw  = UInt(2.W)
    val flag = new Bundle {
      val d    = UInt(1.W)
      val a    = UInt(1.W)
      val g    = UInt(1.W)
      val u    = UInt(1.W)
      val x    = UInt(1.W)
      val w    = UInt(1.W)
      val r    = UInt(1.W)
      val v    = UInt(1.W)
    }
  }
}

class DtlbReq extends XSBundle with HasMEMConst {
  val vaddr = UInt(VAddrBits.W)
}

class DtlbResp extends XSBundle with HasMEMConst {
  val paddr = UInt(PAddrBits.W)
  val miss = Bool()
}

class DtlbToLsuIO extends XSBundle with HasMEMConst {
  val req = Vec(LoadPipelineWidth + StorePipelineWidth, Flipped(Valid(new DtlbReq)))
  val resp = Vec(LoadPipelineWidth + StorePipelineWidth, Valid(new DtlbResp))
}

class DtlbIO extends XSBundle with HasMEMConst {
  val lsu = new DtlbToLsuIO
  // val l2 = TODO
}

class Dtlb extends XSModule with HasMEMConst {
  val io = IO(new DtlbIO)
  // Dtlb has 4 ports: 2 for load, 2 fore store 

  // fake dtlb
  (0 until LoadPipelineWidth + StorePipelineWidth).map(i => {
    io.lsu.resp(i).valid := io.lsu.req(i).valid
    io.lsu.resp(i).bits.paddr := io.lsu.req(i).bits.vaddr
    io.lsu.resp(i).bits.miss := DontCare
  })
}