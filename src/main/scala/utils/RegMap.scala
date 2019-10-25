package utils

import chisel3._
import chisel3.util._

object RegMap {
  def Unwritable = null
  def apply(addr: Int, reg: UInt, wfn: UInt => UInt = (x => x)) = (addr, (reg, wfn))
  def generate(mapping: Map[Int, (UInt, UInt => UInt)], raddr: UInt, rdata: UInt,
    waddr: UInt, wen: Bool, wdata: UInt, wmask: UInt):Unit = {
    val chiselMapping = mapping.map { case (a, (r, w)) => (a.U, r, w) }
    rdata := LookupTree(raddr, chiselMapping.map { case (a, r, w) => (a, r) })
    chiselMapping.map { case (a, r, w) =>
      if (w != null) when (wen && waddr === a) { r := w(MaskData(r, wdata, wmask)) }
    }
  }
  def generate(mapping: Map[Int, (UInt, UInt => UInt)], addr: UInt, rdata: UInt,
    wen: Bool, wdata: UInt, wmask: UInt):Unit = generate(mapping, addr, rdata, addr, wen, wdata, wmask)
}

object MaskedRegMap { // TODO: add read mask
  def Unwritable = null
  def WritableMask = Fill(64, true.B)
  def UnwritableMask = 0.U(64.W)
  def apply(addr: Int, reg: UInt, wmask: UInt = WritableMask, wfn: UInt => UInt = (x => x)) = (addr, (reg, wmask, wfn))
  def generate(mapping: Map[Int, (UInt, UInt, UInt => UInt)], raddr: UInt, rdata: UInt,
    waddr: UInt, wen: Bool, wdata: UInt):Unit = {
    val chiselMapping = mapping.map { case (a, (r, m, w)) => (a.U, r, m, w) }
    rdata := LookupTree(raddr, chiselMapping.map { case (a, r, m, w) => (a, r) })
    chiselMapping.map { case (a, r, m, w) =>
      if (w != null && m != UnwritableMask) when (wen && waddr === a) { r := w(MaskData(r, wdata, m)) }
    }
  }
  def generate(mapping: Map[Int, (UInt, UInt, UInt => UInt)], addr: UInt, rdata: UInt,
    wen: Bool, wdata: UInt):Unit = generate(mapping, addr, rdata, addr, wen, wdata)
}
