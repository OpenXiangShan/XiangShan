package utils

import chisel3._
import chisel3.util._

object RegMap {
  def apply(addr: Int, reg: UInt, wfn: UInt => UInt = (x => x)) = (addr, (reg, wfn))
  def generate(mapping: Map[Int, (UInt, UInt => UInt)], addr: UInt, rdata: UInt,
    wen: Bool, wdata: UInt, wmask: UInt) = {
    val chiselMapping = mapping.map { case (a, (r, w)) => (a.U, r, w) }
    rdata := LookupTree(addr, chiselMapping.map { case (a, r, w) => (a, r) })
    when (wen) {
      chiselMapping.map { case (a, r, w) => when (addr === a) { r := w(MaskData(r, wdata, wmask)) } }
    }
  }
}
