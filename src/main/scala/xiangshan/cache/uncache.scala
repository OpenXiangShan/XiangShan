package xiangshan.cache

import chisel3._
import chisel3.util._

import utils.XSDebug
import bus.tilelink._
import xiangshan.{MicroOp, Redirect, NeedImpl}

class UncacheIO extends DCacheBundle {
  val lsroq = Flipped(new DCacheLoadIO)
  val bus = new TLCached(l1BusParams)
}

// convert DCacheIO to TileLink
class Uncache extends DCacheModule with NeedImpl{
  val io = IO(new UncacheIO)
}
