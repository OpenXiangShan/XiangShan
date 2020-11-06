package cache

import Chisel.UInt
import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chiseltest.ChiselScalatestTester
import device.AXI4RAM
import freechips.rocketchip.amba.axi4.AXI4UserYanker
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink.{TLBuffer, TLCacheCork, TLDelayer, TLMessages, TLPermissions, TLToAXI4, TLXbar}
import org.scalatest.{FlatSpec, Matchers}
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters}
import utils.{DebugIdentityNode, HoldUnless, XSDebug}
import xiangshan.HasXSLog
import xiangshan.testutils.AddSinks

import scala.collection.mutable.{Map, Seq, ListBuffer}

import scala.util.Random

class TLCAgent() extends TLCOp{

}
class TLCSlaveAgent() extends TLCAgent{
  val innerAcquire : ListBuffer[AcquireCalleeTrans] = ListBuffer()
  val innerRelease : ListBuffer[ReleaseCalleeTrans] = ListBuffer()
  val innerProbe : ListBuffer[ProbeCallerTrans] = ListBuffer()
}