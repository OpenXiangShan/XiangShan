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

package cache

import chipsalliance.rocketchip.config.{Field, Parameters}
import chisel3._
import chisel3.util._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest._
import chisel3.experimental.BundleLiterals._
import firrtl.stage.RunFirrtlTransformAnnotation
import chiseltest.ChiselScalatestTester
import device.AXI4RAM
import freechips.rocketchip.amba.axi4.AXI4UserYanker
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, IdRange}
import freechips.rocketchip.tilelink.{TLBuffer, TLCacheCork, TLToAXI4, TLXbar, TLMasterParameters, TLMasterPortParameters, TLClientNode}
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import sifive.blocks.inclusivecache.{CacheParameters, InclusiveCache, InclusiveCacheMicroParameters, InclusiveCacheControlParameters}
import utils.{DebugIdentityNode, HoldUnless, XSDebug}
import xiangshan.{XSBundle, HasXSParameter}
import xiangshan.cache.{DCache, Uncache, DCacheLineReq, DCacheWordReq, MemoryOpConstants}
import xiangshan.testutils.AddSinks
import xstransforms.PrintModuleName
import utils.MaskExpand


import scala.util.Random

// GetGenerator: a tilelink module that generate get of different addr and sizes
class GetGeneratorReq(implicit p: Parameters) extends XSBundle
{
  val address  = Output(UInt(PAddrBits.W))
  val size  = Output(UInt(8.W))
}

class GetGeneratorResp(implicit p: Parameters) extends XSBundle
{
  val data  = Output(UInt((64 * 8).W))
}

class GetGeneratorIO(implicit p: Parameters) extends XSBundle
{
  val req  = DecoupledIO(new GetGeneratorReq)
  val resp = Flipped(DecoupledIO(new GetGeneratorResp))
}

class GetGenerator()(implicit p: Parameters) extends LazyModule with HasXSParameter {

  val clientParameters = TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "GetGenerator",
      sourceId = IdRange(0, 1)
    ))
  )

  val clientNode = TLClientNode(Seq(clientParameters))

  lazy val module = new GetGeneratorImp(this)
}

class GetGeneratorImp(outer: GetGenerator) extends LazyModuleImp(outer)
  with HasXSParameter
{

  val io = IO(Flipped(new GetGeneratorIO))

  val (bus, edge) = outer.clientNode.out.head
  require(bus.d.bits.data.getWidth == l1BusDataWidth, "GetGenerator: tilelink width does not match")

  // assign default values to output signals
  io.req.ready           := false.B
  io.resp.valid          := false.B
  io.resp.bits           := DontCare
  
  bus.a.valid := false.B
  bus.a.bits  := DontCare
  bus.b.ready := false.B
  bus.c.valid := false.B
  bus.c.bits  := DontCare
  bus.d.ready := false.B
  bus.e.valid := false.B
  bus.e.bits  := DontCare

  val mem_acquire = bus.a
  val mem_grant = bus.d

  // tilelink req/resp state machine
  val s_invalid :: s_refill_req :: s_refill_resp :: s_send_resp :: Nil = Enum(4)

  val state = RegInit(s_invalid)

  val req = Reg(new GetGeneratorReq)

  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(mem_grant)

  val refillCycles = 2
  val refill_ctr  = Reg(UInt(log2Up(refillCycles).W))

  val blockSize = 64
  val beatBytes = l1BusDataWidth / 8
  val nBeats = blockSize / beatBytes
  val refill_data = Reg(Vec(nBeats, UInt(l1BusDataWidth.W)))

  when (state =/= s_invalid) {
    XSDebug("state: %d\n", state)
  }

  // --------------------------------------------
  // s_invalid: receive requests
  when (state === s_invalid) {
    io.req.ready := true.B

    when (io.req.fire()) {
      refill_ctr := 0.U
      req := io.req.bits
      state := s_refill_req

      (0 until nBeats) map { i => refill_data(i) := 0.U }
    }
  }

  // --------------------------------------------
  // refill
  when (state === s_refill_req) {
    mem_acquire.valid := true.B
    mem_acquire.bits  := edge.Get(
      fromSource      = 0.U,
      toAddress       = req.address,
      lgSize          = req.size)._2
    when (mem_acquire.fire()) {
      state := s_refill_resp
    }
  }

  when (state === s_refill_resp) {
    mem_grant.ready := true.B

    when (edge.hasData(mem_grant.bits)) {
      when (mem_grant.fire()) {
        refill_ctr := refill_ctr + 1.U
        val beatIdx = (req.address(log2Up(blockSize) - 1, 0) >> log2Up(beatBytes)) + refill_ctr
        val mask = MaskExpand(edge.mask(req.address, req.size))
        // zero out unneeded data, so that, we can use expect to compare data outputs
        XSDebug("beatIdx: %d data: %x mask: %x\n", beatIdx, mem_grant.bits.data, mask)
        refill_data(beatIdx) := mem_grant.bits.data & mask

        when (refill_done) {
          state := s_send_resp
        }
      }
    }
  }

  // --------------------------------------------
  when (state === s_send_resp) {

    val resp_data = Cat((0 until nBeats).reverse map { r => refill_data(r) })
    io.resp.valid     := true.B
    io.resp.bits.data := resp_data

    when (io.resp.fire()) {
      state := s_invalid
    }
  }

  // debug output
  when (io.req.fire()) {
    XSDebug("address: %x size: %d\n", io.req.bits.address, io.req.bits.size)
  }

  when (io.resp.fire()) {
    XSDebug("data: %x\n", io.resp.bits.data)
  }
}

case object UnalignedGetTestKey extends Field[Long]

class UnalignedGetTestTopIO extends Bundle {
  val in = Flipped(DecoupledIO(new Bundle() {
    val wdata = Input(UInt(512.W))
    val waddr = Input(UInt(20.W))
    val raddr = Input(UInt(20.W))
    val rsize  = Input(UInt(8.W))
  }))
  val out = DecoupledIO(new Bundle() {
    val rdata = Output(UInt(512.W))
  })
}

class UnalignedGetTestTop()(implicit p: Parameters) extends LazyModule{

  // use uncache to force L2 eviction
  // so that we can test uncached get
  val uncache = LazyModule(new Uncache())
  val dcache = LazyModule(new DCache())
  val getGenerator = LazyModule(new GetGenerator())
  val l2 = LazyModule(new InclusiveCache(
    CacheParameters(
      level = 2,
      ways = 4,
      sets = 4 * 1024 / (64 * 4 * 4),
      blockBytes = 64,
      beatBytes = 32,
      cacheName = s"L2"
    ),
    InclusiveCacheMicroParameters(
      writeBytes = 8
    ),
    Some(InclusiveCacheControlParameters(
      address = 0x8000000L,
      beatBytes = 8))))

  val ram = LazyModule(new AXI4RAM(
    Seq(AddressSet(0x0L, 0xffffffffffL)),
    memByte = 128 * 1024 * 1024,
    useBlackBox = false
  ))

  val xbar = TLXbar()

  xbar := TLBuffer() := DebugIdentityNode() := dcache.clientNode
  xbar := TLBuffer() := DebugIdentityNode() := getGenerator.clientNode

  l2.node := DebugIdentityNode() := xbar

  ram.node :=
    AXI4UserYanker() :=
    TLToAXI4() :=
    TLBuffer() :=
    TLCacheCork() :=
    DebugIdentityNode() := 
    l2.node

  // connect uncache access to l2 control node
  l2.ctlnode.get := DebugIdentityNode() := uncache.clientNode

  lazy val module = new LazyModuleImp(this) {

    val io = IO(new UnalignedGetTestTopIO)

    val in = HoldUnless(io.in.bits, io.in.fire())

    dcache.module.io <> DontCare
    uncache.module.io <> DontCare

    val flushPort = uncache.module.io.lsq
    val storePort = dcache.module.io.lsu.store
    val loadPort  = getGenerator.module.io

    // 64bit full mask
    val FULL_MASK_64 = BigInt("ffffffffffffffff", 16).U
    val L2_FLUSH_BASE_ADDR = 0x8000000L
    val CONFIG_ADDR = L2_FLUSH_BASE_ADDR + 0x0
    val FLUSH64_ADDR = L2_FLUSH_BASE_ADDR + 0x200
    val FLUSH32_ADDR = L2_FLUSH_BASE_ADDR + 0x240

    def sendFlushReq(addr: UInt): DCacheWordReq = {
      val req = Wire(new DCacheWordReq)
      req.cmd  := MemoryOpConstants.M_XWR
      req.addr := FLUSH64_ADDR.U
      req.data := addr
      req.mask := FULL_MASK_64
      req.id   := 0.U
      req
    }

    def sendStoreReq(addr: UInt, data: UInt): DCacheLineReq = {
      val req = Wire(new DCacheLineReq)
      req.cmd := MemoryOpConstants.M_XWR
      req.addr := addr
      req.data := data
      req.mask := Fill(req.mask.getWidth, true.B)
      req.id   := DontCare
      req
    }

    def sendLoadReq(addr: UInt, size: UInt): GetGeneratorReq = {
      val req = Wire(new GetGeneratorReq)
      req.address := addr
      req.size := size
      req
    }

    val s_idle :: s_write_req :: s_write_resp :: s_flush_req :: s_flush_resp :: s_read_req :: s_read_resp :: s_finish :: Nil = Enum(8)
    val state = RegInit(s_idle)

    switch(state){
      is(s_idle){
        when(io.in.fire()){
          state := s_write_req
        }
      }
      is(s_write_req){
        when(storePort.req.fire()) {
          state := s_write_resp
        }
      }
      is(s_write_resp){
        when(storePort.resp.fire()) {
          state := s_flush_req
        }
      }
      is(s_flush_req){
        when(flushPort.req.fire()) {
          state := s_flush_resp
        }
      }
      is(s_flush_resp){
        when(flushPort.resp.fire()) {
          state := s_read_req
        }
      }
      is(s_read_req){
        when(loadPort.req.fire()) {
          state := s_read_resp
        }
      }
      is(s_read_resp){
        when(loadPort.resp.fire()) {
          state := s_finish
        }
      }
    }

    io.in.ready := state === s_idle

    XSDebug(p"state: $state\n")

    val storeReq = Wire(new DCacheLineReq)

    storeReq := sendStoreReq(in.waddr, in.wdata)

    storePort.req.bits := storeReq
    storePort.req.valid := state === s_write_req
    storePort.resp.ready := true.B
    XSDebug(
      storePort.req.fire(),
      "write data %x to dcache\n",
      storePort.req.bits.data,
    )

    val flushReq = Wire(new DCacheWordReq)

    flushReq := sendFlushReq(in.waddr)

    flushPort.req.bits := flushReq
    flushPort.req.valid := state === s_flush_req
    flushPort.resp.ready := true.B
    XSDebug(
      flushPort.req.fire(),
      "flush address %x to memory\n",
      flushPort.req.bits.addr,
    )

    val loadReq = sendLoadReq(in.raddr, in.rsize)

    loadPort.req.bits := loadReq
    loadPort.req.valid := state === s_read_req
    loadPort.resp.ready := true.B
    XSDebug(
      loadPort.resp.fire(),
      "read data %x form getGenerator\n",
      loadPort.resp.bits.data,
    )

    val rdata = Reg(UInt(512.W))

    when(loadPort.resp.fire()) {
      state := s_finish
      rdata := loadPort.resp.bits.data
    }

    io.out.bits.rdata := rdata
    io.out.valid := state === s_finish

    when(io.out.fire()){
      state := s_idle
    }
  }

}

class UnalignedGetTestTopWrapper()(implicit p: Parameters) extends LazyModule {

  val testTop = LazyModule(new UnalignedGetTestTop())

  lazy val module = new LazyModuleImp(this){
    val io = IO(new UnalignedGetTestTopIO)

    AddSinks()

    io <> testTop.module.io
  }
}

class UnalignedGetTest extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  val annos = Seq(
    VerilatorBackendAnnotation,
    RunFirrtlTransformAnnotation(new PrintModuleName)
  )

  it should "run" in {

    implicit val p = Parameters((site, up, here) => {
      case UnalignedGetTestKey => 0
    })


    test(LazyModule(new UnalignedGetTestTopWrapper()).module)
      .withAnnotations(annos){ c =>


        c.io.in.initSource().setSourceClock(c.clock)
        c.io.out.initSink().setSinkClock(c.clock)

        c.clock.step(100)

        val mem_size = 128 * 1024 * 1024
        val block_size = 64
        val nblocks = mem_size / block_size
        // val nblocks = 100
        for(i <- 0 until nblocks) {
          // we do not support l1plus flush for now
          // so we could only scan the whole memory,
          // and write every block for only once.
          // if we rewrite the same block multiple times
          // GetGenerator could not give correct data since it hasn't been flushed
          // val addr = Random.nextInt(0xfffff) & 0xffe00 // align to block size
          val waddr = i * block_size
          val words = (0 until 8) map { _ =>
            (BigInt(Random.nextLong() & 0x7fffffffffffffffL))
          }
          val wdata = words.foldLeft(BigInt(0))((sum, i) => sum << 64 | i)

          val maxSize = block_size
          val lgMaxSize = log2Up(maxSize)
          val lgRsize = Random.nextInt(lgMaxSize + 1)
          val rsize = 1 << lgRsize

          // addr must be aligned to size
          val offset = (Random.nextInt(maxSize) >> lgRsize) << lgRsize
          val raddr = waddr + offset
          // generate mask from  raddr and rsize
          val mask = (BigInt(1) << (rsize * 8)) - 1
          val rmask = mask << (offset * 8)
          val rdata = wdata & rmask

          println(f"UnalignedGetTest: waddr: $waddr%x wdata: $wdata%x offset: $offset%x rsize: $rsize%d rmask: $rmask%x rdata: $rdata%x")
          c.io.in.enqueue(chiselTypeOf(c.io.in.bits).Lit(
            _.waddr -> waddr.U,
            _.wdata -> wdata.U,
            _.raddr -> raddr.U,
            _.rsize -> lgRsize.U
          ))
          c.io.out.expectDequeue(chiselTypeOf(c.io.out.bits).Lit(
            _.rdata -> rdata.U
          ))
        }
    }
  }
}
