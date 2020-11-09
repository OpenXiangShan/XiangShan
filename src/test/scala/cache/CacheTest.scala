package top

import device._
import utils._

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

// To run the following cache random test, do the following:
// * uncomment the following class
// * comment the NOOPSimTop class in noop/src/test/scala/top/NOOPSim.scala
// * define the macro CACHE_TEST in noop/src/test/csrc/emu.h:141
// * run 'make cache' under noop/

/*
class NOOPSimTop extends Module {
  val io = IO(new Bundle{
    val difftest = new DiffTestIO
  })

  val memBase = 0x80000000L
  val cacheSizeKB = 1 // Bytes
  val memSizeB = 4096 // Bytes
  val NRmemBlock = memSizeB / 8
  val printCnt = 100000

  val Name = "dcache"
  val in = WireInit(0.U.asTypeOf(new SimpleBusUC(userBits = 33)))
  val cohIn = WireInit(0.U.asTypeOf(new SimpleBusUC))
  val mmioOut = WireInit(0.U.asTypeOf(new SimpleBusUC))

  val cacheOut = Cache(in, mmioOut, "b00".U)(CacheConfig(name = Name, totalSize = cacheSizeKB, userBits = 33, ways = 4))
  val mem = Module(new AXI4RAM(memByte = memSizeB, useBlackBox = false))
  mem.io.in <> cacheOut.mem.toAXI4()
  cacheOut.coh <> cohIn

  def addrInRange(x: UInt) = (x >= memBase.U) && x < (memBase + memSizeB).U

  val perfCntHit = WireInit(false.B)
  BoringUtils.addSink(perfCntHit, "perfCntCondM" + Name + "Hit")

  val s_init_req :: s_init_resp :: s_test :: Nil = Enum(3)
  val state = RegInit(s_init_req)

  val initCnt = Counter(NRmemBlock)
  switch (state) {
    is (s_init_req) {
      when (in.req.fire()) { state := s_init_resp }
    }
    is (s_init_resp) {
      when (in.resp.fire()) {
        val wrap = initCnt.inc()
        state := Mux(wrap, s_test, s_init_req)
        when (wrap) {
          printf("One '.' for handling %d requests from CPU, and one '@' for handling %d coherence requests\n",
            printCnt.U, printCnt.U)
        }
      }
    }
  }

  val initAddr = memBase.U + initCnt.value * 8.U
  val initWmask = 0xff.U
  val initCmd = SimpleBusCmd.write

  val randBundle = new Bundle {
    val isWrite = Bool()
    val readyChoose = UInt(2.W)
    val wmask = UInt(8.W)
    val addr = UInt(log2Up(NRmemBlock).W)
    val cohChoose = UInt(1.W)
    val cohAddr = UInt(log2Up(NRmemBlock).W)
    val cohReadyChoose = UInt(2.W)
  }
  val rand = LFSR64(true.B).asTypeOf(randBundle)
  val randAddr = memBase.U + rand.addr * 8.U
  val randWmask = rand.wmask
  val randCmd = Mux(rand.isWrite, SimpleBusCmd.write, SimpleBusCmd.read)

  val addr = Mux(state === s_test, randAddr, initAddr)
  val wmask = Mux(state === s_test, randWmask, initWmask)
  val cmd = Mux(state === s_test, randCmd, initCmd)
  val wdata = Fill(2, addr)
  val user = Cat(addr, Mux(state === s_test, rand.isWrite, true.B))
  in.req.bits.apply(addr = addr, size = "b11".U, user = user,
    wdata = wdata, wmask = wmask, cmd = cmd)
  in.req.valid := (state === s_init_req) || (state === s_test)
  in.resp.ready := rand.readyChoose =/= 0.U

  val cohInflight = RegInit(false.B)
  when (cohIn.resp.fire()) {
    val resp = cohIn.resp.bits
    val isProbeEnd = resp.isProbeMiss() || (resp.cmd === SimpleBusCmd.readLast)
    when (isProbeEnd) { cohInflight := false.B }
  }
  when (cohIn.req.fire()) { cohInflight := true.B }

  cohIn.req.bits.apply(addr = rand.cohAddr * 8.U + memBase.U, size = "b11".U,
    wdata = 0.U, wmask = 0.U, cmd = SimpleBusCmd.probe)
  cohIn.req.valid := (state === s_test) && rand.cohChoose === 0.U && !cohInflight
  cohIn.resp.ready := rand.cohReadyChoose =/= 0.U

  when (Counter((state === s_test) && in.resp.fire(), printCnt)._2) { printf(".") }
  when (Counter((state === s_test) && cohIn.req.fire(), printCnt)._2) { printf("@") }

  Debug(false) {
    when (in.req.fire()) { printf(p"${GTimer()},[in.req] ${in.req.bits}\n") }
  }

  def checkData(addr: UInt, data: UInt): Bool = data === Fill(2, addr)
  def checkDataMask(addr: UInt, data: UInt): Bool = {
    val mask = "hffffffc0".U
    (data & Fill(2, mask)) === Fill(2, addr & mask)
  }

  assert(!(cacheOut.mem.req.valid && !addrInRange(cacheOut.mem.req.bits.addr)),
    "bad addr = %x", cacheOut.mem.req.bits.addr)

  // check rdata from cache
  when (in.resp.valid && !in.resp.bits.user.get(0)) {
    val addr = in.resp.bits.user.get(32,1)
    assert(checkData(addr, in.resp.bits.rdata), "%d: bad rdata = %x at addr = %x",
      GTimer(), in.resp.bits.rdata, addr)
  }

  // check wdata to mem
  when (cacheOut.mem.req.valid && cacheOut.mem.req.bits.isWrite()) {
    val addr = cacheOut.mem.req.bits.addr(31,0)
    assert(checkDataMask(addr, cacheOut.mem.req.bits.wdata), "%d: bad wdata = %x at addr = %x",
      GTimer(), cacheOut.mem.req.bits.wdata, addr)
  }

  // check rdata from cohIn
  val cohAddr = RegEnable(cohIn.req.bits.addr(31,0), cohIn.req.fire())
  when (cohIn.resp.valid) {
    val resp = cohIn.resp.bits
    val isRead = resp.cmd === SimpleBusCmd.readLast || resp.cmd === SimpleBusCmd.read
    val isProbeResp = resp.isProbeHit() || resp.isProbeMiss()
    assert(isRead || isProbeResp)
    assert(!(isRead && !checkDataMask(cohAddr, cohIn.resp.bits.rdata)), "%d: bad rdata = %x at addr = %x",
      GTimer(), cohIn.resp.bits.rdata, cohAddr)
  }

  // only use to keep consistent with NOOPSimTop
  io.difftest := DontCare
  dontTouch(io.difftest)
}
*/
