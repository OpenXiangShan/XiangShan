package device.lvna

import chisel3.{Bundle, _}
import xiangshan._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.util.AsyncQueue
import freechips.rocketchip.devices.debug.DMI_RegAddrs._
import freechips.rocketchip.devices.debug.RWNotify
import freechips.rocketchip.regmapper.{RegField, RegReadFn, RegWriteFn}
import chisel3.util._
import utils._
import system.SoCParamsKey

import scala.tools.nsc.doc.model.Val

object log2Safe {
  def apply(n: BigInt): Int = {
    require(!(n < 0), s"Negative input $n for log2")
    if (n == 0 || n == 1) 1
    else log2Ceil(n)
  }
  def apply(n: Int): Int = apply(BigInt(n))
}

// case object ProcDSidWidth extends Field[Int](3)
// case object NL2CacheCapacity extends Field[Int](1024)
// case object NL3CacheWays extends Field[Int](8)


trait HasControlPlaneParameters{
  implicit val p: Parameters
  val tiles = p(XSTileKey)
  val nTiles = tiles.size
  val ldomDSidWidth = log2Up(nTiles)
  val procDSidWidth = 2   // p(ProcDSidWidth) ?
  val dsidWidth = p(SoCParamsKey).dsidWidth
  val nDSID = 1 << dsidWidth
//  val cycle_counter_width = 64
  // val cacheCapacityWidth = log2Safe(p(NL2CacheCapacity) * 1024 / 64) + 1

  val llcWays = p(SoCParamsKey).L3CacheParamsOpt.get.ways

  def CP_IDSEL = 0x0
  def CP_HARTSEL = 0x8
  def CP_MEM_OFFSET = 0x10
  def CP_IO_OFFSET = 0x18
  def CP_WAYMASK = 0x20
  def CP_NOHYPE_BARRIER = 0x28
  def CP_HARTNUM = 0x30

}

abstract class CPBundle(implicit val p: Parameters) extends Bundle with HasControlPlaneParameters

/**
 * From ControlPlane's side of view.
 */
class ControlPlaneIO(implicit p: Parameters) extends CPBundle with HasAutoCatParameters {
  private val indexWidth = 64

  val updateData   = Input(UInt(32.W))
  val traffic      = Output(UInt(32.W))
  val hartSel      = Output(UInt(ldomDSidWidth.W))
  val hartDsidWen  = Input(Bool())
  val memBase      = Output(UInt(64.W))
  val memBaseLoWen = Input(Bool())
  val memBaseHiWen = Input(Bool())
  val bucket       = Output(new BucketBundle())
  val bktFreqWen   = Input(Bool())
  val bktSizeWen   = Input(Bool())
  val bktIncWen    = Input(Bool())
  val waymask      = Output(UInt(llcWays.W))
  val waymaskWen   = Input(Bool())
  // val dsidSel      = Output(UInt(dsidWidth.W))
  // val dsidSelWen   = Input(Bool())
  val progHartId   = Output(UInt(log2Safe(nTiles).W))
  val progHartIdWen = Input(Bool())
}

class CPToHuancunSetWaymask(implicit p: Parameters) extends CPBundle
{
  val dsid = UInt(dsidWidth.W)
  val waymask = UInt(llcWays.W)
}

/* From ControlPlane's View */
class CPToHuanCunIO(implicit p: Parameters) extends CPBundle {

  val waymaskSetReq = DecoupledIO(new CPToHuancunSetWaymask)
  // val capacity = Input(UInt(cacheCapacityWidth.W))  // Count on way numbers
  // val capacity_dsid = Output(UInt(dsidWidth.W))  // Capacity query dsid
  /*val req_miss = Input(UInt(32.W))
  val req_miss_en = Output(Bool())
  val req_total = Input(UInt(32.W))
  val stat_reset = Output(Bool())

  val autocat_watching_dsid = Output(UInt(dsidWidth.W))
  val autocat_suggested_waymask = Input(UInt(p(NL2CacheWays).W))
  val autocat_set = Output(UInt(32.W))
  val autocat_en = Output(Bool())*/
}

class BucketState(implicit val p: Parameters) extends Bundle with HasControlPlaneParameters with HasTokenBucketParameters {
  val nToken = SInt(tokenBucketSizeWidth.W)
  val traffic = UInt(tokenBucketSizeWidth.W)
  val counter = UInt(32.W)
  val enable = Bool()
}

class BucketIO(implicit val p: Parameters) extends Bundle with HasControlPlaneParameters with HasTokenBucketParameters {
  val dsid = Input(UInt(dsidWidth.W))
  val size = Input(UInt(tokenBucketSizeWidth.W))
  val fire = Input(Bool())
  val enable = Output(Bool())
}

class CPToCore(implicit val p: Parameters) extends Bundle with HasControlPlaneParameters
{
  val memOffsets  = Vec(nTiles, ValidIO(UInt(64.W)))
  val ioOffsets  = Vec(nTiles, ValidIO(UInt(64.W)))
}

class ControlPlane(tlBeatBytes: Int)(implicit p: Parameters) extends LazyModule
with HasControlPlaneParameters
with HasTokenBucketParameters
// with HasAutoCatParameters
{

  val tlNode = TLRegisterNode(
    address = Seq(AddressSet(0x20000, 0xffff)),
    device = new SimpleDevice("control-plane", Seq("LvNA,test", "LvNA,test")),
    beatBytes = tlBeatBytes,
    undefZero = true,
    concurrency = 1
  )


  override lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val core      = new CPToCore()
      val huancun   = new CPToHuanCunIO()
      val cp        = new ControlPlaneIO()
      //val autocat_watching_change = Output(Bool())
    })

    val dsidSel   = RegInit(0.U(dsidWidth.W))
    val hartSel   = RegInit(0.U(ldomDSidWidth.W))
    val nohypeBarrier = RegInit(0.U(1.W))
    val nTilesReg = RegInit(nTiles.U(32.W))
    val memOffsets  = RegInit(VecInit(Seq.fill(nTiles){ 0.U(64.W) }))
    val memOffNewdata = WireInit(VecInit(Seq.fill(nTiles){ false.B }))
    val memOffHasdata = RegInit(VecInit(Seq.fill(nTiles){ false.B }))
    val ioOffsets  = RegInit(VecInit(Seq.fill(nTiles){ 0.U(64.W) }))
    val ioOffNewdata = WireInit(VecInit(Seq.fill(nTiles){ false.B }))
    val ioOffHasdata = RegInit(VecInit(Seq.fill(nTiles){ false.B }))

    (io.core.memOffsets zip memOffsets) foreach {
      case (io_moff, moff) =>
        io_moff.bits := moff
    }
    (io.core.memOffsets zip (memOffHasdata zip memOffNewdata)) foreach {
      case (io_moff, (moffHasdata, moffNewdata)) =>
        io_moff.valid := moffHasdata
        when (io_moff.fire()) {
          moffHasdata := false.B
        }.otherwise {
          moffHasdata := moffNewdata
        }
    }
    (io.core.ioOffsets zip ioOffsets) foreach {
      case (io_iooff,iooff) =>
        io_iooff.bits := iooff
    }
    (io.core.ioOffsets zip (ioOffHasdata zip ioOffNewdata)) foreach {
      case (io_iooff, (iooffHasdata, iooffNewdata)) =>
        io_iooff.valid := iooffHasdata
        when (io_iooff.fire()) {
          iooffHasdata := false.B
        }.otherwise {
          iooffHasdata := iooffNewdata
        }
    }

    val waymasks  = RegInit(VecInit(Seq.fill(1 << dsidWidth){ ((1L << llcWays) - 1).U }))

    //token buckets
    private val bucket_debug = false

    /*val bucketParams = RegInit(VecInit(Seq.fill(nDSID){
      Cat(128.U(tokenBucketSizeWidth.W), 128.U(tokenBucketFreqWidth.W), 128.U(tokenBucketSizeWidth.W)).asTypeOf(new BucketBundle)
    }))

    val bucketState = RegInit(VecInit(Seq.fill(nDSID){
      Cat(0.U(tokenBucketSizeWidth.W), 0.U(tokenBucketSizeWidth.W), 0.U(32.W), true.B).asTypeOf(new BucketState)
    }))*/
    val bucketParams = RegInit(VecInit(Seq.fill(nDSID){0.U.asTypeOf(new BucketBundle)}))

    val bucketState = RegInit(VecInit(Seq.fill(nDSID){0.U.asTypeOf(new BucketState)}))

    val bucketIO = IO(Vec(nTiles, new BucketIO()))

    val timer = RegInit(0.U(64.W))
    timer := Mux(timer === (~0.U(timer.getWidth.W)).asUInt, 0.U, timer + 1.U)

    bucketState.zipWithIndex.foreach { case (state, i) =>
      state.counter := Mux(state.counter >= bucketParams(i).freq, 0.U, state.counter + 1.U)
      val req_sizes = bucketIO.map{bio => Mux(bio.dsid === i.U && bio.fire, bio.size, 0.U) }
      val req_all = req_sizes.reduce(_ + _)
      val updating = state.counter >= bucketParams(i).freq
      val inc_size = Mux(updating, bucketParams(i).inc.asSInt(), 0.S)
      val enable_next = state.nToken + inc_size > req_all.asSInt
      val calc_next = state.nToken + inc_size - req_all.asSInt
      val limit_next = Mux(calc_next < bucketParams(i).size.asSInt, calc_next, bucketParams(i).size.asSInt)

      val has_requester = bucketIO.map{bio => bio.dsid === i.U && bio.fire}.reduce(_ || _)
      when (has_requester) {
        state.traffic := state.traffic + req_all.asUInt
      }

      when (has_requester || updating) {
  //      state.nToken := Mux(enable_next, limit_next, 0.U)
        state.nToken := limit_next
        state.enable := enable_next || (bucketParams(i).freq === 0.U)
      }

      if (bucket_debug && i <= 1) {
        printf(s"cycle: %d bucket %d req_all %d tokens %d inc %d enable_next %b counter %d traffic %d\n",
          GTimer(), i.U(dsidWidth.W), req_all, state.nToken, inc_size, enable_next, state.counter, state.traffic)
      }
    }

    bucketIO.foreach { bio =>
      bio.enable := bucketState(bio.dsid).enable
    }
    
    
    
    /**
      * waymask set.
      */
    val waymaskSetQueue = Module(new Queue(new CPToHuancunSetWaymask,2))
    val newWaymaskSet = WireInit(false.B)
    waymaskSetQueue.io.enq.valid := RegNext(newWaymaskSet, false.B)
    waymaskSetQueue.io.enq.bits.dsid := dsidSel
    waymaskSetQueue.io.enq.bits.waymask := waymasks(dsidSel)
    io.huancun.waymaskSetReq <> waymaskSetQueue.io.deq

    // Miss
    /*val l2_stat_reset = RegInit(false.B)
    val sbus_l2_miss_en = Wire(Bool())
    val miss_en = sbus_l2_miss_en || io.cp.l2_miss_en

    io.huancun.req_miss_en := miss_en
    io.huancun.stat_reset := l2_stat_reset

    when (io.cp.l2_stat_reset_wen) {
      l2_stat_reset := io.cp.updateData
    }

    io.cp.l2_req_miss := RegEnable(io.huancun.req_miss, RegNext(RegNext(miss_en)))    // Wait miss_stat sram addr changes
    io.cp.l2_req_total := RegEnable(io.huancun.req_total, RegNext(RegNext(miss_en)))  // Wait miss_stat sram addr changes*/

    // AutoCat
    /*def cpRegTmpl(init: UInt, enable: Bool): UInt = RegEnable(io.cp.updateData, init, enable)

    io.cp.autocat_suggested_waymask := io.huancun.autocat_suggested_waymask

    AutoCat Enable
    val autocat_en_reg = cpRegTmpl(false.B, io.cp.autocat_wen)
    io.cp.autocat_en := autocat_en_reg
    io.huancun.autocat_en := autocat_en_reg

    Decide autocat refresh cycles: 2**(value) 
    val autocat_reset_bin_power_reg = cpRegTmpl(10.U(resetBinPowerWidth.W), io.cp.autocat_reset_bin_power_wen)
    io.cp.autocat_reset_bin_power := autocat_reset_bin_power_reg

    The label autocat are serving.
      Updating this field will refresh autocat's way hit counters.
    val autocat_watching_dsid = cpRegTmpl(0.U(dsidWidth.W), io.cp.autocat_watching_dsid_wen)
    io.cp.autocat_watching_dsid := autocat_watching_dsid
    io.huancun.autocat_watching_dsid := autocat_watching_dsid
    val watch_change = WireInit(false.B)
    io.autocat_watching_change := watch_change || io.cp.autocat_watching_dsid_wen

    The allowed hits gap between current allocated ways to full-occupied.
    val autocat_gap = cpRegTmpl(500.U(32.W), io.cp.autocat_gap_wen)
    io.cp.autocat_gap := autocat_gap

    private val l2SetCnt: BigInt = p(NL2CacheCapacity) * 1024 / p(NL2CacheWays) / p(CacheBlockBytes)
    private val l2SetBits = l2SetCnt.bitLength
    println(s"cp: l2SetCnt $l2SetCnt, l2SetBits $l2SetBits")
    val autocat_set_sampling_mask = cpRegTmpl(0xf.U(l2SetBits.W), io.cp.autocat_set_wen)
    io.cp.autocat_set := autocat_set_sampling_mask
    io.huancun.autocat_set := autocat_set_sampling_mask*/

    // TL node

    tlNode.regmap(
      CP_IDSEL -> Seq(RegField(64, dsidSel)),
      CP_HARTSEL -> Seq(RegField(64, hartSel)),
      CP_MEM_OFFSET -> Seq(RWNotify(64, memOffsets(hartSel), memOffsets(hartSel),
        Wire(Bool()), memOffNewdata(hartSel)
      )),
      CP_IO_OFFSET -> Seq(RWNotify(64, ioOffsets(hartSel), ioOffsets(hartSel),
        Wire(Bool()), ioOffNewdata(hartSel)
      )),
      CP_WAYMASK -> Seq(RWNotify(64, waymasks(dsidSel), waymasks(dsidSel),
        Wire(Bool()), newWaymaskSet
      )),
      CP_NOHYPE_BARRIER -> Seq(RegField(64, nohypeBarrier)),
      CP_HARTNUM -> Seq(RegField(64, nTilesReg)),
    )

  }
}