package device.lvna

import chisel3._
import xiangshan._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.util.{AsyncQueue}
import freechips.rocketchip.devices.debug.DMI_RegAddrs._
import freechips.rocketchip.devices.debug.RWNotify
import freechips.rocketchip.regmapper.{RegField, RegReadFn, RegWriteFn}
import chisel3.util._
import utils._

object log2Safe {
  def apply(n: BigInt): Int = {
    require(!(n < 0), s"Negative input $n for log2")
    if (n == 0 || n == 1) 1
    else log2Ceil(n)
  }
  def apply(n: Int): Int = apply(BigInt(n))
}

case object ProcDSidWidth extends Field[Int](3)
case object NumCores extends Field[Int](2)
case object NL2CacheCapacity extends Field[Int](1024)
case object NL2CacheWays extends Field[Int](8)


trait HasControlPlaneParameters {
  implicit val p: Parameters
  // val nTiles = p(NumCores)
  val nTiles = 2
  // val ldomDSidWidth = log2Up(nTiles)
  val ldomDSidWidth = 1   // 3 ?
  val procDSidWidth = 0   // p(ProcDSidWidth) ?
  val dsidWidth = ldomDSidWidth + procDSidWidth
  val nDSID = 1 << dsidWidth
  val cycle_counter_width = 64
  val cacheCapacityWidth = log2Safe(p(NL2CacheCapacity) * 1024 / 64) + 1

  /* Access current dsid */
  def CP_HART_DSID = 0x41

  /* Access dsid selector */
  def CP_HART_SEL = 0x42

  /* Read total dsid count */
  def CP_DSID_COUNT = 0x43

  /* Access mem base with current dsid */
  def CP_MEM_BASE_LO = 0x44

  def CP_MEM_BASE_HI = 0x45

  /* Access mem mask with current dsid */
  def CP_MEM_MASK_LO = 0x46

  def CP_MEM_MASK_HI = 0x47

  def CP_BUCKET_FREQ = 0x48

  def CP_BUCKET_SIZE = 0x49

  def CP_BUCKET_INC = 0x4a

  def CP_TRAFFIC = 0x4b

  def CP_WAYMASK = 0x4c

  def CP_L2_CAPACITY = 0x4d

  def CP_DSID_SEL = 0x4e

  def CP_LIMIT_INDEX = 0x4f

  def CP_LIMIT = 0x50

  def CP_LOW_THRESHOLD = 0x51

  def CP_HIGH_THRESHOLD = 0x52

  def CP_MAX_QUOTA = 0x53

  def CP_MIN_QUOTA = 0x54

  def CP_QUOTA_STEP = 0x55

  def CP_TIMER_LO = 0x56

  def CP_TIMER_HI = 0x57

  def CORE_PC_HI    = 0x70

  def CORE_PC_LO    = 0x71

  def CORE_PC_SNAP  = 0x72

  def CORE_PC_READ_DONE  = 0x73

  def CORE_PC_READ  = 0x74

  def CORE_INT_DEBUG = 0x75

  def CORE_N_INT_DEBUG = 0x76

  def CORE_N_INT_DEBUG_LOCAL = 0x77

  def CORE_CSR_INT_VALID = 0x78

  def CORE_CSR_PENDING_INT_LO = 0x79

  def CORE_CSR_PENDING_INT_HI = 0x7a


  def CP_HART_ID = 0x7b

  /* L2 Miss */
  def CP_L2_REQ_MISS = 0x7c

  def CP_L2_REQ_TOTAL = 0x7d

  def CP_L2_STAT_RESET = 0x7e

  def CP_L2_REQ_EN = 0x7f

  /* AutoCat */
  def CP_AUTOCAT_EN = 0x58

  def CP_AUTOCAT_RESET_BIN_POWER = 0x59

  def CP_AUTOCAT_SUGGEST_WAYMASK = 0x5a

  def CP_AUTOCAT_WATCHING_DSID = 0x5b

  def CP_AUTOCAT_SET = 0x5c

  def CP_AUTOCAT_GAP = 0x5d
}

/**
 * From ControlPlane's side of view.
 */
class ControlPlaneIO(implicit val p: Parameters) extends Bundle with HasControlPlaneParameters with HasAutoCatParameters {
  private val indexWidth = 32

  val updateData   = Input(UInt(32.W))
  val traffic      = Output(UInt(32.W))
  val cycle        = Output(UInt(cycle_counter_width.W))
  val capacity     = Output(UInt(cacheCapacityWidth.W))
  val hartDsid     = Output(UInt(ldomDSidWidth.W))
  val hartDsidWen  = Input(Bool())
  val memBase      = Output(UInt(64.W))
  val memBaseLoWen = Input(Bool())
  val memBaseHiWen = Input(Bool())
  val memMask      = Output(UInt(64.W))
  val memMaskLoWen = Input(Bool())
  val memMaskHiWen = Input(Bool())
  val bucket       = Output(new BucketBundle())
  val bktFreqWen   = Input(Bool())
  val bktSizeWen   = Input(Bool())
  val bktIncWen    = Input(Bool())
  val waymask      = Output(UInt(p(NL2CacheWays).W))
  val waymaskWen   = Input(Bool())
  val hartSel      = Output(UInt(indexWidth.W))
  val hartSelWen   = Input(Bool())
  val dsidSel      = Output(UInt(dsidWidth.W))
  val dsidSelWen   = Input(Bool())
  val progHartId   = Output(UInt(log2Safe(nTiles).W))
  val progHartIdWen = Input(Bool())

  val limitIndex        = Output(UInt(4.W))
  val limitIndexWen     = Input(Bool())
  val limit             = Output(UInt(16.W))
  val limitWen          = Input(Bool())
  val lowThreshold      = Output(UInt(8.W))
  val lowThresholdWen   = Input(Bool())
  val highThreshold     = Output(UInt(8.W))
  val highThresholdWen  = Input(Bool())

  val maxQuota      = Output(UInt(8.W))
  val maxQuotaWen   = Input(Bool())
  val minQuota      = Output(UInt(8.W))
  val minQuotaWen   = Input(Bool())
  val quotaStep     = Output(UInt(8.W))
  val quotaStepWen  = Input(Bool())

  val readPC              = Input(Bool())
  val doneReadPC          = Input(Bool())
  val autoPCSnapShotWen   = Input(Bool())
  val autoPCSnapShotEn    = Output(Bool())
  val PC                  = Output(UInt(64.W))

  val assertDebugInt      = Input(Bool())

  /*val l2_miss_en        = Input(Bool())
  val l2_req_miss       = Output(UInt(32.W))
  val l2_req_total      = Output(UInt(32.W))
  val l2_stat_reset_wen = Input(Bool())

  val autocat_en        = Output(Bool())
  val autocat_wen       = Input(Bool())
  val autocat_reset_bin_power = Output(UInt(resetBinPowerWidth.W))
  val autocat_reset_bin_power_wen = Input(Bool())
  val autocat_suggested_waymask = Output(UInt(nrL2Ways.W))
  val autocat_watching_dsid = Output(UInt(dsidWidth.W))
  val autocat_watching_dsid_wen = Input(Bool())
  val autocat_set       = Output(UInt(32.W))
  val autocat_set_wen   = Input(Bool())
  val autocat_gap = Output(UInt(32.W))
  val autocat_gap_wen = Input(Bool())*/
}

/* From ControlPlane's View */
class CPToHuanCunIO(implicit val p: Parameters) extends Bundle with HasControlPlaneParameters {
  val waymask = Output(UInt(p(NL2CacheWays).W))  // waymask returned to L2cache (1 cycle delayed)
  val dsid = Input(UInt(dsidWidth.W))  // DSID from requests L2 cache received
  val capacity = Input(UInt(cacheCapacityWidth.W))  // Count on way numbers
  val capacity_dsid = Output(UInt(dsidWidth.W))  // Capacity query dsid
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
  val hartDsid = UInt(ldomDSidWidth.W)
  val memBase = UInt(64.W)
  val memMask = UInt(64.W)
  val progHartId = UInt(log2Safe(nTiles).W)
}

class ControlPlane(tlBeatBytes: Int)(implicit p: Parameters) extends LazyModule
with HasControlPlaneParameters
with HasTokenBucketParameters
with HasAutoCatParameters
{
  private val memAddrWidth = 64
  private val instAddrWidth = 64
  // private val totalBW = if (p(UseEmu)) 55*8 else 20*8
  private val totalBW = 20*8


  val tlNode = TLRegisterNode(
    address = Seq(AddressSet(0x39010000, 0xffff)),
    device = new SimpleDevice("control-plane", Seq("LvNA,test", "LvNA,test")),
    beatBytes = tlBeatBytes
  )


  override lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val hartDsids = Vec(nTiles, Output(UInt(ldomDSidWidth.W)))
      val memBases  = Vec(nTiles, Output(UInt(memAddrWidth.W)))   //cls: it should be used as memOffset, when soft_control off, memOffset(i)=0
      val memMasks  = Vec(nTiles, Output(UInt(memAddrWidth.W)))
      val pc        = Vec(nTiles, Input(UInt(instAddrWidth.W)))
      val huancun   = new CPToHuanCunIO()
      val cp        = new ControlPlaneIO()
      val mem_part_en = Input(Bool())
      val distinct_hart_dsid_en = Input(Bool())
      val progHartIds = Vec(nTiles, Output(UInt(log2Safe(nTiles).W)))
      //val autocat_watching_change = Output(Bool())
    })

    val hartSel   = RegInit(0.U(ldomDSidWidth.W))
    /*val hartDsids = RegInit(VecInit(Seq.tabulate(nTiles){ i =>
      Mux(io.distinct_hart_dsid_en, i.U(ldomDSidWidth.W), 0.U(ldomDSidWidth.W))
    }))
    val memBases  = RegInit(VecInit(Seq.tabulate(nTiles){ i =>
      val memSize: BigInt = p(ExtMem).map { m => m.master.size }.getOrElse(0x80000000L)
      Mux(io.mem_part_en, (i * memSize / nTiles).U(memAddrWidth.W), 0.U(memAddrWidth.W))
    }))
    val memMasks  = RegInit(VecInit(Seq.fill(nTiles)(~0.U(memAddrWidth.W))))
    val waymasks  = RegInit(VecInit(Seq.fill(1 << dsidWidth){ ((1L << p(NL2CacheWays)) - 1).U }))*/
    val hartDsids = RegInit(VecInit(Seq.tabulate(nTiles){ i => 0.U(ldomDSidWidth.W)}))
    val memBases  = RegInit(VecInit(Seq.tabulate(nTiles){ i =>
      val memSize: BigInt = p(ExtMem).map { m => m.master.size }.getOrElse(0x80000000L)
      0.U(memAddrWidth.W)
    }))
    val memMasks  = RegInit(VecInit(Seq.fill(nTiles)(0.U(memAddrWidth.W))))
    val waymasks  = RegInit(VecInit(Seq.fill(1 << dsidWidth){ ((1L << p(NL2CacheWays)) - 1).U }))

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

    val timer = RegInit(0.U(cycle_counter_width.W))
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
      * Programmable hartid.
      */
    val progHartIds = RegInit(VecInit(Seq.fill(nTiles){ 0.U(log2Safe(nTiles).W) }))
    io.progHartIds := progHartIds
    val l2dsid_reg = RegNext(io.huancun.dsid)  // 1 cycle delay
    io.huancun.waymask := waymasks(l2dsid_reg)

    val currDsid = RegEnable(io.cp.updateData, 0.U, io.cp.dsidSelWen)
    io.cp.dsidSel := currDsid
    io.cp.waymask := waymasks(currDsid)
    io.cp.traffic := bucketState(currDsid).traffic
    io.cp.cycle := timer
    io.cp.capacity := io.huancun.capacity
    io.huancun.capacity_dsid := currDsid

    io.cp.hartDsid := hartDsids(hartSel)
    io.cp.hartSel := hartSel
    io.cp.memBase := memBases(hartSel)
    io.cp.memMask := memMasks(hartSel)
    io.cp.bucket := bucketParams(currDsid)
    io.hartDsids := hartDsids
    io.memBases := memBases
    io.memMasks := memMasks

    when (io.cp.hartSelWen) {
      hartSel := io.cp.updateData
    }

    when (io.cp.hartDsidWen) {
      hartDsids(hartSel) := io.cp.updateData
    }

    val mem_base_lo_tmp = RegInit(0.U(32.W))
    //val mem_mask_lo_tmp = RegInit((~0.U(32.W)).asUInt)
    val mem_mask_lo_tmp = RegInit(0.U(32.W))

    when (io.cp.memBaseLoWen) {
      mem_base_lo_tmp := io.cp.updateData
    }

    when (io.cp.memBaseHiWen) {
      memBases(hartSel) := Cat(io.cp.updateData, mem_base_lo_tmp)
    }

    when (io.cp.memMaskLoWen) {
      mem_mask_lo_tmp := io.cp.updateData
    }

    when (io.cp.memMaskHiWen) {
      memMasks(hartSel) := Cat(io.cp.updateData, mem_mask_lo_tmp)
    }

    when (io.cp.progHartIdWen) {
      progHartIds(hartSel) := io.cp.updateData
    }

    when (io.cp.bktFreqWen) {
      bucketParams(currDsid).freq := io.cp.updateData
    }

    when (io.cp.bktSizeWen) {
      bucketParams(currDsid).size := io.cp.updateData
    }

    when (io.cp.bktIncWen) {
      bucketParams(currDsid).inc := io.cp.updateData
    }

    when (io.cp.waymaskWen) {
      waymasks(currDsid) := io.cp.updateData
    }

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
    def offset(addr: Int): Int = { (addr - CP_HART_DSID) << 2 }

    val traffic_read = WireInit(false.B)
    val timestamp_buffered = Reg(UInt(cycle_counter_width.W))
    when (traffic_read) {
      timestamp_buffered := timer
    }

    tlNode.regmap(
      offset(CP_HART_DSID)   -> Seq(RegField(32, hartDsids(hartSel))),
      offset(CP_HART_SEL)    -> Seq(RegField(32, hartSel)),
      offset(CP_DSID_COUNT)  -> Seq(RegField(32, (1 << dsidWidth).U, ())),
      offset(CP_MEM_BASE_LO) -> Seq(RegField(32, memBases(hartSel)(31, 0), mem_base_lo_tmp)),
      offset(CP_MEM_BASE_HI) -> Seq(RegField(32, memBases(hartSel)(memAddrWidth - 1, 32), (valid: Bool, data: UInt) => {
        when (valid) {
          memBases(hartSel) := Cat(data, mem_base_lo_tmp)
        }
        true.B
      })),
      offset(CP_MEM_MASK_LO) -> Seq(RegField(32, memMasks(hartSel)(31, 0), mem_mask_lo_tmp)),
      offset(CP_MEM_MASK_HI) -> Seq(RegField(32, memMasks(hartSel)(memAddrWidth - 1, 32), (valid: Bool, data: UInt) => {
          when (valid) {
            memMasks(hartSel) := Cat(data, mem_mask_lo_tmp)
          }
          true.B
        }
      )),
      offset(CP_BUCKET_FREQ) -> Seq(RegField(32, bucketParams(currDsid).freq)),
      offset(CP_BUCKET_SIZE) -> Seq(RegField(32, bucketParams(currDsid).size)),
      offset(CP_BUCKET_INC)  -> Seq(RegField(32, bucketParams(currDsid).inc)),
      offset(CP_TRAFFIC)     -> Seq(RWNotify(32, bucketState(currDsid).traffic, bucketState(currDsid).traffic, traffic_read, Wire(Bool()))),
      offset(CP_WAYMASK)     -> Seq(RegField(32, waymasks(currDsid))),
      offset(CP_L2_CAPACITY) -> Seq(RegField(32, io.huancun.capacity, ())),
      offset(CP_DSID_SEL)    -> Seq(RegField(32, currDsid)),
      offset(CP_HART_ID)     -> Seq(RegField(32, progHartIds(hartSel))),
      offset(CP_TIMER_LO)    -> Seq(RegField(32, timestamp_buffered(31, 0), ())),
      offset(CP_TIMER_HI)    -> Seq(RegField(32, timestamp_buffered(63, 32), ())),
      /*offset(CP_L2_REQ_EN)   -> Seq(RWNotify(1, WireInit(false.B), WireInit(false.B), sbus_l2_miss_en, Wire(Bool()))),
      offset(CP_L2_REQ_MISS) -> Seq(RegField.r(32, io.huancun.req_miss)),
      offset(CP_L2_REQ_TOTAL)-> Seq(RegField.r(32, io.huancun.req_total)),
      offset(CP_L2_STAT_RESET)->Seq(RWNotify(1, WireInit(false.B), l2_stat_reset, Wire(Bool()), WireInit(false.B))),
      offset(CP_AUTOCAT_EN)             -> Seq(RegField(1, autocat_en_reg)),
      offset(CP_AUTOCAT_RESET_BIN_POWER)-> Seq(RegField(resetBinPowerWidth, autocat_reset_bin_power_reg)),
      offset(CP_AUTOCAT_SUGGEST_WAYMASK)-> Seq(RegField.r(nrL2Ways, io.huancun.autocat_suggested_waymask)),
      offset(CP_AUTOCAT_WATCHING_DSID)  -> Seq(RWNotify(dsidWidth, autocat_watching_dsid, autocat_watching_dsid, WireInit(false.B), watch_change)),
      offset(CP_AUTOCAT_SET)            -> Seq(RegField(32, autocat_set_sampling_mask)),
      offset(CP_AUTOCAT_GAP)            -> Seq(RegField(32, autocat_gap)),*/
    )



    if (false) {
    // AutoMBA goes here

    val accountingCycle = 10000
    val cycleCounter = RegInit(0.asUInt(64.W))
    val lastTraffic = RegInit(VecInit(Seq.fill(nTiles){ 0.U(32.W) }))

    when (GTimer() >= cycleCounter) {
      for (i <- 0 until nTiles) {
        // 输出每段时间内，产生了多少流量，用于精细观察流量控制策略
        // 输出到了每个时间点后的CDF，用于看整体的流量曲线，观察流量控制效果
        lastTraffic(i) := bucketState(i).traffic
      }
      cycleCounter := cycleCounter + accountingCycle.U
    }

    val policy = "yzh"

    // val highPriorIndex = if (p(UseEmu)) 0 else nTiles
    // val startIndex = if (p(UseEmu)) 1 else 0
    // val limitScaleIndex= if (p(UseEmu)) 1 else 3
    val highPriorIndex = nTiles
    val startIndex = 0
    val limitScaleIndex= 3

    val quota = RegInit((totalBW/10*5).U(8.W))
    val curLevel = RegInit(4.asUInt(4.W))

    val maxQuota = RegEnable(io.cp.updateData, (totalBW/10*9).U(11.W), io.cp.maxQuotaWen)
    val minQuota = RegEnable(io.cp.updateData, (totalBW/10).U(11.W), io.cp.minQuotaWen)
    val quotaStep = RegEnable(io.cp.updateData, (totalBW/10).U(11.W), io.cp.quotaStepWen)

    io.cp.maxQuota := maxQuota
    io.cp.minQuota := minQuota
    io.cp.quotaStep := quotaStep

    val nLevels = 9
    val lowPriorFreqLimits = RegInit(VecInit(Seq.fill(nLevels){ 8.U(16.W) })) // bigger is more strict
    val limitIndex = RegEnable(io.cp.updateData, 0.U(4.W), io.cp.limitIndexWen)
    val lowerThreshold = RegEnable(io.cp.updateData, 64.U(8.W), io.cp.lowThresholdWen)
    val higherThreshold = RegEnable(io.cp.updateData, 112.U(8.W), io.cp.highThresholdWen)

    io.cp.lowThreshold := lowerThreshold
    io.cp.highThreshold := higherThreshold
    io.cp.limitIndex := limitIndex
    io.cp.limit := lowPriorFreqLimits(limitIndex)

    when (io.cp.limitWen) {
      lowPriorFreqLimits(limitIndex) := io.cp.updateData
    }

    if (policy == "solo") {
      ///////////////////////////////////////////
      // c0 Solo:
      for (i <- startIndex until nTiles) {
        bucketParams(i).inc := 0.U
      }
      bucketParams(highPriorIndex).freq := 0.U

    } else if (policy == "yzh") {
      //////////////////////////////////////////////
      // Yu Zihao:
      val startTraffic = RegInit(0.asUInt(32.W))
      val windowCounter = RegInit(0.asUInt(16.W))

      val windowSize = 1000


      // def freqTable() = {
      //   val allocatedBW = (1 until 10).map{totalBW.toDouble/10*_}
      //   val freqs = allocatedBW.map(bw => round(windowSize.toDouble/bw))
      //   println(freqs)
      //   VecInit(freqs.map(_.asUInt(10.W)))
      // }
      // def limitFreqTable() = {
      //   // 这里的3是假设4个核心，剩下3个核心共享余下带宽
      //   val allocatedBW = (1 until 10).map{totalBW - totalBW.toDouble/10*_}
      //   val freqs = allocatedBW.map(bw => round(windowSize.toDouble/(bw/limitScaleIndex)))
      //   println(freqs)
      //   VecInit(freqs.map(_.asUInt(10.W)))
      // }

      when (windowCounter >= windowSize.U) {
        windowCounter := 1.U

        val bandwidthUsage = ((bucketState(highPriorIndex).traffic - startTraffic) << 3).asUInt()
        startTraffic := bucketState(highPriorIndex).traffic

        val nextLevel = Wire(UInt(4.W))
        val nextQuota = Wire(UInt(8.W))

        nextLevel := curLevel
        nextQuota := quota

        when (bandwidthUsage >= ((quota*higherThreshold) >> 7).asUInt ) {
          nextLevel := Mux(curLevel === 8.U, 8.U, curLevel + 1.U)
          nextQuota := Mux(quota >= maxQuota, maxQuota, quota + quotaStep)

        } .elsewhen (bandwidthUsage < ((quota*lowerThreshold) >> 7).asUInt ) {
          nextLevel := Mux(curLevel === 0.U, 0.U, curLevel - 1.U)
          nextQuota := Mux(quota <= minQuota, minQuota, quota - quotaStep)
        }

        bucketParams(highPriorIndex).freq := 0.U
      // bucketParams(highPriorIndex).inc := 1.U
        for (i <- startIndex until nTiles) {
          bucketParams(i).freq := lowPriorFreqLimits(nextLevel)
          bucketParams(i).inc := 1.U
        }
        curLevel := nextLevel
        quota := nextQuota

      }
      when (windowCounter < windowSize.U) {
        windowCounter := windowCounter + 1.U
      }

    } else if (policy == "lzg") {
      ///////////////////////////////////////////////
      // Liu Zhigang:
      val regulationCycles = 16000
      val samplingCycles = 1000
      val startTraffic = RegInit(0.asUInt(32.W))

      val samplingCounter = RegInit(0.asUInt(32.W))
      val regulationCounter = RegInit(0.asUInt(32.W))

      val s_sample :: s_regulate :: Nil = Enum(2)
      val state = RegInit(s_sample)

      // during s_sample state, high priority app runs alone
      // all others' memory requests are blocked
      when (state === s_sample) {
        samplingCounter := samplingCounter + 1.U
        for (i <- startIndex until nTiles) {
          bucketParams(i).freq := 3200.U
          bucketParams(i).inc := 1.U
        }
        when (samplingCounter >= samplingCycles.U) {
          // estimate high priority app's memory bandwidth demand
          // set low priority app's bucket accordingly
          val bandwidthUsage = bucketState(highPriorIndex).traffic - startTraffic
          startTraffic := bucketState(highPriorIndex).traffic
          // val estimatedBandwidth = startTraffic << 4
          // 经验数据，仿真时，一万个周期，两个核最多往下面推了1000个beat
          // val totalBandwidth = regulationCycles / 10
          // 假设统计出来1000个周期内高优先级用了x beat，则在接下来的16000个周期里
          // 低优先级最多可以传输的beat数为 1600 - 16x
          // 相应地，其freq应该为 16000 / (1600 - 16x)
          // 由于这个无法简单表示，所以，我们手动将其分段表示
          // assume NTiles = 4
          // val newFreq = (regulationCycles.U - estimatedBandwidth) >> 4

          // 不那么激进的参数，这个是按照公式算的freq
          // val newFreq = Mux(bandwidthUsage >= 90.U, 100.U,
          //   Mux(bandwidthUsage >= 80.U, 50.U,
          //     Mux(bandwidthUsage >= 70.U, 33.U,
          //       Mux(bandwidthUsage >= 60.U, 25.U,
          //         Mux(bandwidthUsage >= 50.U, 20.U, 10.U)))))

          // 激进的参数，把上面根据freq算的数据，手动扩大几倍，把低优先级限制得更加死
          val newFreq = Mux(bandwidthUsage >= 90.U, 400.U,
            Mux(bandwidthUsage >= 80.U, 300.U,
              Mux(bandwidthUsage >= 70.U, 200.U,
                Mux(bandwidthUsage >= 60.U, 100.U,
                  Mux(bandwidthUsage >= 50.U, 100.U, 10.U)))))


          for (i <- startIndex until nTiles) {
            bucketParams(i).freq := newFreq
            bucketParams(i).inc := 1.U
          }

          regulationCounter := 0.U
          state := s_regulate
        }
      }

      when (state === s_regulate) {
        regulationCounter := regulationCounter + 1.U
        when (regulationCounter >= regulationCycles.U) {
          // temporarily disable all others' memory requests
          // let high priority app runs solo
          samplingCounter := 0.U
          state := s_sample
        }
      }
    }}


    //Boom debug

    val snapshotPC = RegInit(0.asUInt(instAddrWidth.W))
    val hasNewPCSnapshot = RegInit(false.B)
    val autoSnapshot = RegEnable(io.cp.updateData =/= 0.U, true.B, io.cp.autoPCSnapShotWen)

    when ((autoSnapshot || io.cp.readPC) && !hasNewPCSnapshot) {
      snapshotPC := io.pc(hartSel)
      hasNewPCSnapshot := true.B
    }

    when (io.cp.doneReadPC) {
      hasNewPCSnapshot := false.B
    }
    io.cp.autoPCSnapShotEn := autoSnapshot
    io.cp.PC := snapshotPC
  }
}