package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import chisel3.experimental.chiselName
import chisel3.ExcitingUtils._

import scala.math.min

trait MicroBTBPatameter{
    val nWays = 16
    val lowerBitsSize = 20
    val tagSize = 20

    val extended_stat = false
}

@chiselName
class MicroBTB extends BasePredictor
    with MicroBTBPatameter
{
    // val tagSize = VAddrBits - log2Ceil(PredictWidth) - 1
    val untaggedBits = log2Up(PredictWidth) + instOffsetBits

    class MicroBTBResp extends Resp
    {
        val targets = Vec(PredictWidth, UInt(VAddrBits.W))
        val hits = Vec(PredictWidth, Bool())
        val takens = Vec(PredictWidth, Bool())
        val brMask = Vec(PredictWidth, Bool())
        val is_RVC = Vec(PredictWidth, Bool())
    }

    class MicroBTBBranchInfo extends Meta
    {
        val writeWay = Vec(PredictWidth,UInt(log2Ceil(nWays).W))
        val hits = Vec(PredictWidth,Bool())
    }
    val out_ubtb_br_info = Wire(new MicroBTBBranchInfo)
    override val metaLen = out_ubtb_br_info.asUInt.getWidth

    class MicroBTBIO extends DefaultBasePredictorIO
    {
        val out = Output(new MicroBTBResp)   //
        val uBTBMeta = Output(new MicroBTBBranchInfo)
    }

    override val debug = true
    override val io = IO(new MicroBTBIO)
    io.uBTBMeta <> out_ubtb_br_info

    def getTag(pc: UInt) = (pc >> untaggedBits)(tagSize-1, 0)
    def getBank(pc: UInt) = pc(log2Ceil(PredictWidth), instOffsetBits)

    class MicroBTBMeta extends XSBundle
    {
        val is_Br = Bool()
        val is_RVC = Bool()
        val valid = Bool()
        val pred = UInt(2.W)
        val tag = UInt(tagSize.W)
    }

    class MicroBTBEntry extends XSBundle
    {
        val lower = UInt(lowerBitsSize.W)
    }

    class MetaOutput extends XSBundle {
        val is_Br = Bool()
        val is_RVC = Bool()
        val pred = UInt(2.W)
    }

    @chiselName
    class UBTBMetaBank(nWays: Int) extends XSModule {
        val io = IO(new Bundle {
            val wen = Input(Bool())
            val wWay = Input(UInt(log2Up(nWays).W))
            val wdata = Input(new MicroBTBMeta)
            val rtag = Input(UInt(tagSize.W))
            val rdata = Output(new MetaOutput)
            val hit_and_taken = Output(Bool())
            val hit_ohs = Output(Vec(nWays, Bool()))
            val hit_way = Output(UInt(log2Up(nWays).W))
            val allocatable_way = Valid(UInt(log2Up(nWays).W))
            val rWay = Input(UInt(log2Up(nWays).W))
            val rpred = Output(UInt(2.W))
        })
        val mem = Mem(nWays, new MicroBTBMeta)
        val rentries = VecInit((0 until nWays) map (i => mem(i)))
        val hit_ohs = VecInit(rentries map (e => e.valid && e.tag === io.rtag))
        io.hit_and_taken := VecInit(rentries map (e => e.valid && e.tag === io.rtag && e.pred(1))).asUInt.orR
        val hit_way = OHToUInt(hit_ohs)
        //val hit_entry = rentries(hit_way)
        val hit_entry = ParallelMux(hit_ohs zip rentries)

        io.hit_ohs := hit_ohs
        io.hit_way := hit_way
        io.rdata.is_Br  := hit_entry.is_Br
        io.rdata.is_RVC := hit_entry.is_RVC
        io.rdata.pred   := hit_entry.pred
        val entry_emptys = VecInit(rentries.map(e => !e.valid))
        val allocatable = ParallelOR(entry_emptys)
        io.allocatable_way.bits := PriorityEncoder(entry_emptys)
        io.allocatable_way.valid := allocatable
        io.rpred := rentries(io.rWay).pred
        when (io.wen) {
            mem.write(io.wWay, io.wdata)
        }
    }

    @chiselName
    class UBTBDataBank(nWays: Int) extends XSModule {
        val io = IO(new Bundle {
            val wen = Input(Bool())
            val wWay = Input(UInt(log2Up(nWays).W))
            val wdata = Input(new MicroBTBEntry)
            val rOHs = Input(Vec(nWays, Bool()))
            val rdata = Output(new MicroBTBEntry)
        })
        val mem = Mem(nWays, new MicroBTBEntry)
        val rentries = VecInit((0 until nWays) map (i => mem(i)))
        // io.rdata := rentries(io.rWay)
        io.rdata := ParallelMux(io.rOHs zip rentries)
        when (io.wen) {
            mem.write(io.wWay, io.wdata)
        }
    }

    val metaBanks = Seq.fill(PredictWidth)(Module(new UBTBMetaBank(nWays)))
    val dataBanks = Seq.fill(PredictWidth)(Module(new UBTBDataBank(nWays)))
    val metas = VecInit(metaBanks.map(_.io))
    val datas = VecInit(dataBanks.map(_.io))

    val uBTBMeta = VecInit(metas.map(m => m.rdata))
    val uBTB     = VecInit(datas.map(d => d.rdata))

    val do_reset = RegInit(true.B)
    val reset_way = RegInit(0.U(log2Ceil(nWays).W))
    when (do_reset) { reset_way := reset_way + 1.U }
    when (reset_way === (nWays-1).U) { do_reset := false.B }

    //uBTB read
    //tag is packet aligned
    val packetAlignedPC = packetAligned(io.pc.bits)

    val read_valid = io.pc.valid
    val read_req_tag = getTag(packetAlignedPC)

    class ReadRespEntry extends XSBundle
    {
        val is_RVC = Bool()
        val target = UInt(VAddrBits.W)
        val valid = Bool()
        val taken = Bool()
        val is_Br = Bool()
    }
    val read_resp = Wire(Vec(PredictWidth,new ReadRespEntry))
    
    (0 until PredictWidth).map{ b => metas(b).rtag := read_req_tag }
    val read_hit_ohs = (0 until PredictWidth).map{ b => metas(b).hit_ohs }
    val read_hit_vec = VecInit(read_hit_ohs.map{oh => ParallelOR(oh).asBool})
    val read_hit_ways = (0 until PredictWidth).map{ b => metas(b).hit_way }

    (0 until PredictWidth).map(b => datas(b).rOHs := read_hit_ohs(b))

    val  uBTBMeta_resp = VecInit((0 until PredictWidth).map(b => metas(b).rdata))
    val  btb_resp = VecInit((0 until PredictWidth).map(b => datas(b).rdata))  

    for(i <- 0 until PredictWidth){
        // do not need to decide whether to produce results\
        read_resp(i).valid := io.inMask(i)
        read_resp(i).taken := read_resp(i).valid && metas(i).hit_and_taken
        read_resp(i).is_Br  := read_resp(i).valid && uBTBMeta_resp(i).is_Br
        read_resp(i).target := Cat(io.pc.bits(VAddrBits-1, lowerBitsSize+instOffsetBits), btb_resp(i).asUInt, 0.U(instOffsetBits.W))
        read_resp(i).is_RVC := read_resp(i).valid && uBTBMeta_resp(i).is_RVC

        out_ubtb_br_info.hits(i) := read_hit_vec(i)
    }

    //TODO: way alloc algorithm
    def alloc_way(valids:UInt ,meta_tags:UInt,req_tag:UInt) = {
        val way = Wire(UInt(log2Up(BtbWays).W))
        val all_valid = valids.andR.asBool
        val tags = Cat(meta_tags,req_tag)
        val l = log2Ceil(nWays)
        val nChunks = (tags.getWidth + l - 1) / l
        val chunks = (0 until nChunks) map { i =>
            tags(min((i+1)*l, tags.getWidth)-1, i*l)
        }
        way := Mux(all_valid,chunks.reduce(_^_),PriorityEncoder(~valids))
        way
    }

    val alloc_ways = (0 until PredictWidth).map{ b => 
        Mux(metas(b).allocatable_way.valid, metas(b).allocatable_way.bits, LFSR64()(log2Ceil(nWays)-1,0))}
    (0 until PredictWidth).map(i => out_ubtb_br_info.writeWay(i) := Mux(read_hit_vec(i).asBool,read_hit_ways(i),alloc_ways(i)))

    //response
    //only when hit and instruction valid and entry valid can output data
    for(i <- 0 until PredictWidth)
    {
        io.out.targets(i) := read_resp(i).target
        io.out.hits(i) := read_resp(i).valid && read_hit_vec(i)
        io.out.takens(i) := read_resp(i).taken
        io.out.is_RVC(i) := read_resp(i).is_RVC
        io.out.brMask(i) := read_resp(i).is_Br
    }

    //uBTB update 
    //backend should send fetch pc to update
    val u = io.update.bits
    val update_br_pc  = u.pc
    val update_br_idx = u.fetchIdx
    val update_br_offset = (update_br_idx << instOffsetBits).asUInt()
    val update_fetch_pc = update_br_pc - update_br_offset
    val update_write_way = u.bpuMeta.ubtbWriteWay
    val update_hits = u.bpuMeta.ubtbHits
    val update_taken = u.taken

    val update_bank = getBank(update_br_pc)
    val update_base_bank = getBank(update_fetch_pc)
    val update_tag = getTag(update_br_pc)
    val update_target = Mux(u.pd.isBr, u.brTarget, u.target)
    val update_target_lower = update_target(lowerBitsSize-1+instOffsetBits, instOffsetBits)
    val update_is_BR_or_JAL = (u.pd.brType === BrType.branch) || (u.pd.brType === BrType.jal) 
  
  
    val jalFirstEncountered = !u.isMisPred && !u.bpuMeta.btbHitJal && (u.pd.brType === BrType.jal)
    val entry_write_valid = io.update.valid && (u.isMisPred || jalFirstEncountered) && !u.isReplay //io.update.valid //&& update_is_BR_or_JAL
    val meta_write_valid = io.update.valid && (u.isMisPred || jalFirstEncountered) && !u.isReplay//io.update.valid //&& update_is_BR_or_JAL

    for (b <- 0 until PredictWidth) {
        datas(b).wen := do_reset || (entry_write_valid && b.U === update_bank)
        datas(b).wWay := Mux(do_reset, reset_way, update_write_way)
        datas(b).wdata := Mux(do_reset, 0.U.asTypeOf(new MicroBTBEntry), update_target_lower.asTypeOf(new MicroBTBEntry))
    }



    //write the uBTBMeta
    (0 until PredictWidth).map(i => metas(i).rWay := update_write_way)
    val update_write_meta = Wire(new MicroBTBMeta)
    update_write_meta.is_Br  := u.pd.brType === BrType.branch
    update_write_meta.is_RVC := u.pd.isRVC
    update_write_meta.valid  := true.B
    update_write_meta.tag    := update_tag
    update_write_meta.pred   := Mux(!update_hits,
                                    Mux(update_taken,3.U,0.U),
                                    satUpdate( metas(update_bank).rpred,2,update_taken)
                                )

    for (b <- 0 until PredictWidth) {
        metas(b).wen := do_reset || (meta_write_valid && b.U === update_bank)
        metas(b).wWay := Mux(do_reset, reset_way, update_write_way)
        metas(b).wdata := Mux(do_reset, 0.U.asTypeOf(new MicroBTBMeta), update_write_meta)
    }

    if (!env.FPGAPlatform && env.EnablePerfDebug) {
      val ubtbAns = Wire(Vec(PredictWidth, new PredictorAnswer))
      // ubtbAns.hit := io.pc.valid && read_hit_vec.asUInt.orR

      ubtbAns.zipWithIndex.foreach{ case(x,i) =>
          x.hit := io.out.hits(i)
          x.taken := io.out.takens(i)
          x.target := io.out.targets(i)
      }

      ExcitingUtils.addSource(ubtbAns, "ubtbAns")
    }

    if (BPUDebug && debug) {
        XSDebug(read_valid,"uBTB read req: pc:0x%x, tag:%x \n",io.pc.bits,read_req_tag)
        XSDebug(read_valid,"uBTB read resp:   read_hit_vec:%b, \n",read_hit_vec.asUInt)
        for(i <- 0 until PredictWidth) {
            XSDebug(read_valid,"bank(%d)   hit:%d   way:%d   valid:%d  is_RVC:%d  taken:%d   isBr:%d   target:0x%x  alloc_way:%d\n",
                                    i.U,read_hit_vec(i),read_hit_ways(i),read_resp(i).valid,read_resp(i).is_RVC,read_resp(i).taken,read_resp(i).is_Br,read_resp(i).target,out_ubtb_br_info.writeWay(i))
        }

        XSDebug(meta_write_valid,"uBTB update: update | pc:0x%x  | update hits:%b | | update_write_way:%d  | update_bank: %d| update_br_index:%d | update_tag:%x | update_lower 0x%x\n "
                    ,update_br_pc,update_hits,update_write_way,update_bank,update_br_idx,update_tag,update_target_lower(lowerBitsSize-1,0))
        XSDebug(meta_write_valid, "uBTB update: update_taken:%d | old_pred:%b | new_pred:%b\n",
            update_taken, metas(update_bank).rpred,
            Mux(!update_hits,
                    Mux(update_taken,3.U,0.U),
                    satUpdate( metas(update_bank).rpred,2,update_taken)
                ))

    }

    if (extended_stat) {
        val high_identical = update_target(VAddrBits-1, lowerBitsSize) =/= update_fetch_pc(VAddrBits-1, lowerBitsSize)
        XSDebug(io.update.valid, "extended_stat: identical %d\n", high_identical)
    }
   
   //bypass:read-after-write 
//    for( b <- 0 until PredictWidth) {
//         when(update_bank === b.U && meta_write_valid && read_valid
//             && Mux(b.U < update_base_bank,update_tag===read_req_tag+1.U ,update_tag===read_req_tag))  //read and write is the same fetch-packet
//         {
//             io.out.targets(b) := u.target
//             io.out.takens(b) := u.taken
//             io.out.is_RVC(b) := u.pd.isRVC
//             io.out.notTakens(b) := (u.pd.brType === BrType.branch) && (!io.out.takens(b))
//             XSDebug("uBTB bypass hit! :   hitpc:0x%x |  hitbanck:%d  |  out_target:0x%x\n",io.pc.bits+(b<<1).asUInt(),b.U, io.out.targets(b))
//         }
//     }
}