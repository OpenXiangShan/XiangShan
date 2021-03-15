package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import chisel3.experimental.chiselName

import scala.math.min

trait MicroBTBPatameter{
    val nWays = 16
    val lowerBitsSize = 20
    val tagSize = 20
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

    class MicroBTBBranchInfo extends Meta {}

    class MicroBTBIO extends DefaultBasePredictorIO
    {
        val out = Output(new MicroBTBResp)   //
    }

    override val debug = true
    override val io = IO(new MicroBTBIO)

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

    class MicroBTBData extends XSBundle
    {
        val lower = UInt(lowerBitsSize.W)
    }

    class ReadResp extends XSBundle
    {
        val valid = Bool()
        val taken = Bool()
        val target = UInt(VAddrBits.W)
        val is_RVC = Bool()
        val is_Br = Bool()
    }

    class UBTBBank(val nWays: Int) extends XSModule with HasIFUConst {
        val io = IO(new Bundle {
            val read_pc = Flipped(Valid(UInt(VAddrBits.W)))
            val read_resp = Output(new ReadResp)
            val read_hit = Output(Bool())

            val update_write_meta = Flipped(Valid(new MicroBTBMeta))
            val update_write_data = Flipped(Valid(new MicroBTBData))
            val update_taken = Input(Bool())
        })

        val debug_io = IO(new Bundle {
            val read_hit = Output(Bool())
            val read_hit_way = Output(UInt(log2Ceil(nWays).W))

            val update_hit = Output(Bool())
            val update_hit_way = Output(UInt(log2Ceil(nWays).W))
            val update_write_way = Output(UInt(log2Ceil(nWays).W))
            val update_old_pred = Output(UInt(2.W))
            val update_new_pred = Output(UInt(2.W))
        })
        val meta = Module(new AsyncDataModuleTemplate(new MicroBTBMeta, nWays, nWays*2, 1))
        val data = Module(new AsyncDataModuleTemplate(new MicroBTBData, nWays,   nWays, 1))

        for (w <- 0 until nWays) {
            meta.io.raddr(w) := w.U
            meta.io.raddr(w+nWays) := w.U
            data.io.raddr(w) := w.U
        }
        
        val rmetas = meta.io.rdata.take(nWays)
        val rdatas = data.io.rdata
        
        val packetAlignedPC = packetAligned(io.read_pc.bits)
        val read_tag = getTag(io.read_pc.bits)
        
        val hits = VecInit(rmetas.map(m => m.valid && m.tag === read_tag))
        val takens = VecInit(rmetas.map(m => m.pred(1)))
        val hit_oh = hits.asUInt
        val hit_and_taken = VecInit((hits zip takens) map {case (h, t) => h && t}).asUInt.orR
        val hit_meta = ParallelMux(hits zip rmetas)
        val hit_data = ParallelMux(hits zip rdatas)
        val target = Cat(io.read_pc.bits(VAddrBits-1, lowerBitsSize+instOffsetBits), hit_data.lower, 0.U(instOffsetBits.W))
        
        val ren = io.read_pc.valid
        io.read_resp.valid := ren
        io.read_resp.is_RVC := ren && hit_meta.is_RVC
        io.read_resp.is_Br := ren && hit_meta.is_Br
        io.read_resp.taken := ren && hit_and_taken
        io.read_resp.target := target
        io.read_hit := hit_oh.orR

        debug_io.read_hit := hit_oh.orR
        debug_io.read_hit_way := OHToUInt(hit_oh)
        
        val do_reset = RegInit(true.B)
        val reset_way = RegInit(0.U(log2Ceil(nWays).W))
        when (RegNext(reset.asBool) && !reset.asBool) {
            do_reset := true.B
            reset_way := 0.U
        }
        when (do_reset) { reset_way := reset_way + 1.U }
        when (reset_way === (nWays-1).U) { do_reset := false.B }

        val update_rmetas = meta.io.rdata.drop(nWays)
        val update_tag = io.update_write_meta.bits.tag
        val update_hits = VecInit(update_rmetas.map(m => m.valid && m.tag === update_tag))
        val update_hit = update_hits.asUInt.orR
        val update_hit_way = OHToUInt(update_hits.asUInt)
        val update_hit_meta = ParallelMux(update_hits zip update_rmetas)
        val update_old_pred = update_hit_meta.pred
        val update_new_pred =
            Mux(update_hit,
                satUpdate(update_old_pred, 2, io.update_taken),
                Mux(io.update_taken, 3.U, 0.U))
        val update_alloc_way = {
            val source = Cat(VecInit(update_rmetas.map(_.tag)).asUInt, update_tag)
            val l = log2Ceil(nWays)
            val nChunks = (source.getWidth + l - 1) / l
            val chunks = (0 until nChunks) map { i =>
                source(min((i+1)*l, source.getWidth)-1, i*l)
            }
            ParallelXOR(chunks)
        }
        val update_emptys = update_rmetas.map(m => !m.valid)
        val update_has_empty_way = update_emptys.reduce(_||_)
        val update_empty_way = ParallelPriorityEncoder(update_emptys)
        val update_way = Mux(update_hit, update_hit_way, Mux(update_has_empty_way, update_empty_way, update_alloc_way))
    
        meta.io.waddr(0) := Mux(do_reset, reset_way, RegNext(update_way))
        meta.io.wen(0)   := do_reset || RegNext(io.update_write_meta.valid)
        meta.io.wdata(0) := Mux(do_reset,
                                0.U.asTypeOf(new MicroBTBMeta),
                                RegNext(io.update_write_meta.bits))
        meta.io.wdata(0).pred := Mux(do_reset, 0.U(2.W), RegNext(update_new_pred))
        data.io.waddr(0) := Mux(do_reset, reset_way, RegNext(update_way))
        data.io.wen(0)   := do_reset || RegNext(io.update_write_data.valid)
        data.io.wdata(0) := Mux(do_reset,
                                0.U.asTypeOf(new MicroBTBData),
                                RegNext(io.update_write_data.bits))
        
        debug_io.update_hit := update_hit
        debug_io.update_hit_way := update_hit_way
        debug_io.update_write_way := update_way
        debug_io.update_old_pred := update_old_pred
        debug_io.update_new_pred := update_new_pred
    }

    val ubtbBanks = Seq.fill(PredictWidth)(Module(new UBTBBank(nWays)))
    val banks = VecInit(ubtbBanks.map(_.io))
    
    val read_resps = VecInit(banks.map(b => b.read_resp))

    for (b <- 0 until PredictWidth) {
        banks(b).read_pc.valid := io.inMask(b)
        banks(b).read_pc.bits := io.pc.bits
        
        //only when hit and instruction valid and entry valid can output data
        io.out.targets(b) := read_resps(b).target
        io.out.hits(b)   := banks(b).read_hit && ctrl.ubtb_enable
        io.out.takens(b) := read_resps(b).taken
        io.out.is_RVC(b) := read_resps(b).is_RVC
        io.out.brMask(b) := read_resps(b).is_Br
    }

    //uBTB update 
    //backend should send fetch pc to update
    val u = RegNext(io.update.bits)
    val update_valid = RegNext(io.update.valid)
    val update_packet_pc = packetAligned(u.ftqPC)
    val update_takens = u.takens

    val update_tag = getTag(update_packet_pc)
    val update_target_lower = u.target(lowerBitsSize-1+instOffsetBits, instOffsetBits)
  
    // only when taken should we update target
    val data_write_valids = 
        VecInit((0 until PredictWidth).map(i =>
            update_valid && u.valids(i) && u.takens(i)))
    val meta_write_valids = 
        VecInit((0 until PredictWidth).map(i =>
            update_valid && u.valids(i) && (u.br_mask(i) || u.takens(i))))
        
    val update_write_metas = Wire(Vec(PredictWidth, new MicroBTBMeta))
    val update_write_datas = Wire(Vec(PredictWidth, new MicroBTBData))
    for (i <- 0 until PredictWidth) {
        update_write_metas(i).is_Br  := u.br_mask(i)
        update_write_metas(i).is_RVC := u.rvc_mask(i)
        update_write_metas(i).valid  := true.B
        update_write_metas(i).tag    := update_tag
        update_write_metas(i).pred   := DontCare

        update_write_datas(i).lower := update_target_lower
    }
    
    for (b <- 0 until PredictWidth) {
        banks(b).update_write_meta.valid := meta_write_valids(b)
        banks(b).update_write_meta.bits := update_write_metas(b)
        banks(b).update_write_data.valid := data_write_valids(b)
        banks(b).update_write_data.bits := update_write_datas(b)
        banks(b).update_taken := update_takens(b)
    }

    if (!env.FPGAPlatform) {
        XSPerf("ubtb_commit_hits",
            PopCount((u.takens zip u.valids zip u.metas zip u.pd) map {
                case (((t, v), m), pd)  => t && v && m.ubtbHit.asBool && !pd.notCFI && update_valid}))
        XSPerf("ubtb_commit_misses",
            PopCount((u.takens zip u.valids zip u.metas zip u.pd) map {
                case (((t, v), m), pd)  => t && v && !m.ubtbHit.asBool && !pd.notCFI && update_valid}))
    }
    
    if (BPUDebug && debug) {
        val update_pcs  = VecInit((0 until PredictWidth).map(i => update_packet_pc + (i << instOffsetBits).U))
        val update_bank = u.cfiIndex.bits
        val read_valid = io.pc.valid
        val read_req_tag = getTag(io.pc.bits)
        val debug_banks = VecInit(ubtbBanks.map(_.debug_io))
        val read_hit_vec = VecInit(debug_banks.map(b => b.read_hit))
        val read_hit_ways = VecInit(debug_banks.map(b => b.read_hit_way))
        val update_hits = VecInit(debug_banks.map(b => b.update_hit))
        val update_hit_ways = VecInit(debug_banks.map(b => b.update_hit_way))
        val update_write_ways = VecInit(debug_banks.map(b => b.update_write_way))
        val update_old_preds = VecInit(debug_banks.map(b => b.update_old_pred))
        val update_new_preds = VecInit(debug_banks.map(b => b.update_new_pred))
        XSDebug(read_valid,p"uBTB read req: pc:0x${Hexadecimal(io.pc.bits)}, tag:${Hexadecimal(read_req_tag)}\n")
        XSDebug(read_valid,p"uBTB read resp: read_hit_vec:${Binary(read_hit_vec.asUInt)}\n")
        for(i <- 0 until PredictWidth) {
            XSDebug(read_valid,
                p"bank($i) hit:${read_hit_vec(i)} way:${read_hit_ways(i)} " +
                p"valid:${read_resps(i).valid} is_RVC:${read_resps(i).is_RVC} " +
                p"taken:${read_resps(i).taken} isBr:${read_resps(i).is_Br} " +
                p"target:0x${Hexadecimal(read_resps(i).target)}\n")
            XSDebug(data_write_valids(i),
                p"uBTB update data($i): update pc:0x${Hexadecimal(update_pcs(i))} " +
                p"update_lower 0x$update_target_lower\n")
            XSDebug(meta_write_valids(i),
                p"update hit:${update_hits(i)} udpate_hit_way:${update_hit_ways(i)} " +
                p"update_write_way:${update_write_ways(i)} update_taken:${update_takens(i)}" +
                p"isBr:${u.br_mask(i)} isRVC:${u.rvc_mask(i)} update_tag:${update_tag}" +
                p"update_pred(${update_old_preds(i)} -> ${update_new_preds(i)}\n")
        }
    }
}