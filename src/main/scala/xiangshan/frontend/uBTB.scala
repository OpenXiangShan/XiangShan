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
            val to_write_way = Output(UInt(log2Ceil(nWays).W))

            val update_way = Input(UInt(log2Ceil(nWays).W))
            val update_read_pred = Output(UInt(2.W))

            val update_write_meta = Flipped(Valid(new MicroBTBMeta))
            val update_write_data = Flipped(Valid(new MicroBTBData))
        })
        val meta = Module(new AsyncDataModuleTemplate(new MicroBTBMeta, nWays, nWays, 1))
        val data = Module(new AsyncDataModuleTemplate(new MicroBTBData, nWays, nWays, 1))

        for (w <- 0 until nWays) {
            meta.io.raddr(w) := w.U
            data.io.raddr(w) := w.U
        }
        meta.io.waddr(0) := io.update_way
        meta.io.wen(0)   := io.update_write_meta.valid
        meta.io.wdata(0) := io.update_write_meta.bits
        data.io.waddr(0) := io.update_way
        data.io.wen(0)   := io.update_write_data.valid
        data.io.wdata(0) := io.update_write_data.bits

        val rmetas = meta.io.rdata
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
        
        val emptys = rmetas.map(m => !m.valid)
        val allocatable = VecInit(emptys).asUInt.orR
        val empty_way = ParallelPriorityEncoder(emptys)
        val hit_way = OHToUInt(hit_oh)
        val random_way = LFSR64()(log2Ceil(nWays)-1,0)
        io.to_write_way := Mux(hit_oh.orR, hit_way, Mux(allocatable, empty_way, random_way))

        val ren = io.read_pc.valid
        io.read_resp.valid := ren
        io.read_resp.is_RVC := ren && hit_meta.is_RVC
        io.read_resp.is_Br := ren && hit_meta.is_Br
        io.read_resp.taken := ren && hit_and_taken
        io.read_resp.target := target

        io.read_hit := ren && hit_oh.orR

        io.update_read_pred := rmetas(io.update_way).pred
    }

    val ubtbBanks = Seq.fill(PredictWidth)(Module(new UBTBBank(nWays)))
    val banks = VecInit(ubtbBanks.map(_.io))
    
    val read_resps = VecInit(banks.map(b => b.read_resp))

    for (b <- 0 until PredictWidth) {
        banks(b).read_pc.valid := io.pc.valid && io.inMask(b)
        banks(b).read_pc.bits := io.pc.bits
        
        out_ubtb_br_info.writeWay(b) := banks(b).to_write_way
        out_ubtb_br_info.hits(b) := banks(b).read_hit

        //only when hit and instruction valid and entry valid can output data
        io.out.targets(b) := read_resps(b).target
        io.out.hits(b)   := banks(b).read_hit
        io.out.takens(b) := read_resps(b).taken
        io.out.is_RVC(b) := read_resps(b).is_RVC
        io.out.brMask(b) := read_resps(b).is_Br
    }

    val do_reset = RegInit(true.B)
    val reset_way = RegInit(0.U(log2Ceil(nWays).W))
    when (do_reset) { reset_way := reset_way + 1.U }
    when (reset_way === (nWays-1).U) { do_reset := false.B }

    //uBTB update 
    //backend should send fetch pc to update
    val u = RegNext(io.update.bits)
    val update_valid = RegNext(io.update.valid)
    val update_packet_pc = packetAligned(u.ftqPC)
    val update_write_ways = VecInit(u.metas.map(_.ubtbWriteWay))
    val update_hits = u.metas.map(_.ubtbHits)
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
        
    val new_preds =
        VecInit((0 until PredictWidth).map(i => 
            Mux(!update_hits(i), Mux(update_takens(i),3.U,0.U),
                satUpdate(banks(i).update_read_pred,2,update_takens(i)))))

    val update_write_metas = Wire(Vec(PredictWidth, new MicroBTBMeta))
    val update_write_datas = Wire(Vec(PredictWidth, new MicroBTBData))
    for (i <- 0 until PredictWidth) {
        update_write_metas(i).is_Br  := u.br_mask(i)
        update_write_metas(i).is_RVC := u.rvc_mask(i)
        update_write_metas(i).valid  := true.B
        update_write_metas(i).tag    := update_tag
        update_write_metas(i).pred   := new_preds(i)

        update_write_datas(i).lower := update_target_lower
    }
    
    for (b <- 0 until PredictWidth) {
        banks(b).update_way := update_write_ways(b)
        banks(b).update_write_meta.valid := do_reset || meta_write_valids(b)
        banks(b).update_write_meta.bits := 
            Mux(do_reset, 0.U.asTypeOf(new MicroBTBMeta), update_write_metas(b))
        banks(b).update_write_data.valid := do_reset || data_write_valids(b)
        banks(b).update_write_data.bits := 
            Mux(do_reset, 0.U.asTypeOf(new MicroBTBData), update_write_datas(b))
    }
    
    if (BPUDebug && debug) {
        val update_pcs  = VecInit((0 until PredictWidth).map(i => update_packet_pc + (i << instOffsetBits).U))
        val update_bank = u.cfiIndex.bits
        val read_valid = io.pc.valid
        val read_req_tag = getTag(io.pc.bits)
        val read_hit_vec = VecInit(banks.map(b => b.read_hit))
        val read_hit_ways = VecInit(banks.map(b => b.to_write_way))
        XSDebug(read_valid,"uBTB read req: pc:0x%x, tag:%x \n",io.pc.bits,read_req_tag)
        XSDebug(read_valid,"uBTB read resp:   read_hit_vec:%b, \n",read_hit_vec.asUInt)
        for(i <- 0 until PredictWidth) {
            XSDebug(read_valid,"bank(%d)   hit:%d   way:%d   valid:%d  is_RVC:%d  taken:%d   isBr:%d   target:0x%x  alloc_way:%d\n",
                        i.U, read_hit_vec(i), read_hit_ways(i), read_resps(i).valid, read_resps(i).is_RVC,
                        read_resps(i).taken, read_resps(i).is_Br, read_resps(i).target, out_ubtb_br_info.writeWay(i))
            XSDebug(data_write_valids(i),
                        "uBTB update data(%d): update | pc:0x%x  | update hits:%b | update_write_way:%d  |  update_lower 0x%x\n ",
                        i.U, update_pcs(i), update_hits(i), update_write_ways(i), update_target_lower(lowerBitsSize-1,0))
            XSDebug(meta_write_valids(i), "uBTB update meta(%d): update_taken:%d | old_pred:%b | new_pred:%b | br:%d | rvc:%d | update_tag:%x\n",
                i.U, update_takens(i), banks(i).update_read_pred, new_preds(i), u.br_mask(i), u.rvc_mask(i), update_tag)
        }
    }
}