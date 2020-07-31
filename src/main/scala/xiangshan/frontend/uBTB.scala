package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.ALUOpType
import utils._

trait MicroBTBPatameter{
    val nWays = 16
    val offsetSize = 13
    val tagSize = VAddrBits - log2Ceil(PredictWidth) - 1

}

class MicroBTB extends BasePredictor
    with MicroBTBPatameter
{
    class MicroBTBResp extends resp
    {
        val targets = Vec(PredictWidth, ValidUndirectioned(UInt(VaddrBits.W)))
        val takens = Vec(PredictWidth, Bool())
        val notTakens = Vec(PredictWidth, Bool())
        val isRVC = Vec(PredictWidth, Bool())
    }

    class MicroBTBPredictMeta extends meta
    {
        val writeWay = UInt(log2Ceil(nWays).W)
        val hits = Vec(PredictWidth,Bool())
    }
    val out_meta = Wire(new MicroBTBMeta)
    override val metaLen = out_meta.asUInt.getWidth

    class MicroBTBIO extends defaultBasePredictorIO
    {
        val out = Output(new MicroBTBResp)   //
        val meta = Output(new MicroBTBPredictMeta)
    }

    val io = IO(new MicroBTBIO)

    def getTag(pc: UInt) = pc >> (log2Ceil(PredictWidth) + 1).U
    def getBank(pc: UInt) = pc(log2Ceil(PredictWidth) ,1)
    def satUpdate(old: UInt, len: Int, taken: Bool): UInt = {
        val oldSatTaken = old === ((1 << len)-1).U
        val oldSatNotTaken = old === 0.U
        Mux(oldSatTaken && taken, ((1 << len)-1).U,
            Mux(oldSatNotTaken && !taken, 0.U,
            Mux(taken, old + 1.U, old - 1.U)))
    } 

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
        val offset = SInt(offsetSize.W)
    }

    val uBTBMeta = RegInit(0.U).asTypeOf(Vec(PredictWidth,Vec(nWays,new MicroBTBMeta)))
    val uBTB  = Reg(Vec(PredictWidth,Vec(nWays,new MicroBTBEntry)))

    //uBTB read
    //tag is bank align
    val read_req_tag = getTag(io.pc)
    val read_req_basebank = getBank(io.pc)
    val read_mask = io.inMask
    
    class ReadRespEntry extends XSBundle
    {
        val is_RVC = Bool()
        val target = UInt(VAddrBits.W)
        val valid = Bool()
        val taken = Bool()
        val notTaken = Bool()
    }
    val read_resp = Wire(Vec(PredictWidth,new ReadRespEntry))

    val read_bank_inOrder = VecInit((0 until PredictWidth).map(b => (read_req_basebank + b.U)(PredictWidth-1,0) ))

    val read_hit_ohs = read_bank_inOrder.map{ b =>
        VecInit((0 until nWays) map {w => 
            uBTBMeta(b)(w).tag === read_req_tag
        })
    }


    val read_hit_vec = read_hit_ohs.map{oh => ParallelOR(oh)}
    val read_hit_ways = read_hit_ohs.map{oh => PriorityEncoder(oh)}
    val read_hit =  ParallelOR(read_hit_vec)
    val read_hit_way = PriorityEncoder(ParallelOR(read_hit_vec.map(_.asUInt)))
    
    read_bank_inOrder.foreach{ i =>
        val  meta_resp = uBTBMeta(i)(read_hit_ways(i))
        val  btb_resp = uBTB(i)(read_hit_ways(i))
        var  index = 0
        read_resp(i).valid := meta_resp.valid && read_hit_vec(i) && read_mask(index)
        read_resp(i).taken := read_resp(i).valid && meta_resp.pred(1)
        read_resp(i).notTaken := read_resp(i).valid && !meta_resp.pred(1)
        read_resp(i).target := (io.in.pc).asSInt + (index<<1).S + btb_resp.offset
        index += 1

        out_meta.hits(i) := read_hit_vec(i)
    }

    //TODO: way alloc algorithm
    val alloc_way = { 
        val r_metas = Cat(VecInit(meta.map(e => VecInit(e.map(_.tag)))).asUInt, (s1_idx)(tagSz-1,0))
        val l = log2Ceil(nWays)
        val nChunks = (r_metas.getWidth + l - 1) / l
        val chunks = (0 until nChunks) map { i =>
        r_metas(min((i+1)*l, r_metas.getWidth)-1, i*l)
        }
        chunks.reduce(_^_)
    }
    val out_meta.writeWay = Mux(read_hit,read_hit_way,alloc_way)

    //response
    //only when hit and instruction valid and entry valid can output data
    for(i <- 0 until PredictWidth)
    {
        when(read_resp(i).valid)
        {
            io.out.targets(i) := read_resp(i).target
            io.out.takens(i) := read_resp(i).taken
            io.out.isRVC(i) := read_resp(i).is_RVC
            io.out.notTakens(i) := read_resp(i).notTaken
\        } .otherwise 
        {
            io.out := (0.U).asTypeOf(new MicroBTBResp)
        }

    }

    //uBTB update 
    //backend should send fetch pc to update
    val update_fetch_pc  = Wire(UInt(VAddrBits.W))//TODO: io.update.bitspc
    val update_idx = io.update.bitsfetchIdx
    val update_br_offset = update_idx << 1.U
    val update_br_pc = update_fetch_pc + update_br_offset
    val update_write_way = io.update.bits.brInfo.ubtbWriteWay
    val update_hits = io.update.bits.brInfo.ubtbHits
    val update_taken = io.update.bits.taken

    val update_bank = getBank(update_br_pc)
    val update_tag = getTag(update_br_pc)
    val update_taget_offset =  io.update.bits.target.asSInt - update_br_pc.asSInt
    val update_is_BR_or_JAL = (io.update.bits.pd.brType === BrType.branch) || (io.update.bits.pd.brType === BrType.jal) 

    val uBTB_write_valid = io.update.valid && io.update.bits.isMisPred
    val uBTB_Meta_write_valid = io.update.valid && update_is_BR_or_JAL
    //write btb target when miss prediction
    when(uBTB_write_valid)
    {
        uBTB(update_bank)(update_write_way).offset := update_taget_offset
    }
    //write the meta
    when(uBTB_Meta_write_valid)
    {
        //commit update
        uBTBMeta(update_bank)(update_write_way).is_Br := io.update.bits.pd.brType === BrType.branch
        uBTBMeta(update_bank)(update_write_way).is_RVC := io.update.bits.pd.isRVC
        uBTBMeta(update_bank)(update_write_way).valid := true.B
        uBTBMeta(update_bank)(update_write_way).tag := update_tag
        uBTBMeta(update_bank)(update_write_way).pred := 
        Mux(!update_hits(update_bank),
            Mux(update_taken,3.0,0.U)
            satUpdate( uBTBMeta(update_bank)(update_write_way).pred,2,update_taken)
        )
    }

    //bypass:read-after-write 


}