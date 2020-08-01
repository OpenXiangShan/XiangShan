package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.ALUOpType
import utils._

import scala.math.min

trait MicroBTBPatameter{
    val nWays = 16
    val offsetSize = 13
}

class MicroBTB extends BasePredictor
    with MicroBTBPatameter
{
    val tagSize = VAddrBits - log2Ceil(PredictWidth) - 1

    class MicroBTBResp extends Resp
    {
        val targets = Vec(PredictWidth, ValidUndirectioned(UInt(VAddrBits.W)))
        val takens = Vec(PredictWidth, Bool())
        val notTakens = Vec(PredictWidth, Bool())
        val isRVC = Vec(PredictWidth, Bool())
    }

    class MicroBTBPredictMeta extends Meta
    {
        val writeWay = UInt(log2Ceil(nWays).W)
        val hits = Vec(PredictWidth,Bool())
    }
    val out_uBTBMeta = Wire(new MicroBTBPredictMeta)
    override val uBTBMetaLen = out_uBTBMeta.asUInt.getWidth

    class MicroBTBIO extends DefaultBasePredictorIO
    {
        val out = Output(new MicroBTBResp)   //
        val uBTBMeta = Output(new MicroBTBPredictMeta)
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

    val uBTBMeta = RegInit((0.U).asTypeOf(Vec(nWays, Vec(PredictWidth, new MicroBTBMeta))))
    val uBTB = Reg(Vec(nWays, Vec(PredictWidth, new MicroBTBEntry)))

    //uBTB read
    //tag is bank align
    val read_valid = io.pc.valid
    val read_req_tag = getTag(io.pc.bits)
    val read_req_basebank = getBank(io.pc.bits)
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
    val isInNextRow = VecInit((0 until PredictWidth).map(_.U < read_req_basebank))
    val read_hit_ohs = read_bank_inOrder.map{ b =>
        VecInit((0 until nWays) map {w => 
            Mux(isInNextRow(b),read_req_tag + 1.U,read_req_tag) === uBTBMeta(b)(w).tag
        })
    }


    val read_hit_vec = VecInit(read_hit_ohs.map{oh => ParallelOR(oh).asBool})
    val read_hit_ways = VecInit(read_hit_ohs.map{oh => PriorityEncoder(oh)})
    val read_hit =  ParallelOR(read_hit_vec).asBool
    val read_hit_way = PriorityEncoder(ParallelOR(read_hit_vec.map(_.asUInt)))
    
    read_bank_inOrder.foreach{ i =>
        when(read_valid){
            val  uBTBMeta_resp = uBTBMeta(i)(read_hit_ways(i))
            val  btb_resp = uBTB(i)(read_hit_ways(i))
            var  index = 0
            read_resp(i).valid := uBTBMeta_resp.valid && read_hit_vec(i) && read_mask(index)
            read_resp(i).taken := read_resp(i).valid && uBTBMeta_resp.pred(1)
            read_resp(i).notTaken := read_resp(i).valid && !uBTBMeta_resp.pred(1)
            read_resp(i).target := (io.pc.bits).asSInt + (index<<1).S + btb_resp.offset
            index += 1

            out_uBTBMeta.hits(i) := read_hit_vec(i)
        }
    }

    //TODO: way alloc algorithm
    val alloc_way = { 
        val r_uBTBMetas = Cat(VecInit(uBTBMeta.map(e => VecInit(e.map(_.tag)))).asUInt, (read_req_tag)(tagSize-1,0))
        val l = log2Ceil(nWays)
        val nChunks = (r_uBTBMetas.getWidth + l - 1) / l
        val chunks = (0 until nChunks) map { i =>
        r_uBTBMetas(min((i+1)*l, r_uBTBMetas.getWidth)-1, i*l)
        }
        chunks.reduce(_^_)
    }
    val out_uBTBMeta.writeWay = Mux(read_hit,read_hit_way,alloc_way)

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
        } .otherwise 
        {
            io.out := (0.U).asTypeOf(new MicroBTBResp)
        }

    }

    //uBTB update 
    //backend should send fetch pc to update
    val update_fetch_pc  = Wire(UInt(VAddrBits.W))//TODO: io.update.bitspc
    val update_idx = io.update.bits.fetchIdx
    val update_br_offset = update_idx << 1.U
    val update_br_pc = update_fetch_pc + update_br_offset
    val update_write_way = io.update.bits.brInfo.ubtbWriteWay
    val update_hits = io.update.bits.brInfo.ubtbHits
    val update_taken = io.update.bits.taken

    val update_bank = getBank(update_br_pc)
    val update_base_bank = getBank(update_fetch_pc)
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
    //write the uBTBMeta
    when(uBTB_Meta_write_valid)
    {
        //commit update
        uBTBMeta(update_bank)(update_write_way).is_Br := io.update.bits.pd.brType === BrType.branch
        uBTBMeta(update_bank)(update_write_way).is_RVC := io.update.bits.pd.isRVC
        uBTBMeta(update_bank)(update_write_way).valid := true.B
        uBTBMeta(update_bank)(update_write_way).tag := update_tag
        uBTBMeta(update_bank)(update_write_way).pred := 
        Mux(!update_hits(update_bank),
            Mux(update_taken,3.U,0.U),
            satUpdate( uBTBMeta(update_bank)(update_write_way).pred,2,update_taken)
        )
    }

    //bypass:read-after-write 
    val rawBypassHit = Wire(Vec(PredictWidth, Bool()))
    for( b <- 0 until PredictWidth) {
        when(update_bank === b.U && read_hit_vec(b) && uBTB_Meta_write_valid && read_valid
            && Mux(b.U < update_base_bank,update_tag===read_req_tag+1.U ,update_tag===read_req_tag))  //read and write is the same fetch-packet
        {
            io.out.targets(b) := io.update.bits.target
            io.out.takens(b) := io.update.bits.taken
            io.out.isRVC(b) := io.update.bits.pd.isRVC
            io.out.notTakens(b) := (io.update.bits.pd.brType === BrType.branch) && (!io.out.takens(b))
        }
    }



}