package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.ALUOpType
import utils._

import scala.math.min

trait MicroBTBPatameter{
    val nWays = 16
    val offsetSize = 20
}

class MicroBTB extends BasePredictor
    with MicroBTBPatameter
{
    val tagSize = VAddrBits - log2Ceil(PredictWidth) - 1

    class MicroBTBResp extends Resp
    {
        val targets = Vec(PredictWidth, UInt(VAddrBits.W))
        val hits = Vec(PredictWidth, Bool())
        val takens = Vec(PredictWidth, Bool())
        val notTakens = Vec(PredictWidth, Bool())
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
        val uBTBBranchInfo = Output(new MicroBTBBranchInfo)
    }

    override val io = IO(new MicroBTBIO)
    io.uBTBBranchInfo <> out_ubtb_br_info

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

    XSDebug(read_valid,"uBTB read req: pc:0x%x, tag:%x  basebank:%d\n",io.pc.bits,read_req_tag,read_req_basebank)
    
    class ReadRespEntry extends XSBundle
    {
        val is_RVC = Bool()
        val target = UInt(VAddrBits.W)
        val valid = Bool()
        val taken = Bool()
        val notTaken = Bool()
    }
    val read_resp = Wire(Vec(PredictWidth,new ReadRespEntry))

    val read_bank_inOrder = VecInit((0 until PredictWidth).map(b => (read_req_basebank + b.U)(log2Up(PredictWidth)-1,0) ))
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
    

    val  uBTBMeta_resp = VecInit((0 until PredictWidth).map(b => uBTBMeta(read_bank_inOrder(b))(read_hit_ways(b))))//uBTBMeta(i)(read_hit_ways(index))
    val  btb_resp = VecInit((0 until PredictWidth).map(b => uBTB(read_bank_inOrder(b))(read_hit_ways(b))))  

    for(i <- 0 until PredictWidth){
        // do not need to decide whether to produce results\
        read_resp(i).valid := uBTBMeta_resp(i).valid && read_hit_vec(i) && read_mask(i)
        read_resp(i).taken := read_resp(i).valid && uBTBMeta_resp(i).pred(1)
        read_resp(i).notTaken := read_resp(i).valid && !uBTBMeta_resp(i).pred(1)
        read_resp(i).target := ((io.pc.bits).asSInt + (i<<1).S + btb_resp(i).offset).asUInt
        read_resp(i).is_RVC := read_resp(i).valid && uBTBMeta_resp(i).is_RVC

        out_ubtb_br_info.hits(i) := read_hit_vec(i)
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
    out_ubtb_br_info.writeWay.map(_:= Mux(read_hit,read_hit_way,alloc_way))
    XSDebug(read_valid,"uBTB read resp:   read_hit_vec:%d, read_hit_way:%d  alloc_way:%d\n",read_hit_vec.asUInt,read_hit_way,alloc_way)
    for(i <- 0 until PredictWidth) {
        XSDebug(read_valid,"bank(%d)   hit:%d   way:%d   valid:%d  is_RVC:%d  taken:%d   notTaken:%d   target:0x%x\n",
                                 i.U,read_hit_vec(i),read_hit_ways(i),read_resp(i).valid,read_resp(i).is_RVC,read_resp(i).taken,read_resp(i).notTaken,read_resp(i).target )
    }
    //response
    //only when hit and instruction valid and entry valid can output data
    for(i <- 0 until PredictWidth)
    {
        when(read_resp(i).valid)
        {
            io.out.targets(i) := read_resp(i).target
            io.out.hits(i) := true.B
            io.out.takens(i) := read_resp(i).taken
            io.out.is_RVC(i) := read_resp(i).is_RVC
            io.out.notTakens(i) := read_resp(i).notTaken
        } .otherwise 
        {
            io.out := (0.U).asTypeOf(new MicroBTBResp)
        }

    }

    //uBTB update 
    //backend should send fetch pc to update
    val update_fetch_pc  = io.update.bits.pc
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

    val uBTB_write_valid = io.update.valid && io.update.bits.isMisPred && update_is_BR_or_JAL
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
    XSDebug(uBTB_Meta_write_valid,"uBTB update: update fetch pc:0x%x  | real pc:0x%x  | update hits%b  | update_write_way:%d\n",update_fetch_pc,update_br_pc,update_hits,update_write_way)
   
   //bypass:read-after-write 
   for( b <- 0 until PredictWidth) {
        when(update_bank === b.U && uBTB_Meta_write_valid && read_valid
            && Mux(b.U < update_base_bank,update_tag===read_req_tag+1.U ,update_tag===read_req_tag))  //read and write is the same fetch-packet
        {
            io.out.targets(b) := io.update.bits.target
            io.out.takens(b) := io.update.bits.taken
            io.out.is_RVC(b) := io.update.bits.pd.isRVC
            io.out.notTakens(b) := (io.update.bits.pd.brType === BrType.branch) && (!io.out.takens(b))
            XSDebug("uBTB bypass hit! :   hitpc:0x%x |  hitbanck:%d  |  out_target:0x%x\n",io.pc.bits+ (b.U << 1.U),b.U, io.out.targets(b))
        }
    }
}