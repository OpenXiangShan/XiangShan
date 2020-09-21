package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._

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
        val uBTBBranchInfo = Output(new MicroBTBBranchInfo)
    }

    override val debug = true
    override val io = IO(new MicroBTBIO)
    io.uBTBBranchInfo <> out_ubtb_br_info

    def getTag(pc: UInt) = (pc >> (log2Ceil(PredictWidth) + 1)).asUInt()
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
    // val read_mask = circularShiftLeft(io.inMask, PredictWidth, read_req_basebank)

    
    class ReadRespEntry extends XSBundle
    {
        val is_RVC = Bool()
        val target = UInt(VAddrBits.W)
        val valid = Bool()
        val taken = Bool()
        val is_Br = Bool()
    }
    val read_resp = Wire(Vec(PredictWidth,new ReadRespEntry))

    val read_bank_inOrder = VecInit((0 until PredictWidth).map(b => (read_req_basebank + b.U)(log2Up(PredictWidth)-1,0) ))
    val isInNextRow = VecInit((0 until PredictWidth).map(_.U < read_req_basebank))
    val read_hit_ohs = read_bank_inOrder.map{ b =>
        VecInit((0 until nWays) map {w => 
            Mux(isInNextRow(b),read_req_tag + 1.U,read_req_tag) === uBTBMeta(w)(b).tag
        })
    }
    val read_hit_vec = VecInit(read_hit_ohs.map{oh => ParallelOR(oh).asBool})
    val read_hit_ways = VecInit(read_hit_ohs.map{oh => PriorityEncoder(oh)})
    val read_hit =  ParallelOR(read_hit_vec).asBool
    val read_hit_way = PriorityEncoder(ParallelOR(read_hit_ohs.map(_.asUInt)))
    

    val  uBTBMeta_resp = VecInit((0 until PredictWidth).map(b => uBTBMeta(read_hit_ways(b))(read_bank_inOrder(b))))
    val  btb_resp = VecInit((0 until PredictWidth).map(b => uBTB(read_hit_ways(b))(read_bank_inOrder(b))))  

    for(i <- 0 until PredictWidth){
        // do not need to decide whether to produce results\
        read_resp(i).valid := uBTBMeta_resp(i).valid && read_hit_vec(i) && io.inMask(i)
        read_resp(i).taken := read_resp(i).valid && uBTBMeta_resp(i).pred(1)
        read_resp(i).is_Br  := read_resp(i).valid && uBTBMeta_resp(i).is_Br
        read_resp(i).target := ((io.pc.bits).asSInt + (i<<1).S + btb_resp(i).offset).asUInt
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
    val alloc_ways = read_bank_inOrder.map{ b => 
        alloc_way(VecInit(uBTBMeta.map(w => w(b).valid)).asUInt,
                  VecInit(uBTBMeta.map(w => w(b).tag)).asUInt,
                  Mux(isInNextRow(b).asBool,read_req_tag + 1.U,read_req_tag))
        
    }
    (0 until PredictWidth).map(i => out_ubtb_br_info.writeWay(i) := Mux(read_hit_vec(i).asBool,read_hit_ways(i),alloc_ways(i)))

    //response
    //only when hit and instruction valid and entry valid can output data
    for(i <- 0 until PredictWidth)
    {
        io.out.targets(i) := read_resp(i).target
        io.out.hits(i) := read_resp(i).valid
        io.out.takens(i) := read_resp(i).taken
        io.out.is_RVC(i) := read_resp(i).is_RVC
        io.out.brMask(i) := read_resp(i).is_Br
    }

    //uBTB update 
    //backend should send fetch pc to update
    val u = io.update.bits.ui
    val update_br_pc  = u.pc
    val update_br_idx = u.fetchIdx
    val update_br_offset = (update_br_idx << 1).asUInt()
    val update_fetch_pc = update_br_pc - update_br_offset
    val update_write_way = u.brInfo.ubtbWriteWay
    val update_hits = u.brInfo.ubtbHits
    val update_taken = u.taken

    val update_bank = getBank(update_br_pc)
    val update_base_bank = getBank(update_fetch_pc)
    val update_tag = getTag(update_br_pc)
    val update_target = Mux(u.pd.isBr, u.brTarget, u.target)
    val update_taget_offset =  update_target.asSInt - update_br_pc.asSInt
    val update_is_BR_or_JAL = (u.pd.brType === BrType.branch) || (u.pd.brType === BrType.jal) 
  
  
    val jalFirstEncountered = !u.isMisPred && !u.brInfo.btbHitJal && (u.pd.brType === BrType.jal)
    val entry_write_valid = io.update.valid && (u.isMisPred || !u.isMisPred && u.pd.isBr || jalFirstEncountered)//io.update.valid //&& update_is_BR_or_JAL
    val meta_write_valid = io.update.valid && (u.isMisPred || !u.isMisPred && u.pd.isBr || jalFirstEncountered)//io.update.valid //&& update_is_BR_or_JAL
    //write btb target when miss prediction
    when(entry_write_valid)
    {
        uBTB(update_write_way)(update_bank).offset := update_taget_offset
    }
    //write the uBTBMeta
    when(meta_write_valid)
    {
        //commit update
        uBTBMeta(update_write_way)(update_bank).is_Br := u.pd.brType === BrType.branch
        uBTBMeta(update_write_way)(update_bank).is_RVC := u.pd.isRVC
        //(0 until PredictWidth).foreach{b =>  uBTBMeta(update_write_way)(b).valid := false.B}
        uBTBMeta(update_write_way)(update_bank).valid := true.B
        uBTBMeta(update_write_way)(update_bank).tag := update_tag
        uBTBMeta(update_write_way)(update_bank).pred := 
        Mux(!update_hits,
            Mux(update_taken,3.U,0.U),
            satUpdate( uBTBMeta(update_write_way)(update_bank).pred,2,update_taken)
        )
    }

    if (BPUDebug && debug) {
        XSDebug(read_valid,"uBTB read req: pc:0x%x, tag:%x  basebank:%d\n",io.pc.bits,read_req_tag,read_req_basebank)
        XSDebug(read_valid,"uBTB read resp:   read_hit_vec:%b, \n",read_hit_vec.asUInt)
        for(i <- 0 until PredictWidth) {
            XSDebug(read_valid,"bank(%d)   hit:%d   way:%d   valid:%d  is_RVC:%d  taken:%d   isBr:%d   target:0x%x  alloc_way:%d\n",
                                    i.U,read_hit_vec(i),read_hit_ways(i),read_resp(i).valid,read_resp(i).is_RVC,read_resp(i).taken,read_resp(i).is_Br,read_resp(i).target,out_ubtb_br_info.writeWay(i))
        }

        XSDebug(meta_write_valid,"uBTB update: update | pc:0x%x  | update hits:%b | | update_write_way:%d  | update_bank: %d| update_br_index:%d | update_tag:%x | upadate_offset 0x%x\n "
                    ,update_br_pc,update_hits,update_write_way,update_bank,update_br_idx,update_tag,update_taget_offset(offsetSize-1,0))
        XSDebug(meta_write_valid, "uBTB update: update_taken:%d | old_pred:%b | new_pred:%b\n",
            update_taken, uBTBMeta(update_write_way)(update_bank).pred,
            Mux(!update_hits,
                Mux(update_taken,3.U,0.U),
                satUpdate( uBTBMeta(update_write_way)(update_bank).pred,2,update_taken)))

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