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

    // val uBTBMeta = RegInit((0.U).asTypeOf(Vec(nWays, Vec(PredictWidth, new MicroBTBMeta))))
    // val uBTB = Reg(Vec(nWays, Vec(PredictWidth, new MicroBTBEntry)))

    // class UBTBMem[T <: Data](gen: T, nWays: Int) extends XSModule {
    //     class UBTBBundleR[T <: Data](private val gen: T, val way: Int) extends Bundle {
    //         val data = Output(Vec(way, gen))
    //     }
    //     class UBTBReadBus[T <: Data](private val gen: T, val way: Int) {
    //         val resp = Output(new UBTBBundleR(gen, way))
    //     }
    //     class UBTBWriteBus[T <: Data](private val gen: T, val set: Int, val way: Int) extends Bundle {
    //         val req = 
    //     }
    //     val io = IO(new Bundle {
    //         val wen = Input(Bool())
    //         val wWay = Input(UInt(log2Up(nWays).W))
    //         val wRow = Input(UInt(log2Up(PredictWidth).W))
    //         val wdata = Input(new T)
    //         val entries = Output(Vec(nWays, Vec(PredictWidth, gen)))
    //     })
    //     val mem = RegInit((0.U).asTypeOf(Vec(nWays, Vec(PredictWidth, new T))))
    //     io.entries := mem
    //     when (io.wen) {
    //         mem(wWay)(wRow) := wdata
    //     }
    // }

    class MetaOutput extends XSBundle {
        val is_Br = Bool()
        val is_RVC = Bool()
        val pred = UInt(2.W)
    }

    class UBTBMetaBank(nWays: Int) extends XSModule {
        val io = IO(new Bundle {
            val wen = Input(Bool())
            val wWay = Input(UInt(log2Up(nWays).W))
            val wdata = Input(new MicroBTBMeta)
            val rtag = Input(UInt(tagSize.W))
            val rdata = Output(new MetaOutput)
            val hit_ohs = Output(Vec(nWays, Bool()))
            val allocatable_way = Valid(UInt(log2Up(nWays).W))
            val rWay = Input(UInt(log2Up(nWays).W))
            val rpred = Output(UInt(2.W))
        })
        val mem = Mem(nWays, new MicroBTBMeta)
        val rentries = VecInit((0 until nWays) map (i => mem(i)))
        val hit_ohs = VecInit(rentries map (e => e.valid && e.tag === io.rtag))
        val hit_way = PriorityEncoder(hit_ohs)
        val hit_entry = rentries(hit_way)
        io.hit_ohs := hit_ohs
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

    class UBTBDataBank(nWays: Int) extends XSModule {
        val io = IO(new Bundle {
            val wen = Input(Bool())
            val wWay = Input(UInt(log2Up(nWays).W))
            val wdata = Input(new MicroBTBEntry)
            val rWay = Input(UInt(log2Up(nWays).W))
            val rdata = Output(new MicroBTBEntry)
        })
        val mem = Mem(nWays, new MicroBTBEntry)
        val rentries = VecInit((0 until nWays) map (i => mem(i)))
        io.rdata := rentries(io.rWay)
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
    when (reset_way === nWays.U) { do_reset := false.B }

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
    
    (0 until PredictWidth).map{ b => metas(b).rtag := Mux(isInNextRow(b),read_req_tag + 1.U,read_req_tag) }
    val read_hit_ohs = read_bank_inOrder.map{ b => metas(b).hit_ohs }
    val read_hit_vec = VecInit(read_hit_ohs.map{oh => ParallelOR(oh).asBool})
    val read_hit_ways = VecInit(read_hit_ohs.map{oh => PriorityEncoder(oh)})
    // val read_hit =  ParallelOR(read_hit_vec).asBool
    // val read_hit_way = PriorityEncoder(ParallelOR(read_hit_ohs.map(_.asUInt)))
    

    (0 until PredictWidth).map(b => datas(b).rWay := read_hit_ways((b.U + PredictWidth.U - read_req_basebank)(log2Up(PredictWidth)-1, 0)))

    val  uBTBMeta_resp = VecInit((0 until PredictWidth).map(b => metas(read_bank_inOrder(b)).rdata))
    val  btb_resp = VecInit((0 until PredictWidth).map(b => datas(read_bank_inOrder(b)).rdata))  

    for(i <- 0 until PredictWidth){
        // do not need to decide whether to produce results\
        read_resp(i).valid := read_hit_vec(i) && io.inMask(i)
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

    // val alloc_ways = read_bank_inOrder.map{ b => 
    //     alloc_way(VecInit(uBTBMeta.map(w => w(b).valid)).asUInt,
    //               VecInit(uBTBMeta.map(w => w(b).tag)).asUInt,
    //               Mux(isInNextRow(b).asBool,read_req_tag + 1.U,read_req_tag))
        
    // }

    val alloc_ways = read_bank_inOrder.map{ b => 
        Mux(metas(b).allocatable_way.valid, metas(b).allocatable_way.bits, LFSR64()(log2Ceil(nWays)-1,0))}
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
    // when(entry_write_valid)
    // {
    //     uBTB(update_write_way)(update_bank).offset := update_taget_offset
    // }
    for (b <- 0 until PredictWidth) {
        datas(b).wen := do_reset || (entry_write_valid && b.U === update_bank)
        datas(b).wWay := Mux(do_reset, reset_way, update_write_way)
        datas(b).wdata := Mux(do_reset, 0.U.asTypeOf(new MicroBTBEntry), update_taget_offset.asTypeOf(new MicroBTBEntry))
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
            update_taken, metas(update_bank).rpred,
            Mux(!update_hits,
                    Mux(update_taken,3.U,0.U),
                    satUpdate( metas(update_bank).rpred,2,update_taken)
                ))

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