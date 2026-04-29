
package xiangshan.cache.mmu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import utility._
import coupledL2.utils.SplittedSRAM
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._
import xiangshan.backend.fu.{PMPReqBundle, PMPRespBundle}
import cats.instances.boolean


 
class mptReqBundle(implicit p: Parameters) extends XSBundle with MPTCacheParam {//mpt io interface req and resp , id is not used in ptw 
    val reqPA = UInt(ppnLen.W)
    val id = UInt(mptSourceWidth.W)
    val mptOnly = Bool()//1bit control logic for L2TLB
}



class mptTlbRespBundle (implicit p: Parameters) extends XSBundle with MPTCacheParam {//L2TB return info to L1
    val mptOnly = Bool()//1bit control logic for L2TLB
    val accessFault = Bool()
    val mptPerm = UInt(3.W)
    val mptLevel =  UInt(mptLevelLenUInt.W)//UInt level 
    val contigousPerm = Bool()//only work for non H l0 pte
    //indicate continous 8 permission. can not compress as l0pte(8bit valididx) 
    val permIsNAPOT = Bool()
    def genFakeResp():Unit={
        this.accessFault:= false.B
        this.mptPerm:= Fill(3,1.U(1.W))
        this.contigousPerm:= true.B
        this.mptLevel :=  3.U
        this.permIsNAPOT := true.B
    }
    def applyMptc2TlbResp(childBundle : mptRespBundle):Unit ={
        this.accessFault:= childBundle.accessFault
        this.mptPerm:= childBundle.mptPerm
        this.contigousPerm:= childBundle.contigousPerm
        this.mptLevel := childBundle.mptLevel
        this.permIsNAPOT := childBundle.permIsNAPOT 
        this.mptOnly := childBundle.mptOnly 
    }
}
class mptRespBundle(implicit p: Parameters) extends mptTlbRespBundle with MPTCacheParam {//mptc 
    val id = UInt(mptSourceWidth.W)
    val reqPA = UInt(ppnLen.W)// in req to out req 
}

// class SRAMTemplateMPT[T <: Data]
// (
//   gen: T, set: Int, way: Int = 1,
//   shouldReset: Boolean = false, holdRead: Boolean = false,
//   singlePort: Boolean = true, bypassWrite: Boolean = false,
//   clkDivBy2: Boolean = false, readMCP2: Boolean = true
// ) extends Module {
//   val io = IO(new Bundle {
//     val r = Flipped(new SRAMReadBus(gen, set, way))
//     val w = Flipped(new SRAMWriteBus(gen, set, way))
//   })

//   val wordType = UInt(gen.getWidth.W)
//   val array = SyncReadMem(set, Vec(way, wordType))

//   val (ren, wen) = (io.r.req.valid, io.w.req.valid)
//   val realRen = ren  && !wen// delay 1 gate

//   val setIdx =  io.w.req.bits.setIdx
//   val wdata = VecInit((io.w.req.bits.data).map(_.asTypeOf(wordType)))
//   val waymask =  io.w.req.bits.waymask.getOrElse("b1".U)
//   when (wen) { array.write(setIdx, wdata, waymask.asBools)}

//   val raw_rdata = array.read(io.r.req.bits.setIdx, realRen)// 

//   val mem_rdata = raw_rdata
//   val rdata = mem_rdata.map(_.asTypeOf(gen))

//   io.r.resp.data := VecInit(rdata)
//   io.r.req.ready := !wen 
//   io.w.req.ready := true.B

// }


class MptOutputSwitchBoxIO(implicit p: Parameters) extends MMUIOBaseBundle with MPTCacheParam {
    val MergeArbValid = Input(Bool())
    val MergeArbReady = Output(Bool())
    val MergeArbFault = Input(Bool())
    
    val MptInValid = Output(Bool())
    val MptInReady = Input(Bool())

    val MptOutValid = Input(Bool())
    val MptOutData = Input(new mptRespBundle())

    val L1TLBReady = Input(Bool())
    val L1TLBValid = Output(Bool()) 
    val L1TLBOutData = Output(new mptTlbRespBundle())    
    val L1TLBOutMptOnly = Output(Bool())
    val L1TLBReqPA  = Output(UInt(ppnLen.W))
}
class MptOutputSwitchBox (implicit p: Parameters)  extends XSModule with MPTCacheParam {
    // All L2TLB address translations must be combined with the MPT response before returning. 
    // Therefore, a small state machine is required to demultiplex control signals. 
    // Additionally, the MPT output data needs to be stored in a register.
    
    val io = IO(new  MptOutputSwitchBoxIO())

    val MptOutDataWire = Wire(new mptTlbRespBundle())
    MptOutDataWire.applyMptc2TlbResp(io.MptOutData)

    val MptOutDataReg = Reg (new mptTlbRespBundle())
    val MptOutMptOnly = RegInit(false.B)
    val MptOutReqPA =  Reg(UInt(ppnLen.W))
    
    val flush  = io.sfence.valid || io.csr.satp.changed || io.csr.vsatp.changed || io.csr.hgatp.changed|| io.csr.priv.virt_changed || (if(HasMptCheck) (io.csr.mmpt.changed) else false.B)
    
    when(io.MptOutValid){
        MptOutDataReg :=  MptOutDataWire 
        MptOutMptOnly := io.MptOutData.mptOnly 
        MptOutReqPA := io.MptOutData.reqPA 
    }
    when(io.MergeArbFault && io.MergeArbValid) {
         MptOutMptOnly := false.B
    }


    io.L1TLBOutData := Mux(io.MptOutValid, MptOutDataWire ,MptOutDataReg)
    io.L1TLBOutMptOnly := Mux(io.MptOutValid,io.MptOutData.mptOnly ,MptOutMptOnly) && !(io.MergeArbValid && io.MergeArbFault)
    io.L1TLBReqPA:= Mux(io.MptOutValid,io.MptOutData.reqPA ,MptOutReqPA)

    object MySwitchState extends ChiselEnum {
            val sIDLE , sSEND_MPT, sSEND_L1TLB = Value
        }
    import MySwitchState._
    val curState = RegInit(sIDLE)
    val nextState = WireDefault(sIDLE)
    when(flush){
        curState := sIDLE
    }.otherwise{
        curState:= nextState// 2 proc FSM 
    }
    //fsm start
    io.MergeArbReady := false.B
    io.MptInValid := false.B
    io.L1TLBValid := false.B 
    nextState := curState
    switch(curState) {
        is(sIDLE) {
            when(io.MergeArbValid) {
                when(io.MergeArbFault){
                    io.L1TLBValid := true.B
                    when(io.L1TLBReady) {
                        io.MergeArbReady:= true.B
                        nextState := sIDLE
                    }.otherwise{
                        nextState := sSEND_L1TLB    
                    }
                }.otherwise{
                    io.MptInValid := true.B
                    when(io.MptInReady){
                        nextState := sSEND_MPT
                    }
                }
            }
            when(io.MptOutValid && io.MptOutData.mptOnly) {// mpt valid without merge arb first implies that it is mpt only request.try change later
                io.L1TLBValid := true.B
                when(io.L1TLBReady) {
                    io.MergeArbReady:= true.B
                    nextState := sIDLE
                }.otherwise{
                    nextState := sSEND_L1TLB    
                }
            }
        }

        is(sSEND_MPT) {//delay+1+mptclk
            io.MptInValid := false.B
            when(io.MptOutValid) {
                io.L1TLBValid := true.B
                when(io.L1TLBReady) {
                    io.MergeArbReady:= true.B
                    nextState := sIDLE
                }.otherwise{
                    nextState := sSEND_L1TLB    
                }
            }
        }

        is(sSEND_L1TLB) {
            io.L1TLBValid := true.B
            when(io.L1TLBReady) {
                io.MergeArbReady:= true.B
                nextState := sIDLE
            }
        }
    }
    //fsm end    
}

class PLRUOH(log_ways: Int, is_top : Boolean = true) extends Module {
        val wayNum = 1<< log_ways
        val io = IO(new Bundle {
        val  access = Flipped(ValidIO(UInt(wayNum.W)))
        val replace = Output(UInt(wayNum.W))
        val upperCom = Option.when(!is_top)(Output(Bool()))
    }) 
    if(log_ways==0){   
        io.replace:= 1.U//OH 1 is b0
        
    } else if( log_ways==1){//delay 1 gate 
        val changed = io.access.bits(1) || io.access.bits(0) 
        // 01 will let state points to right 10 to left entry, 00 will disable state input,if input freezes, i.e. same access value with valid for more than 1 clk, the state will not change,great
        if(!is_top){io.upperCom.get := changed }
        val state= RegEnable( io.access.bits(0), false.B,io.access.valid && changed) // OH last bit indicates the next state 01 state 1, 10 state 0 
        io.replace:= Cat(state, ~state) //replace state 1 : 10, state 0 01 opposite of the direction iof input
    } else {
        val top = wayNum
        val mid = 1<<(log_ways-1)
        val plruleft = Module(new PLRUOH(log_ways-1,false))//gen left and right entry
        plruleft.io.access.bits := io.access.bits(top-1, mid)
        plruleft.io.access.valid := io.access.valid

        val plruright = Module(new PLRUOH(log_ways-1,false))
        plruright.io.access.bits := io.access.bits(mid-1, 0)
        plruright.io.access.valid := io.access.valid

        val changed = plruleft.io.upperCom.get || plruright.io.upperCom.get
        val state= RegEnable( plruright.io.upperCom.get , false.B,io.access.valid && changed) // OH last bit indicates the next state 01 state 1, 10 state 0 
        if(!is_top){io.upperCom.get := changed}
        val leftreplace = (Fill(plruleft.wayNum,state) & plruleft.io.replace)
        val rightreplace =  (Fill(plruleft.wayNum,!state) & plruright.io.replace)//replace state 1 : 10, state 0 01
        io.replace:= Cat(leftreplace, rightreplace) 
    }
}
class PLRUOHSet(sets_log2: Int, log_ways: Int) extends Module  {
    val wayNum =1<< log_ways
    val setNum =1<< sets_log2 
    val io = IO(new Bundle {
    val  access = Flipped(ValidIO(UInt(wayNum.W)))
    val replace = Output(UInt(wayNum.W))
    val idx = Input(UInt(sets_log2.W))
    })

    val plruSet = Array.fill(setNum)(Module(new PLRUOH(log_ways)).io)
    val outputArray = Wire(Vec(setNum,UInt(wayNum.W)))
    val hitArray = Wire(Vec(setNum,Bool()))
    
    for (i <- 0 until setNum) {
      val Idxhit = (i.U === io.idx)
      hitArray(i) := Idxhit
      plruSet(i).access.bits := io.access.bits
      plruSet(i).access.valid := io.access.valid & Idxhit 
      outputArray(i):= plruSet(i).replace 
    }
    io.replace := Mux1H(hitArray,outputArray)     //better readablity, select replace based on hit idx   

    //select plru with idx, is this a demux? ans: it is
    //outputArray(i):= (Fill(setNum,Idxhit) & plruSet(i).replace)}// a switch to 0 or repalce based on idxhit
    // io.replace:= outputArray.reduce(_|_) //maybe not ideal,better self decide what kind of logic is used here                              
    // outputArray(i):=  plruSet(i).replace}
    //io.replace:= outputArray(io.idx)
} 


class mptData (implicit p: Parameters) extends XSBundle with MPTCacheParam {
    val data =  UInt(perms16Len.W) 
    def apply(data:UInt) :Unit={
        this.data:= data//zero extended 
    }
    def getPPN : UInt ={//get PPN
        this.data(ppnLen-1,0)
    }
    def getAddr(offset: UInt):UInt={
        Cat(this.getPPN,Cat(offset,0.U(3.W)))//2|3 byte =64bit
    } 
    def extractPerm(select: UInt): (UInt) = {//extract XWR using 4bit offset
    // cal start end and extract
        (this.data>>(select*3.U))(2,0)// not quite sure what kind of crap will be synthesized. I meant it to be a binary mux
    }
}

class mptEntry (implicit p: Parameters) extends XSBundle with MPTCacheParam {
    val N = Bool()
    val data = new mptData()
    val L= Bool()
    val V= Bool()
    def apply(sMEMResp:UInt):Unit={
        this.V:= (sMEMResp(0)===1.U)
        this.L:= (sMEMResp(1)===1.U)
        this.N:= (sMEMResp(63)===1.U)
        this.data.apply(sMEMResp(57,10))//xiangshan only support 48bit PA, so PPN only needs 36
    }
    def isValid:Bool={
        this.V
    }
    
    def isLeaf:Bool={
        this.L
    }

    def getAddr(offset: UInt):UInt={
        this.data.getAddr(offset)
 
    }

    def genFake(level : UInt):Unit={
        this.N := false.B
        this.data.data  := "h802F4".U
        this.L := false.B
        this.V := true.B
        switch(level) {
            is("b1000".U) {
                this.N := false.B
                this.data.data  := "h802F4".U
                this.L := false.B
                this.V := true.B
            }
            is("b0100".U) {
                this.N := false.B
                this.data.data  :="h802F4".U
                this.L := false.B
                this.V := true.B
            }
            // 你可以继续添加更多的 case
            is("b0010".U) {
                if(HasMptCheckDefault4k) {
                    this.N := false.B
                    this.data.data  := "h802F4".U
                    this.L := false.B
                    this.V := true.B
                } else {
                    this.N := false.B
                    this.data.data  := "hFFFFFFFFFFFF".U
                    this.L := true.B
                    this.V := true.B
                }
            }
 
            is("b0001".U) {
                this.N := false.B
                this.data.data  := "hFFFFFFFFFFFF".U
                this.L := true.B
                this.V :=  true.B
            }
        }
    }    
}


class mptCacheTag ( tagLen: Int, isSp:Boolean = false) (implicit p: Parameters) extends XSBundle with MPTCacheParam {//<= , isL0:Boolean = false
    //val sdid = if(SDID_cache_store_en) Some(UInt(SDIDLen.W)) else None //6.W 没用
    //if(!isL0){val valid = Bool()}
    val tag = UInt(tagLen.W)
    val level = Option.when(isSp)(UInt((mptLevelLenOH-1).W))//sp can not be l0   
    // val valid = Bool()
    def hit(ppn: UInt ): Bool={
        tag === ppn(ppnLen-1,ppnLen-tagLen)//tag =5, (47,43)
    }
    def hitSp (ppn: UInt ): Bool ={
        val hitL3 = (this.tag(tagLen-1,tagLen-mptL3TagLen) === ppn(ppnLen-1,ppnLen-mptL3TagLen))//tag =5, (47,43)
        val hitL2 = (this.tag(tagLen-1,tagLen-mptL2TagLen)  === ppn(ppnLen-1,ppnLen-mptL2TagLen))
        val hitL1 = (this.tag === ppn(ppnLen-1,ppnLen-tagLen)) 
        val hotVal = Mux1H( Seq(
            this.level.get(2)->hitL3,
            this.level.get(1)->hitL2, 
            this.level.get(0)->hitL1))//it is a tuple scala> 1 -> 2 res0: (Int, Int) = (1,2)
        hotVal 
    }
}
class mptCacheData( isPerms: Boolean= false)  (implicit p: Parameters) extends XSBundle with MPTCacheParam {
    val data = if(isPerms) UInt(perms16Len.W)  else UInt(ppnLen.W) //36.W
    def extractPerm(select: UInt): (UInt) = {//extract XWR using 4bit offset
    // cal start end and extract
    require(isPerms ,"extractPerm is only valid when isPerms is true")
    (this.data>>(select*3.U))(2,0)// not quite sure what kind of crap will be synthesized. I meant it to be a binary mux
    }
}

class mptCacheL0(implicit p: Parameters)  extends XSBundle with MPTCacheParam {
    val cacheData   = new mptCacheData(isPerms=true)
    val tag         = new mptCacheTag(tagLen = mptL0TagLen)//,isL0 = true )
}

class mptCacheReq (implicit p: Parameters) extends XSBundle with MPTCacheParam {
    val mptOnly = Bool()
    val reqPA= UInt(ppnLen.W)
    val source = UInt(mptSourceWidth.W)
}


//垃圾pipe没有复位信号导致我一定要自定义一个pipe才行
class MPTPipe (implicit p: Parameters) extends mptCacheReq { 
    val dataValid= Bool() 
    val flushCache =Bool()

    def applySplitData(MPTPipeControl:MPTPipeControl, MPTPipeData: MPTPipeData): Unit = {
        this.dataValid := MPTPipeControl.dataValid
        this.flushCache := MPTPipeControl.flushCache
        this.mptOnly := MPTPipeControl.mptOnly
        this.reqPA := MPTPipeData.reqPA
        this.source := MPTPipeData.source
    }

    def createSplitData():(MPTPipeControl, MPTPipeData) = {
        val mptPipeControl = Wire(new MPTPipeControl) //evil defination of hardware type :-(
        val mptPipeData = Wire(new MPTPipeData)
        mptPipeData.reqPA := this.reqPA
        mptPipeData.source := this.source
        
        mptPipeControl.dataValid := this.dataValid
        mptPipeControl.flushCache := this.flushCache
        mptPipeControl.mptOnly := this.mptOnly
        (mptPipeControl,mptPipeData)
    }
}  

class MPTPipeControl (implicit p: Parameters) extends XSBundle with MPTCacheParam {
    val dataValid= Bool() 
    val flushCache =Bool()
    val mptOnly = Bool()
    def applyPipeData(mptPipe: MPTPipe): Unit = {
        this.dataValid := mptPipe.dataValid
        this.flushCache := mptPipe.flushCache
        this.mptOnly := mptPipe.mptOnly
    }
}  

class MPTPipeData (implicit p: Parameters) extends XSBundle with MPTCacheParam {
    val reqPA= UInt(ppnLen.W)
    val source = UInt(mptSourceWidth.W)
    def applyPipeData(MPTPipe: MPTPipe): Unit = {
        this.reqPA := MPTPipe.reqPA
        this.source := MPTPipe.source
    }
}  

object MPTPipeWithReset {
   def apply(enqValid: Bool, enqBits: MPTPipe, latency: Int): Valid[MPTPipe] = {
    require(latency >= 0, "Pipe latency must be greater than or equal to zero!")

    if (latency == 0) {
      val out = Wire(Valid(chiselTypeOf(enqBits)))
      out.valid := enqValid
      out.bits := enqBits
      out
    } else{ 
        val v = RegNext(enqValid, false.B) //valid has reset 
        
        val (mptPipeControlIn,mptPipeDataIn) = enqBits.createSplitData()//split input as data and control 
        val mptPipeData     = RegEnable(mptPipeDataIn, (mptPipeControlIn.dataValid||mptPipeControlIn.flushCache) && enqValid) //data has no reset 
        val mptPipeControl  = RegEnable(mptPipeControlIn,0.U.asTypeOf(mptPipeControlIn), enqValid)//control has reset 
        val b               = Wire(chiselTypeOf(enqBits))
        b.applySplitData(mptPipeControl,mptPipeData) //merge pipe control and data signal 
        apply(v, b, latency - 1)
      }
  }
}

//垃圾pipe end


class refillBundle(implicit p: Parameters)  extends XSBundle with MPTCacheParam {
        val level = UInt(mptLevelLenOH.W)
        val PA= UInt((PAddrBits- MptOff).W)
        val refillData = new mptData()
        val isAf = Bool()
        val isLeafMpte =Bool() // is leaf? decide what cache is refilled
    }
class MPTCacheIO(implicit p: Parameters) extends MMUIOBaseBundle with MPTCacheParam{
    val req = Flipped(DecoupledIO(new mptCacheReq()))

    val respHit = ValidIO(new Bundle {// source is waiting for cache to resp
        val accessFault= Bool()
        val perm = UInt(3.W)
        val tlbContigousPerm = Bool() 
        val permIsNAPOT = Bool()
        val source= UInt(mptSourceWidth.W)
        val mptLevel = UInt(log2Up(mptLevelLenOH).W)
        val mptOnly = Bool()
        val reqPA= UInt(ppnLen.W)
    })

    val respMiss = DecoupledIO(new Bundle {
        val hitLevel=UInt((mptLevelLenOH).W)
        val ppn = UInt(ppnLen.W)//minsize is 4k,dont need 12bits offset
        val source= UInt(mptSourceWidth.W)
        val PA= UInt((ppnLen).W)
        val mptOnly = Bool()
    })
    
   val refill = Flipped(ValidIO(new refillBundle()))
    
}

class MPTCache (implicit p: Parameters) extends XSModule with MPTCacheParam {
    val io = IO(new MPTCacheIO)
    ////mfence signal
    val mfenceActive = WireInit(false.B)
    val fencePA= WireInit(false.B)
    val mfencevalid= io.sfence.valid && io.sfence.bits.mfence.get
    // This MPT design supports partial cache flushing by PA. When flushing, in addition to leaf nodes, intermediate nodes are also invalidated 
    switch(Cat(io.sfence.bits.rs2, io.sfence.bits.rs1).asUInt){ 
        is("b11".U) {
            fencePA:= (io.sfence.bits.id===io.csr.mmpt.sdid) && mfencevalid// delay of about 10 gates
        }
        is("b01".U){
            fencePA:= mfencevalid
        }
        is("b10".U){
            mfenceActive:= (io.sfence.bits.id===io.csr.mmpt.sdid) && mfencevalid
        }
        is("b00".U){
            mfenceActive:= mfencevalid
        }
    }
    val flushAll = mfenceActive || io.csr.mmpt.changed
    val mptFlushReset = (reset.asBool || flushAll).asAsyncReset//wehen csr change or mfence flush
    withReset (mptFlushReset){// flush according to fence
        val pipeFlowEn= Wire(Bool())

        val refilling = Wire(Bool())
        val refillCounter= RegInit(0.U(4.W))
        when(io.refill.valid) {//&& ! io.refill.bits.isAf) {//refill valid and not access fault
            refillCounter:="b1000".U
        }.elsewhen(io.sfence.valid) {
            refillCounter:=0.U
        }.otherwise{
            refillCounter:=refillCounter>>1.U
        }

   
        refilling := refillCounter > 0.U// is refiill state when counter != 0 for 4 rounds
        val respHitRegTmp = Wire(Bool())//resphitreg is defined later
        pipeFlowEn:= (io.respMiss.ready || respHitRegTmp ) ||refilling ||fencePA //& (!(io.refill.valid && ! io.refill.bits.isAf)))
        // Without respHitReg, if the final stage hits and stalls, the hit control signal will get stuck at a high level, causing the entire MMU to fail.
        // Switch the pipeline input based on whether it is an mfence-PA operation or a refill operation.
        val PAfenceInputs = Wire(new MPTPipe)
        PAfenceInputs.reqPA := io.sfence.bits.addr(47,12)
        PAfenceInputs.source := io.req.bits.source 
        PAfenceInputs.dataValid := false.B   
        PAfenceInputs.flushCache := true.B
        PAfenceInputs.mptOnly := false.B

        val ioInputs= Wire(new MPTPipe)
        ioInputs.reqPA := io.req.bits.reqPA
        ioInputs.source := io.req.bits.source        
        ioInputs.dataValid := io.req.fire
        ioInputs.flushCache := false.B
        ioInputs.mptOnly:= io.req.bits.mptOnly

        
        val pipeInputs = Wire(new MPTPipe)
 

        val stageReq = MPTPipeWithReset(pipeFlowEn, pipeInputs,1)  
        val stageDelayin = stageReq.bits
        val stageDelay= MPTPipeWithReset(pipeFlowEn, stageDelayin,1)
        val stageCheckin = stageDelay.bits     
        val stageCheck= MPTPipeWithReset(pipeFlowEn, stageCheckin,1)
        val stageRespin = stageCheck.bits
        val stageResp= MPTPipeWithReset(pipeFlowEn,stageRespin,1)

        //priority
        //1. fence PA  
        //2. refill and last stage valid
        //3. normal request
 
        pipeInputs := Mux(fencePA, PAfenceInputs, Mux(refilling && stageResp.bits.dataValid,stageResp.bits, ioInputs))
        when(io.sfence.valid){
 
            pipeInputs.dataValid := false.B
            //pipeInputs.flushCache := false.B
            pipeInputs.mptOnly := false.B
 
            stageDelayin.dataValid := false.B
            //stageDelayin.flushCache := false.B
            stageDelayin.mptOnly := false.B

            stageCheckin.dataValid := false.B
            //stageCheckin.flushCache := false.B
            stageCheckin.mptOnly := false.B

            stageRespin.dataValid := false.B
            //stageRespin.flushCache := false.B
            stageRespin.mptOnly := false.B
        }
        //val ready = RegInit(true.B)
        io.req.ready := ((io.respMiss.ready && !refilling) || (refilling && !stageResp.bits.dataValid))  && !fencePA //&& !(io.refill.valid && ! io.refill.bits.isAf) //blocking
        //init cache tag
        val l3Tag = Reg(Vec(l3Size, new mptCacheTag(tagLen = mptL3TagLen)))
        val l3Valid = RegInit(Vec(l3Size, Bool()) , 0.U.asTypeOf(Vec(l3Size, Bool())) )

        val l2Tag = Reg(Vec(l2Size, new mptCacheTag(tagLen = mptL2TagLen)))
        val l2Valid = RegInit(Vec(l2Size, Bool()), 0.U.asTypeOf(Vec(l2Size, Bool())))
        
        val l1Tag = Reg(Vec(l1Size,  new mptCacheTag(tagLen = mptL1TagLen))) 
        val l1Valid = RegInit(Vec(l1Size, Bool()), 0.U.asTypeOf(Vec(l1Size, Bool())))

        val spTag = Reg(Vec(spSize, new mptCacheTag(tagLen = mptspTagLen,isSp = true)))
        val spValid = RegInit(Vec(spSize, Bool()), 0.U.asTypeOf(Vec(spSize, Bool())))

        val l3Data = Reg(Vec(l3Size, new mptCacheData()))
        val l2Data = Reg(Vec(l2Size, new mptCacheData()))
        val l1Data = Reg(Vec(l1Size,  new mptCacheData())) 
        val spData = Reg(Vec(spSize, new mptCacheData(isPerms = true)))  
        val l0Data =  Module(new SplittedSRAM(
            new mptCacheL0(),
            set =  l0nSets, way = l0nWays,
            setSplit = 1,
            waySplit = 2,
            dataSplit = 1,
            singlePort = sramSinglePort,
            readMCP2 = false,
            hasMbist = hasMbist,
            hasSramCtl = hasSramCtl
        ))//1clk delay from req to resp Module(new SRAMTemplateMPT(new mptCacheL0(),set =  l0nSets, way = l0nWays))
        val l0Valid = RegInit(Vec(l0nSets,Vec(l0nWays, Bool())), 0.U.asTypeOf(Vec(l0nSets,Vec(l0nWays, Bool()))))

        val mptCacheL3Replace = Module(new PLRUOH(log_ways = log2Up(l3Size))).io
        val mptCacheL2Replace = Module(new PLRUOH(log_ways = log2Up(l2Size))).io
        val mptCacheL1Replace = Module(new PLRUOH(log_ways = log2Up(l1Size))).io
        val mptCacheL0Replace = Module(new PLRUOHSet(sets_log2 = log2Up(l0nSets),log_ways = log2Up(l0nWays))).io
        val mptCacheSpReplace = Module(new PLRUOH(log_ways = log2Up(spSize))).io
        //alloc replacement policy,use PLRU with Onehot in/out

        val (l3hit,l3hitPPN) ={
            val hitVecTemp = l3Tag.zip(l3Valid).map{case(x,v) =>x.hit(stageReq.bits.reqPA) && v}//hit when valid and tag equal stagereq
            when(stageReq.bits.flushCache)  {  // clean fence valid if hit tag
                hitVecTemp.zip(l3Valid).map{case(x,v)=> 
                    when(x){v := false.B}
                }
            }
            val hitVec = RegEnable(VecInit(hitVecTemp), stageReq.bits.dataValid) //ready at stage check, use datavalid instead of stageReq.valid
            //val hitData= ParallelPriorityMux(hitVec zip l3Data)
            val hitData= RegEnable(Mux1H(hitVecTemp,l3Data), stageReq.bits.dataValid)//we can use onehot mux, should be faster.
            val hit=RegEnable(hitVecTemp.reduce(_||_), stageReq.bits.dataValid)// 1 bit hit ,avaliable at stage delay after 2 or gates
            
            mptCacheL3Replace.access.bits:= hitVec.asUInt //assign hitVec to plru to update plru state ,miss(hitVec = h0) will not change the plru state 
            mptCacheL3Replace.access.valid:=  stageDelay.bits.dataValid //ready at stage check
            (hit,hitData)
            
        }

        val (l2hit,l2hitPPN) ={
            val hitVecTemp = l2Tag.zip(l2Valid).map{case(x,v) =>x.hit(stageReq.bits.reqPA) && v}//hit when valid and tag equal stagereq
            when(stageReq.bits.flushCache)  {  // clear fence valid
                hitVecTemp.zip(l2Valid).map{case(x,v)=> 
                    when(x){v:= false.B}
                }
            }
            val hitVec = RegEnable(VecInit(hitVecTemp), stageReq.bits.dataValid) //ready at stage check
            //val hitData= ParallelPriorityMux(hitVec zip l3Data)
            val hitData= RegEnable(Mux1H(hitVecTemp,l2Data), stageReq.bits.dataValid)//we can use onehot mux, should be faster.
            val hit=RegEnable(hitVecTemp.reduce(_||_), stageReq.bits.dataValid)// 1 bit hit ,avaliable at stage delay after 2 or gates
            
            mptCacheL2Replace.access.bits:= hitVec.asUInt //assign hitVec to plru to update plru state ,miss(hitVec = h0) will not change the plru state 
            mptCacheL2Replace.access.valid:=  stageDelay.bits.dataValid //ready at stage check
            (hit,hitData)
        }

        val (l1hit, l1hitPPN) ={
            val hitVecTemp = l1Tag.zip(l1Valid).map{case(x,v) =>x.hit(stageReq.bits.reqPA) && v}//hit when valid and tag equal stagereq
            when(stageReq.bits.flushCache)  {  // clear fence valid
                hitVecTemp.zip(l1Valid).map{case(x,v)=> 
                    when(x){v:= false.B}
                }
            }            
            val hitVec = RegEnable(VecInit(hitVecTemp), stageReq.bits.dataValid) //ready at stage check
            //val hitData= ParallelPriorityMux(hitVec zip l3Data)
            val hitData= RegEnable(Mux1H(hitVecTemp,l1Data), stageReq.bits.dataValid)//we can use onehot mux, should be faster.
            val hit=RegEnable(hitVecTemp.reduce(_||_), stageReq.bits.dataValid)// 1 bit hit ,avaliable at stage delay after 2 or gates
            
            mptCacheL1Replace.access.bits:= hitVec.asUInt //assign hitVec to plru to update plru state ,miss(hitVec = h0) will not change the plru state 
            mptCacheL1Replace.access.valid:=  stageDelay.bits.dataValid //ready at stage check        
            (hit,hitData)

        }
        /////////////////////////// gen addr hit(l3-l1) at stage check
        //val(hitAddrLevelTemp,hitAddrT)=PriorityMux(Seq(l1hit,l2hit,l3hit),Seq(("b001".U,l1hitPPN),("b010".U, l2hitPPN),("b100".U,l3hitPPN))) 官方的PriorityMux 会在Select none 给出h0，但我要其它的default value
        val missLevel = Mux(io.csr.mmpt.mode === 2.U,"b1000".U,"b0100".U)//enablesmmpt52 = true, 0 delay since io.csr.mmpt.mode will not change during cache read
        val hitAddrLevelTemp= Mux(l1hit,"b0001".U,Mux(l2hit,"b0010".U,Mux(l3hit,"b0100".U,missLevel)))
        val hitAddrDataTemp= Mux(l1hit,l1hitPPN.data,Mux(l2hit,l2hitPPN.data,Mux(l3hit,l3hitPPN.data,io.csr.mmpt.ppn(ppnLen-1,0))))
        val hitAddrData= RegEnable(hitAddrDataTemp,stageDelay.bits.dataValid) 
        val hitAddrLevel=RegEnable(hitAddrLevelTemp,stageDelay.bits.dataValid)
        
        /////////////////////////// 
    
        val (l0hit, l0HitPerms,l0PermTlbCompress,l0PermIs64kNAPOT) ={
            val idx = getl0set(pipeInputs.reqPA) //..    
            
            l0Data.io.r.req.bits.apply(setIdx = idx)//.. 0 delay  stagereq reg get valid at the same time
            l0Data.io.r.req.valid:= pipeInputs.dataValid|| pipeInputs.flushCache//read and write at the same time will not cause error, but read is invalid 

            val l0validReg =  RegEnable(l0Valid(getl0set(stageReq.bits.reqPA)), 0.U.asTypeOf(Vec(l0nWays, Bool())) ,stageReq.bits.dataValid|| stageReq.bits.flushCache)
            val dataResp = RegEnable(l0Data.io.r.resp.data, stageReq.bits.dataValid|| stageReq.bits.flushCache)//data avaliable at stage delay
            val setTag = dataResp.map(_.tag)
            val setData = dataResp.map(_.cacheData)//4 entry+tag
            //some wire
            val hitVecTemp = setTag.zip(l0validReg).map{case(x,v)=>x.hit(stageDelay.bits.reqPA) && v}//hit when valid and tag equal
            
            //delay (29 bit===):(1xnor + 5*and), (&& valid):(1 and) total 7
            //MfencePA
            when(stageDelay.bits.flushCache)  {  // clear fence valid
                hitVecTemp.zipWithIndex.map{case(x,i)=> 
                    when(x){l0Valid(getl0set(stageDelay.bits.reqPA))(i) := false.B}
                }
            }
            //
            val hitVec = RegEnable(VecInit(hitVecTemp),stageDelay.bits.dataValid)//valid at stage check
            val hitData= Mux1H(hitVecTemp,setData)//we use onehot mux, should be faster than ParallelPriorityMux. delay:log2(4)*2=4
            val hitDataReg= RegEnable(hitData,stageDelay.bits.dataValid)//valid at stage check, total delay 11 gates
            val hit=RegEnable(hitVecTemp.reduce(_||_),stageDelay.bits.dataValid)// 4-> 1 bit hit 

            val hitPermsTemp= hitDataReg.extractPerm(stageCheck.bits.reqPA(3,0))//always 15:12 delay log2(16)*3=12 gates

            mptCacheL0Replace.access.bits:= hitVec.asUInt //assign hitVec to plru to update plru state ,miss(hitVec = h0) will not change the plru state 
            mptCacheL0Replace.access.valid:=  stageCheck.bits.dataValid // processing at stage check, ready at stage resp
            mptCacheL0Replace.idx:= getl0set(stageCheck.bits.reqPA)

            val PermsAsVec = Wire(Vec(16,UInt(3.W))) //perm xwr bits, total 16 xwrs in one mpte
            for (i <- 0 until 16) {PermsAsVec(i):= hitDataReg.data((2+ i*3), (i*3))}
            val PermsEqual = Wire(Vec(16-1,Bool()))
            for (i <- 0 until 16-1) {PermsEqual(i):= PermsAsVec(i+1) === PermsAsVec(i)}//delay 1XNOR + 2 and gates = 3   
            val low8PermsAllEqual = PermsEqual.slice(0,7).reduce(_&&_) //=PermsEqual(6,0), delay 3
            val high8PermsAllEqual = PermsEqual.slice(8,15).reduce(_&&_) //=PermsEqual(14,8)
            val allEqual = low8PermsAllEqual && high8PermsAllEqual && PermsEqual(7) //Delay 2
            val tlbCompress= Mux( stageCheck.bits.reqPA(3,0) < 8.U,low8PermsAllEqual, high8PermsAllEqual)//delay 3
            
            (hit,RegEnable(hitPermsTemp,stageCheck.bits.dataValid),RegEnable(tlbCompress,stageCheck.bits.dataValid),RegEnable(allEqual,stageCheck.bits.dataValid))//ready at stage resp,hit reaady at stage check
        }
    
        //val (sphit,spHitPerms,spPermIsNAPOT,splevel) = {
        val (sphit,spHitPerms, splevel) = {
            val hitVecTemp = spTag.zip(spValid).map{case(x,v)=>x.hitSp(stageReq.bits.reqPA) && v}//hit when valid and tag equal delay 7 + mux1h delay 4 gates
            when(stageReq.bits.flushCache)  {  // clear fence valid
                hitVecTemp.zip(spValid).map{case(x,v)=> 
                    when(x){v:= false.B}
                }
            }

            val hitVec = RegEnable(VecInit(hitVecTemp), stageReq.bits.dataValid) //ready at stage delay
            ////////
            val levelVec = spTag.map(x=>x.level.get)
            val level = Mux1H(hitVec,levelVec)//ready at stage delay, require cache to be consistent
            val levelReg= RegEnable(level,stageDelay.bits.dataValid)   
            val levelUInt = Wire(UInt(mptLevelLenUInt.W))//4levels len 2 
            levelUInt:= OHToUInt(Cat(levelReg,0.U(1.W)))   

            val extractOffset= Mux1H(   Seq(
            level(2)->stageDelay.bits.reqPA(3+9*3,9*3),
            level(1)->stageDelay.bits.reqPA(3+9*2,9*2),
            level(0)-> stageDelay.bits.reqPA(3+9,9)
            ))

            val extractOffsetReg= RegEnable(extractOffset,stageDelay.bits.dataValid) //ready at stage check

            //val hitData= ParallelPriorityMux(hitVec zip l3Data)
            val hitData = Mux1H(hitVec,spData)//we can use onehot mux, should be faster.
            val hitDataReg = RegEnable(hitData,stageDelay.bits.dataValid)//valid at stage check, total delay 11 gates
            val hitPermsTemp = hitDataReg.extractPerm(extractOffsetReg)//always 15:12 delay log2(16)*3=12 gates
            val hit=RegEnable(hitVec.reduce(_||_), stageDelay.bits.dataValid)// 1 bit hit 
            
            mptCacheSpReplace.access.bits:= hitVec.asUInt //assign hitVec to plru to update plru state ,miss(hitVec = h0) will not change the plru state 
            mptCacheSpReplace.access.valid:=  stageDelay.bits.dataValid //ready at stage check
            
            (hit,RegEnable(hitPermsTemp,stageCheck.bits.dataValid) ,RegEnable(levelUInt,stageCheck.bits.dataValid))//ready at stage resp,hit reaady at stage check

        }
    ///////gen perms hit l0 sp at stage check 

        val hitPerms = sphit || l0hit
        val respHitReg = RegEnable(hitPerms & stageCheck.bits.dataValid ,false.B,pipeFlowEn)//// Data is latched regardless of dataValid. If dataValid is low, the hit signal is also considered invalid. In this case, hitPerms actually holds the result from the previous pipeline stage.

        respHitRegTmp := respHitReg
        io.respHit.valid := respHitReg && !refilling && !fencePA//&& !(io.refill.valid && ! io.refill.bits.isAf)
        val (sphitReg,l0hitReg) = (RegEnable(sphit,pipeFlowEn),RegEnable(l0hit,pipeFlowEn))//(RegEnable(sphit,stageCheck.bits.dataValid),RegEnable(l0hit,stageCheck.bits.dataValid))
        io.respHit.bits.perm := Mux1H(Seq(sphitReg,l0hitReg),Seq(spHitPerms,l0HitPerms)) //1 mux at output, 2 gates, should be fine
        io.respHit.bits.source := stageResp.bits.source //转一圈回去
        io.respHit.bits.mptOnly:= stageResp.bits.mptOnly
        io.respHit.bits.reqPA:= stageResp.bits.reqPA
        io.respHit.bits.tlbContigousPerm:= l0hitReg && l0PermTlbCompress
        //io.respHit.bits.permIsNAPOT:=  Mux1H(Seq(sphitReg,l0hitReg),Seq(spPermIsNAPOT,l0PermIs64kNAPOT)) 
        io.respHit.bits.permIsNAPOT:=  l0hitReg && l0PermIs64kNAPOT 
        io.respHit.bits.accessFault:= (!io.respHit.bits.perm(0)) && io.respHit.bits.perm(1)  //not read but write//false.B//entry in mpt cache is always valid 
        io.respHit.bits.mptLevel:= Mux1H(Seq(sphitReg,l0hitReg),Seq(splevel,0.U(mptLevelLenUInt.W)))//splevel is converted to binary for l1/l2tlb level compare with s1pte and s2pte


        val respMissReg =RegEnable(!hitPerms &  stageCheck.bits.dataValid,false.B,pipeFlowEn)// RegEnable(!hitPerms &  stageCheck.bits.dataValid,false.B,stageCheck.valid)//无论是否datavalid都读入
        io.respMiss.valid := respMissReg && !refilling && !fencePA//&& !(io.refill.valid && ! io.refill.bits.isAf)
        io.respMiss.bits.hitLevel := RegEnable(hitAddrLevel,pipeFlowEn)//RegEnable(hitAddrLevel,stageCheck.bits.dataValid)
        io.respMiss.bits.ppn := RegEnable(hitAddrData,pipeFlowEn)//RegEnable(hitAddrData,stageCheck.bits.dataValid)
        io.respMiss.bits.source := stageResp.bits.source
        io.respMiss.bits.mptOnly:= stageResp.bits.mptOnly
        io.respMiss.bits.PA:= stageResp.bits.reqPA 
        ////read logic end
        ////refill write logic start
        // If a circular pipe is used to resolve refill conflicts, the cache will be accessed twice with the same tag during the loop. For TLRU, this is not an issue as the LRU queue remains unchanged. But what about PLRU? It should probably be fine as well.

        /////////////////// If TLRU is used, when the cache is empty, the LRU pointer should point to an empty entry because empty entries have never been accessed. Therefore, we can normally use the entry pointed to by the LRU for refilling.
        // If PLRU is used, when the cache is empty, accessing neighbors of an empty entry may cause the LRU to stop pointing to that empty entry. For example, with a 4-way PLRU sequence ABACAD, B would be replaced by D, while the empty entry next to A would not be written to.
        // We could fill the cache from top to bottom regardless of the PLRU state when it is not full.
        // However, due to tight timing constraints, let's try using only PLRU first to see how much waste/inefficiency it causes.

        val l3RefillEn = io.refill.bits.level(3).asBool & (!io.refill.bits.isLeafMpte) &(io.refill.valid && ! io.refill.bits.isAf)
        val rfl3Tag = io.refill.bits.PA(PAddrBits- MptOff-1, PAddrBits- MptOff- mptL3TagLen)
        val l3VictimWay = mptCacheL3Replace.replace// ready after idx is set , OH encoding

        val l2RefillEn = io.refill.bits.level(2).asBool & (!io.refill.bits.isLeafMpte)&(io.refill.valid && ! io.refill.bits.isAf)
        val rfl2Tag = io.refill.bits.PA(PAddrBits- MptOff-1, PAddrBits- MptOff- mptL2TagLen)
        val l2VictimWay = mptCacheL2Replace.replace// ready after idx is set , OH encoding

        val l1RefillEn = io.refill.bits.level(1).asBool & (!io.refill.bits.isLeafMpte)&(io.refill.valid && ! io.refill.bits.isAf)
        val rfl1Tag = io.refill.bits.PA(PAddrBits- MptOff-1, PAddrBits- MptOff- mptL1TagLen)
        val l1VictimWay = mptCacheL1Replace.replace// ready after idx is set , OH encoding

        val l0RefillEn = io.refill.bits.level(0).asBool & (io.refill.bits.isLeafMpte) &(io.refill.valid && ! io.refill.bits.isAf)
        val rfl0Tag = io.refill.bits.PA(PAddrBits- MptOff-1, PAddrBits- MptOff- mptL0TagLen)
        val rfl0SetIdx = io.refill.bits.PA(PAddrBits- MptOff- mptL0TagLen-1, 0)
        val l0VictimWay = mptCacheL0Replace.replace// ready after idx is set , OH encoding


        val spRefillEn = (!io.refill.bits.level(0).asBool) & io.refill.bits.isLeafMpte &(io.refill.valid && ! io.refill.bits.isAf)
        val rfspTag = io.refill.bits.PA(PAddrBits- MptOff-1, PAddrBits- MptOff- mptspTagLen)
        val spVictimWay = mptCacheSpReplace.replace // 

        ///write data into cache 1 cycle 
        val l0Wdata = Wire(new mptCacheL0()) // 0 delay ,wire signal assignment
        l0Wdata.tag.tag := rfl0Tag
        l0Wdata.cacheData.data := io.refill.bits.refillData.data  
        l0Data.io.w.req <> DontCare //default val for write channel is invalid 
        l0Data.io.w.req.valid := false.B
        when (l0RefillEn) {
            l0Data.io.w.apply(
            valid = true.B, //valid when refill
            setIdx = rfl0SetIdx,
            data = l0Wdata,
            waymask = l0VictimWay
            )
            for (j <- 0 until l0nSets) { 
                for (i <- 0 until l0nWays) {
                    when(l0VictimWay(i)===1.U && rfl0SetIdx===j.U){
                        l0Valid(j)(i):= true.B
                    }
                }
            }
            
            mptCacheL0Replace.idx:= rfl0SetIdx // Overwrites the assignment from the pipeline; this behavior is not immediately obvious. Consider extracting this signal separately for clarity.
            // During refill, switch the PLRU input to the refill data and update immediately. 
            // Timing breakdown: 4-way update delay (2 gates), 32-set index switching (3 gates), l0 refillEn (1 gate), mux for switching refillEn input (3 gates).
            mptCacheL0Replace.access.bits:= l0VictimWay
            mptCacheL0Replace.access.valid:= true.B
        }

        when (l3RefillEn) {
            for (i <- 0 until l3Size) {
                when(l3VictimWay(i)===1.U){
                    l3Tag(i).tag := rfl3Tag 
                    l3Valid(i) := true.B
                    l3Data(i).data:= io.refill.bits.refillData.getPPN 
                }
            }
            mptCacheL3Replace.access.bits:= l3VictimWay
            mptCacheL3Replace.access.valid:= true.B
        }

        when (l2RefillEn) {
            for (i <- 0 until l2Size) {
                when(l2VictimWay(i)===1.U){
                    l2Tag(i).tag := rfl2Tag 
                    l2Valid(i) := true.B
                    l2Data(i).data:= io.refill.bits.refillData.getPPN 
                }
            }
            mptCacheL2Replace.access.bits:= l2VictimWay
            mptCacheL2Replace.access.valid:= true.B
        }
        when (l1RefillEn) {
             for (i <- 0 until l1Size) {
                when(l1VictimWay(i)===1.U){
                    l1Tag(i).tag := rfl1Tag 
                    l1Valid(i) := true.B
                    l1Data(i).data:= io.refill.bits.refillData.getPPN 
                }
            }
            mptCacheL1Replace.access.bits:= l1VictimWay
            mptCacheL1Replace.access.valid:= true.B
        }
        when (spRefillEn) {
            /*spVictimWay.zipWithIndex.map{case(en,i) => // update cache content
                when(en){*/
            for (i <- 0 until spSize) {
                when(spVictimWay(i)===1.U){    
                    spTag(i).tag    := rfspTag 
                    spValid(i) := true.B
                    spTag(i).level.get  := io.refill.bits.level(3,1)
                    spData(i).data  := io.refill.bits.refillData.data 
                }
            }
            mptCacheSpReplace.access.bits:= spVictimWay
            mptCacheSpReplace.access.valid:= true.B
        }
    }
}
////////////////////////////////////////// MptMissQueue START ///////////////////////////////////////////////////////////////////////////////
 
 

class MptMissQueueToTWReqBundle(implicit p: Parameters) extends XSBundle with MPTCacheParam {
    val hitAddr =  UInt(ppnLen.W)
    val reqPA = UInt((PAddrBits- MptOff).W)
    val hitLevel = UInt(mptLevelLenOH.W)
}
 
class missCacheBundle(implicit p: Parameters) extends XSBundle with MPTCacheParam {
    val hitLevel = UInt(mptLevelLenOH.W)
    val hitAddr = UInt(ppnLen.W)//hit addr 
    val source= UInt(mptSourceWidth.W)
    val PA= UInt((ppnLen).W)//req pa minsize is 4k,dont need 12bits offset
    val mptOnly= Bool()//1bit control signal for tlb, does basically nothing in mptc
}

class MptMissQueueIO(implicit p: Parameters)  extends MMUIOBaseBundle with MPTCacheParam {
    val missCache =Flipped(DecoupledIO(new missCacheBundle()))

    val twReq = DecoupledIO(new MptMissQueueToTWReqBundle())

    
    val refill = Flipped(ValidIO(new refillBundle()))
    val resp = ValidIO(new Bundle {// source is waiting for cache to resp
        val AccessFault = Bool()
        val mptLevel = UInt(mptLevelLenOH.W)
        val perm = UInt(3.W)
        val PermTlbCompress = Bool()
        val permIsNAPOT = Bool()
        val mptOnly = Bool()
        val reqPA = UInt(ppnLen.W)
        val source= UInt(mptSourceWidth.W)
    })

}

class MptMissQueue(implicit p: Parameters) extends XSModule with MPTCacheParam {
    val io=IO(new MptMissQueueIO)

    val flush=io.sfence.valid || io.csr.mmpt.changed
    val mptFlushReset= (reset.asBool || flush).asAsyncReset 
    withReset (mptFlushReset){//
        val ReqFIFO = Module(new Queue(new missCacheBundle() ,entries=4,pipe = true)).io //FIFO queue,record offset
        val FIFONotEmpty = ReqFIFO.deq.valid
        val FIFONotFull = ReqFIFO.enq.ready
        val ReqAltInput = Wire(new missCacheBundle())//alternative input for refill
        val refillReg = RegEnable(io.refill.bits, 0.U.asTypeOf(new refillBundle()),io.refill.valid)//refill input, valid when refill is valid

        val refilling = Wire(Bool())
        val refillCounter= RegInit(0.U(4.W))
        when(io.refill.valid) {//refill valid and not access fault
            refillCounter:="b1000".U
        } .otherwise{
            refillCounter:=refillCounter>>1.U
        }

   
        refilling := refillCounter > 0.U// is refiill state when counter != 0 4 clk
        
        val L3Hit = ReqFIFO.deq.bits.PA(ppnLen - 1,ppnLen - mptL3TagLen) === refillReg.PA(PAddrBits- MptOff - 1,PAddrBits- MptOff-mptL3TagLen)//35:31 31:27
        val L2Hit = ReqFIFO.deq.bits.PA(ppnLen - 1,ppnLen - mptL2TagLen) === refillReg.PA(PAddrBits- MptOff - 1,PAddrBits- MptOff-mptL2TagLen)//35:22 31:18
        val L1Hit = ReqFIFO.deq.bits.PA(ppnLen - 1,ppnLen - mptL1TagLen) === refillReg.PA(PAddrBits- MptOff - 1,PAddrBits- MptOff-mptL1TagLen)//35:13 31:9
        val L0Hit = ReqFIFO.deq.bits.PA(ppnLen - 1,MptOff-offLen) === refillReg.PA //35:4 31:0
        val HitFIFO = Mux1H(Seq(
            refillReg.level(3)-> L3Hit,
            refillReg.level(2)-> L2Hit,
            refillReg.level(1)-> L1Hit,
            refillReg.level(0)-> L0Hit//4k addr len can not repeat
        )) //

        io.missCache.ready := FIFONotFull  && !refilling

        ReqFIFO.enq.valid := (FIFONotFull && io.missCache.valid && !refilling)  || (refilling && FIFONotEmpty  && !refillReg.isLeafMpte  && !refillReg.isAf)  || (refilling && FIFONotEmpty  && !HitFIFO && (refillReg.isLeafMpte || refillReg.isAf) ) 
        // Entries are enqueued under three conditions: 1. Normal miss cache request; 2. Refill of a non-leaf MPTE; 3. Refill of a leaf MPTE or access fault, provided it is not a hit.
        ReqFIFO.enq.bits := Mux(refilling, ReqAltInput, io.missCache.bits)

        ReqFIFO.deq.ready :=  (FIFONotEmpty && refilling)


        ReqAltInput := ReqFIFO.deq.bits 

        when(HitFIFO && refilling ) {
            ReqAltInput.hitLevel := refillReg.level
            ReqAltInput.hitAddr :=  refillReg.refillData.getPPN
        }

        val retPermOffset= Mux1H(Seq(//different level use different offset vpnnLen
            refillReg.level(3)-> ReqFIFO.deq.bits.PA(MptOff-offLen - 1+vpnnLen*3, 0+vpnnLen*3),//30:27
            refillReg.level(2)-> ReqFIFO.deq.bits.PA(MptOff-offLen - 1+vpnnLen*2, 0+vpnnLen*2),//21:18
            refillReg.level(1)-> ReqFIFO.deq.bits.PA(MptOff-offLen - 1+vpnnLen, 0+vpnnLen),//12:9
            refillReg.level(0)-> ReqFIFO.deq.bits.PA(MptOff-offLen - 1, 0))//3:0
        )    
        io.resp.bits.perm := refillReg.refillData.extractPerm(retPermOffset)//extractPerm is a function in mptData, extract perm from refillData, offset is the offset of the perm in refillData.data

        io.resp.valid := FIFONotEmpty && HitFIFO && refilling &&(refillReg.isAf || refillReg.isLeafMpte) //refillCounter.orR means we are refilling, FIFO not empty means we have a request to respond
        io.resp.bits.AccessFault := refillReg.isAf ||((!io.resp.bits.perm(0)) && io.resp.bits.perm(1))//not read but write is af  //access fault, no need to refill, just return the fault
        io.resp.bits.mptLevel :=  OHToUInt(refillReg.level) // mptLevelLenOH.W
        io.resp.bits.reqPA := ReqFIFO.deq.bits.PA //reqPA is the PA of the request, used to generate the refill address
        io.resp.bits.source := ReqFIFO.deq.bits.source //source is the source 
        io.resp.bits.mptOnly:= ReqFIFO.deq.bits.mptOnly && io.resp.valid

        val PermsAsVec = Wire(Vec(16,UInt(3.W))) //perm xwr bits, total 16 xwrs in one mpte
        for (i <- 0 until 16) {PermsAsVec(i):= refillReg.refillData.data((2+ i*3), (i*3))}
        val PermsEqual = Wire(Vec(16-1,Bool()))
        for (i <- 0 until 16-1) {PermsEqual(i):= PermsAsVec(i+1) === PermsAsVec(i)}//delay 1XNOR + 2 and gates = 3   
        val leftPermsAllEqual = PermsEqual.slice(0,7).reduce(_&&_) //=PermsEqual(6,0), delay 3
        val rightPermsAllEqual = PermsEqual.slice(8,15).reduce(_&&_) //=PermsEqual(14,8)
        //OH (refillReg.level === 1.U(4.W)) is (refillReg.level(0))
        io.resp.bits.permIsNAPOT := (refillReg.level(0)) && leftPermsAllEqual && rightPermsAllEqual && PermsEqual(7) //Delay 2
        io.resp.bits.PermTlbCompress := (refillReg.level(0)) && Mux(ReqFIFO.deq.bits.PA(MptOff-offLen - 1, 0)<8.U,leftPermsAllEqual, rightPermsAllEqual)//delay 3

        io.twReq.bits.hitAddr:=ReqFIFO.deq.bits.hitAddr
        io.twReq.bits.reqPA:=ReqFIFO.deq.bits.PA(ppnLen - 1,MptOff-offLen)
        io.twReq.bits.hitLevel:=ReqFIFO.deq.bits.hitLevel
        io.twReq.valid:= FIFONotEmpty  && !refilling && io.twReq.ready
    }
}


class mptTableWalkerIO(implicit p: Parameters) extends MMUIOBaseBundle with MPTCacheParam {
    val req = Flipped(DecoupledIO(new MptMissQueueToTWReqBundle()))
    
    //val resp = DecoupledIO(new TWtoMptMissQueueRespBundle())

    val mem = new Bundle {
        val req = DecoupledIO(new Bundle { val addr = UInt(PAddrBits.W)})
        val resp = Flipped(ValidIO(UInt(XLEN.W))) 
        //val mask = Input(Bool()) dont need？ 
    }

    val pmp = new Bundle { 
        val req = ValidIO(new PMPReqBundle())
        val resp = Flipped(new PMPRespBundle())
    }
    val refill = ValidIO(new refillBundle()) 

}

 
class MPTTableWalker (implicit p: Parameters) extends XSModule with MPTCacheParam{
    val io=IO(new mptTableWalkerIO)
    val mem=io.mem 
    
    io.pmp.req.bits.size := 3.U 
    io.pmp.req.bits.cmd := TlbCmd.read

    val flush=io.sfence.valid || io.csr.mmpt.changed
    val mptFlushReset= (reset.asBool || flush).asAsyncReset 
    withReset (mptFlushReset){//
        ////////////////
        val pa = RegEnable(io.req.bits.reqPA, 0.U,io.req.fire)
        // Store the received request PA in a register, used to generate the PN1/2/3 offsets for synthesizing the table walk address.
        io.refill.bits.PA := pa
        ////////////////////////////////////////////////////////////////////

        // 定义level寄存器
        val setLevel= Wire(Bool())
        val setPmpCheckLevel= Wire(Bool())
        val nextLevel = Wire(UInt(mptLevelLenOH.W))
        val nextPmpCheckLevel = Wire(UInt(mptLevelLenOH.W))

        val level = RegEnable(nextLevel,"b1000".U(mptLevelLenOH.W),setLevel)
        val pmpCheckLevel = RegEnable(nextPmpCheckLevel,"b1000".U(mptLevelLenOH.W),setPmpCheckLevel)

 
        val mpteResp =Wire(new mptEntry())
        if(HasMptCheckDefault){
            mpteResp.genFake(level)
        }else{
            mpteResp.apply(mem.resp.bits)// mem  mpte  mpteData
        }

        //////

        val mpteData = Reg(new mptData())
        // Stores the returned permissions/lower-level address, or the incoming request address entry; used to return permissions or synthesize the lower-level page table address with PA.
        
        io.refill.bits.refillData:= mpteData// output alloc
        val isLeafMpte=RegEnable(mpteResp.isLeaf,false.B,io.mem.resp.valid)
        io.refill.bits.isLeafMpte := isLeafMpte// tell cache if the current refill is leaf node 
        val mpteInvalid=RegEnable(!mpteResp.isValid,false.B,io.mem.resp.valid)//1 level not on top of mem.resp 
        val rsvZeroError0=RegEnable(mem.resp.bits(9,2).orR,false.B,io.mem.resp.valid)//max 3 level or gate on top of mem.resp，NON ZERO error of mtpe
        val rsvZeroError1=RegEnable(mem.resp.bits(62,58).orR,false.B,io.mem.resp.valid)
        val rsvZeroError2=false.B
        when(io.req.fire) {
            mpteData.apply(io.req.bits.hitAddr)
        }.elsewhen(io.mem.resp.valid){
            mpteData:=mpteResp.data
        }

        val pn=Wire(UInt(9.W))
        pn:=Mux1H(Seq(
        level(3)-> Cat(0.U(4.W),pa(47-MptOff,43-MptOff)),
        level(2)-> pa(42-MptOff,34-MptOff),
        level(1)-> pa(33-MptOff,25-MptOff) ,
        level(0)-> pa(24-MptOff,16-MptOff)
        ))

        //3 layers of gate logic select the coresponding PN[i] based on cur level,just a onehot mux 
        val memAddr = mpteData.getAddr(pn)//生成访问的addr， cat wire 0 延迟
        io.mem.req.bits.addr:= memAddr
        //////////////////////////////////////////////////////////////// 
        io.pmp.req.valid:= DontCare 
        io.pmp.req.bits.addr:= Mux(io.mem.resp.valid, mpteResp.getAddr(pn), memAddr)//should be safer than just := memAddr
        //io.pmp.req.bits.cmd := TlbCmd.read // uncomment 
        //io.pmp.req.bits.size := 3.U // TODO: fix it
        ////AccessFault logic
        val pmpFail= if(HasMptCheckDefault)(false.B) else (!isLeafMpte)&&(io.pmp.resp.ld || io.pmp.resp.mmio) //PMP delay unknown 
        val entryError=if(HasMptCheckDefault)(false.B) else( mpteInvalid|| rsvZeroError0|| rsvZeroError1 || rsvZeroError2 ||((!isLeafMpte) && level===1.U))//level==0 non leaf, zero=/=0,pmp fail,invalid casue AccessFault
        val AccessFault= entryError|| pmpFail//pmp fail also cause AccessFault
       
        io.refill.bits.level:= Mux(pmpFail,pmpCheckLevel,level)//pmpFail return next level,else cur level
        io.refill.bits.isAf:= AccessFault    

        //io.refill.bits.level:= level
        // pmp fail will not be recorded(if the root addr+ pn[i] cause pmp fail does not necessarily mean that the root addr + other offset will cause pmp fail, so entry will be refilled as a normal intermidiate node)
        //////////////////////////////FSM

    object mystate extends ChiselEnum {
            val sIDLE , sMEM_REQ, sMEM_RESP, sADDR_PROC = Value
        }
    import mystate._
    
        //val sIDLE :: sMEM_REQ :: sMEM_RESP :: sADDR_PROC :: sMptMissQueue_RETURN  :: Nil = Enum(5)
        val curState = RegInit(sIDLE)
        val nextState = WireDefault(sIDLE)
        curState:= nextState// 2 proc FSM 
        //fsm start
            mem.req.valid := false.B
            io.req.ready:= false.B
            nextState := curState
            setLevel:= false.B
            setPmpCheckLevel:=false.B
            nextLevel:=level>> 1.U //onehotcounter-1  no aflevel 
            nextPmpCheckLevel:=pmpCheckLevel>> 1.U
            io.refill.valid:= false.B
        //default val
            switch(curState) {
                is(sIDLE) {
                    io.req.ready:= true.B
                    when(io.req.fire){ 
                        setPmpCheckLevel:=true.B
                        nextLevel:= io.req.bits.hitLevel
                        nextPmpCheckLevel:=io.req.bits.hitLevel
                        nextState := sMEM_REQ//to mem req if fire 
                        setLevel:=true.B
                    }
                }
                is(sMEM_REQ) {
                    mem.req.valid:= true.B//req valid when not fire                    
                  
                    when(io.mem.req.fire) {//just waiting, timing safe
                        nextState := sMEM_RESP //to wait resp
                    }.otherwise{
 
                    }
                }
                is(sMEM_RESP) {//unknown in delay,timing?
                    when(io.mem.resp.valid) {//do nothing,delay one cycle OPTPOINT*
                    nextState := sADDR_PROC
                    setPmpCheckLevel:= true.B
                    }
                }
                is(sADDR_PROC) {//known delay
                // 处理返回的节点
                    when(AccessFault||isLeafMpte){//out delay unknown 
                        //when(isLeafMpte){io.refill.valid:= true.B}
                        io.refill.valid:= true.B
                        nextState :=sIDLE    //OPTPOINT*
                    }.otherwise{
                        io.refill.valid:= true.B//start refill
                        setLevel:=true.B
                        nextState :=sMEM_REQ    //OPTPOINT*
                    }      
                }

            }
        //fsm end         
    }
}

class mptCheckerIO(implicit p: Parameters) extends MMUIOBaseBundle with HasPtwConst {
val mem = new Bundle {
    val req = DecoupledIO(new L2TlbMemReqBundle())
    val resp = Flipped(ValidIO(UInt(XLEN.W)))
    val mask = Input(Bool())//mask bit 
  }
    val req = Flipped(DecoupledIO(new mptReqBundle()))
    val resp = ValidIO(new mptRespBundle())

    val pmp = new Bundle {
        val req = ValidIO(new PMPReqBundle())
        val resp = Flipped(new PMPRespBundle())
    }
}

class mptChecker(implicit p: Parameters) extends XSModule with HasPtwConst {
    val io = IO(new mptCheckerIO)
    io.mem.req.bits.hptw_bypassed := true.B//never refill to page cache
    io.mem.req.bits.id:=mptcMemReqID.U(bMemID.W)
    val mptCacheInst    = Module((new MPTCache())).io
    val mptTWInst       = Module((new MPTTableWalker())).io
    val MptMissQueueInst  = Module((new MptMissQueue())).io
 
    mptCacheInst.csr <> io.csr // 
    mptCacheInst.sfence <> io.sfence // 
    
    mptTWInst.csr <> io.csr // 
    mptTWInst.sfence  <> io.sfence  // 
    
    MptMissQueueInst.csr <> io.csr // 
    MptMissQueueInst.sfence  <> io.sfence  
 
    mptCacheInst.req.bits.mptOnly:= io.req.bits.mptOnly//need some fix 
    mptCacheInst.req.bits.reqPA := io.req.bits.reqPA
    mptCacheInst.req.bits.source := io.req.bits.id
    mptCacheInst.req.valid:=  io.req.valid
    io.req.ready:= mptCacheInst.req.ready
   
 

    val mptReturn = Wire(new mptRespBundle())
    mptReturn.mptPerm := Mux(MptMissQueueInst.resp.valid, MptMissQueueInst.resp.bits.perm, mptCacheInst.respHit.bits.perm) 
    mptReturn.contigousPerm := Mux(MptMissQueueInst.resp.valid, MptMissQueueInst.resp.bits.PermTlbCompress, mptCacheInst.respHit.bits.tlbContigousPerm)
    mptReturn.id := Mux(MptMissQueueInst.resp.valid, MptMissQueueInst.resp.bits.source, mptCacheInst.respHit.bits.source)
    mptReturn.mptLevel:= Mux(MptMissQueueInst.resp.valid, MptMissQueueInst.resp.bits.mptLevel, mptCacheInst.respHit.bits.mptLevel)
    mptReturn.mptOnly:= Mux(MptMissQueueInst.resp.valid, MptMissQueueInst.resp.bits.mptOnly, mptCacheInst.respHit.bits.mptOnly)
    mptReturn.reqPA:= Mux(MptMissQueueInst.resp.valid, MptMissQueueInst.resp.bits.reqPA, mptCacheInst.respHit.bits.reqPA)
    mptReturn.accessFault:=  Mux(MptMissQueueInst.resp.valid, MptMissQueueInst.resp.bits.AccessFault, mptCacheInst.respHit.bits.accessFault)
    mptReturn.permIsNAPOT :=  Mux(MptMissQueueInst.resp.valid, MptMissQueueInst.resp.bits.permIsNAPOT, mptCacheInst.respHit.bits.permIsNAPOT)

    io.resp.valid := MptMissQueueInst.resp.valid||mptCacheInst.respHit.valid
    io.resp.bits <> mptReturn

    MptMissQueueInst.refill<>mptTWInst.refill
    //cache miss send MptMissQueue
    MptMissQueueInst.missCache.bits.mptOnly:=  mptCacheInst.respMiss.bits.mptOnly
    MptMissQueueInst.missCache.bits.hitLevel:= mptCacheInst.respMiss.bits.hitLevel
    MptMissQueueInst.missCache.bits.hitAddr:= mptCacheInst.respMiss.bits.ppn
    MptMissQueueInst.missCache.bits.source:=  mptCacheInst.respMiss.bits.source
    MptMissQueueInst.missCache.bits.PA:= mptCacheInst.respMiss.bits.PA
    MptMissQueueInst.missCache.valid:= mptCacheInst.respMiss.valid
    mptCacheInst.respMiss.ready:= MptMissQueueInst.missCache.ready
    //cache refill io
    mptCacheInst.refill <> mptTWInst.refill
    // MptMissQueue-twio
    mptTWInst.req <> MptMissQueueInst.twReq
   // tw-MptMissQueueio
    //table walk 读取ram    
    io.mem.req.bits.addr:= mptTWInst.mem.req.bits.addr
    io.mem.req.valid := mptTWInst.mem.req.valid
    mptTWInst.mem.req.ready:= io.mem.req.ready

    mptTWInst.mem.resp.bits:= io.mem.resp.bits 
    mptTWInst.mem.resp.valid:= io.mem.resp.valid
    //mptTWInst.mem.resp.ready:= true.B   
    //PMP 接口
    mptTWInst.pmp.resp <> io.pmp.resp
    io.pmp.req<> mptTWInst.pmp.req 

    val mptDiabledFakedRespValid = RegInit(false.B)
    val mptDiabledFakedMptOnly = RegInit(false.B)
    when (io.csr.mmpt.mode === 0.U){
        mptCacheInst.req.valid:= false.B //mptmode  return 111

        io.req.ready := true.B  
        when(io.req.fire) {
            mptDiabledFakedRespValid := true.B
            mptDiabledFakedMptOnly := io.req.bits.mptOnly
        }
        io.resp.valid := mptDiabledFakedRespValid 
        io.resp.bits.mptOnly := mptDiabledFakedMptOnly
        when(io.resp.fire){
            mptDiabledFakedRespValid := false.B
            mptDiabledFakedMptOnly := false.B 
        }
    }

    // if(HasMptCheckDefault){
    //     io.mem.req.valid:= false.B
    //     mptTWInst.pmp.resp.ld:= false.B
    //     mptTWInst.pmp.resp.st:= false.B
    //     mptTWInst.pmp.resp.instr:= false.B
    //     mptTWInst.pmp.resp.mmio:= false.B
    //     mptTWInst.pmp.resp.atomic:= false.B//fake pmp

    //     mptTWInst.mem.resp.bits:=Cat(0.U(6.W), ~(0.U(48.W)), 0.U(8.W),"b11".U(2.W))//fake leaf node , allow all
    //     mptTWInst.mem.resp.valid:= RegNext( mptTWInst.mem.req.valid,false.B) // fake mem 
    // }
}

 