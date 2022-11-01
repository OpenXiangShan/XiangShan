package xiangshan.backend.fu.util

import chisel3._
import chisel3.util._
import xiangshan.XSBundle
import chipsalliance.rocketchip.config.Parameters
import utils.ConsecutiveOnes

trait SdtrigExt {
  implicit val p: Parameters
  class TDataRegs extends XSBundle {
    val tdata1 = UInt(XLEN.W)
    val tdata2 = UInt(XLEN.W)
  }
  object TDataRegs extends TDataRegs {
    def apply() = new TDataRegs
  }

  class Tdata1Bundle extends XSBundle {
    val type_ = TrigTypeEnum()    // [XLEN-1: XLEN-4]
    val dmode = Bool()            // [XLEN-5]
    val data  = new Tdata1Data    // [XLEN-6, 0]
    require(this.getWidth == XLEN)
    def getTriggerAction : TrigActionEnum = {
      this.data.asTypeOf(new MControlData).action
    }
    def getTriggerChain: Bool = {
      this.data.asTypeOf(new MControlData).chain
    }
    def getTiming: Bool = {
      this.data.asTypeOf(new MControlData).timing
    }
  }
  object Tdata1Bundle {
    def apply(): Tdata1Bundle = new Tdata1Bundle
    def Read(rdata: UInt) : UInt = rdata
    def Write(wdata: UInt, tdata1: UInt, chainable: Bool) : UInt = {
      val tdata1_old = WireInit(tdata1.asTypeOf(new Tdata1Bundle))
      val tdata1_new = Wire(new Tdata1Bundle)
      val wdata_new = WireInit(wdata.asTypeOf(new Tdata1Bundle))
      tdata1_new.type_ := wdata_new.type_.legalize
      tdata1_new.dmode := false.B // not support yet
      when (wdata_new.type_.asUInt === TrigTypeEnum.MCONTROL) {
        tdata1_new.data.value := MControlData.Write(wdata_new.data.asUInt, tdata1_old.data.asUInt, chainable)
      }.otherwise {
        tdata1_new.data.value := 0.U
      }
      tdata1_new.asUInt
    }
  }

  class TrigTypeEnum extends Bundle {
    val value         = UInt(4.W)
    def NONE          = 0.U
    def LEGACY        = 1.U
    def MCONTROL      = 2.U
    def ICOUNT        = 3.U
    def ITRIGGER      = 4.U
    def ETRIGGER      = 5.U
    def MCONTROL6     = 6.U
    def TMEXTTRIGGER  = 7.U
    def disabled      = 15.U
    /**
      * XS supports part of trigger type of Sdtrig extension
      * @param data trigger type checked
      * @return true.B, If XS support this trigger type
      */
    def isLegal : Bool = {
      this.asUInt === this.MCONTROL
    }
    def legalize : TrigTypeEnum = {
      Mux(this.isLegal, this.asUInt, this.disabled).asTypeOf(new TrigTypeEnum)
    }
  }
  object TrigTypeEnum extends TrigTypeEnum {
    def apply()   = new TrigTypeEnum
  }

  protected class Tdata1Data extends XSBundle {
    val value = UInt((XLEN-5).W)
  }

  class MControlData extends Tdata1Data {
    override val value = null
    val maskmax   = UInt(6.W)                           // [XLEN-6: XLEN-11]
    val zero1     = if (XLEN==64) UInt(30.W) else null  // [XLEN-12: 23]
    val sizehi    = if (XLEN==64) UInt(2.W) else null   // [22:21]
    val hit       = Bool()                              // [20]
    val select    = Bool()                              // [19]
    val timing    = Bool()                              // [18]
    val sizelo    = UInt(2.W)                           // [17:16]
    val action    = TrigActionEnum()                    // [15:12]
    val chain     = Bool()                              // [11]
    val match_    = TrigMatchEnum()                     // [10:7]
    val m         = Bool()                              // [6]
    val zero2     = Bool()                              // [5]
    val s         = Bool()                              // [4]
    val u         = Bool()                              // [3]
    val execute   = Bool()                              // [2]
    val store     = Bool()                              // [1]
    val load      = Bool()                              // [0]
    require(this.getWidth == (new Tdata1Data).getWidth)

    def isFetchTrigger: Bool = this.execute
    def isMemAccTrigger: Bool = this.store || this.load
  }
  object MControlData {
    def Read(rdata: UInt) : UInt = rdata
    def Write(wdata: UInt, tdata1data: UInt, chainable: Bool) : UInt = {
      val mcontrol_old = WireInit(tdata1data.asTypeOf(new MControlData))
      val mcontrol_new = WireInit(wdata.asTypeOf(new MControlData))
      val wdata_new = WireInit(wdata.asTypeOf(new MControlData))
      mcontrol_new.maskmax  := 0.U
      mcontrol_new.zero1    := 0.U
      mcontrol_new.sizehi   := 0.U
      mcontrol_new.hit      := false.B
      mcontrol_new.select   := wdata_new.execute && wdata_new.select // not support rdata/wdata trigger
      mcontrol_new.timing   := false.B // only support trigger fires before its execution
      mcontrol_new.sizelo   := 0.U
      mcontrol_new.action   := wdata_new.action.legalize
      mcontrol_new.chain    := chainable && wdata_new.chain
      mcontrol_new.match_   := wdata_new.match_.legalize
      mcontrol_new.zero2    := 0.U
      mcontrol_new.asUInt
    }
  }

  class TrigActionEnum extends Bundle {
    val value       = UInt(4.W)
    def BKPT_EXCPT  = 0.U // raise breakpoint exception
    def DEBUG_MODE  = 1.U // enter debug mode
    def TRACE_ON    = 2.U
    def TRACE_OFF   = 3.U
    def TRACE_NOTIFY= 4.U
    def default     = this.BKPT_EXCPT
    /**
      * XS supports part of trigger action type of Sdtrig extension
      * @param data action checked
      * @return true.B, If XS support such trigger action type
      */
    def isLegal : Bool = {
      this.asUInt === this.BKPT_EXCPT || this.asUInt === this.DEBUG_MODE
    }
    def legalize : TrigActionEnum = {
      Mux(this.isLegal, this.asUInt, this.default).asTypeOf(new TrigActionEnum)
    }
  }
  object TrigActionEnum extends TrigActionEnum {
    def apply() = new TrigActionEnum
  }

  class TrigMatchEnum extends Bundle {
    val value     = UInt(4.W)
    private def not_bit = 8.U
    def EQ        = 0.U
    def NAPOT     = 1.U
    def GE        = 2.U
    def LT        = 3.U
    def MASK_LO   = 4.U
    def MASK_HI   = 5.U
    def NE        = EQ      | this.not_bit // not eq
    def NNAPOT    = NAPOT   | this.not_bit // not napot
    def NMASK_LO  = MASK_LO | this.not_bit // not mask low
    def NMASK_HI  = MASK_HI | this.not_bit // not mask high
    def default   = this.EQ
    def isRVSpecLegal : Bool = {
      this.asUInt === this.EQ || this.asUInt === this.NAPOT ||
        this.asUInt === this.GE || this.asUInt === this.LT ||
        this.asUInt === this.MASK_LO || this.asUInt === this.MASK_HI ||
        this.asUInt === this.NE || this.asUInt === this.NNAPOT ||
        this.asUInt === this.NMASK_LO || this.asUInt === this.NMASK_HI
    }

    /**
      * XS supports part of trigger match type of Sdtrig extension
      * @param data match type checked
      * @return true.B, If XS support such trigger match type
      */
    def isLegal : Bool = {
      this.asUInt === this.EQ || this.asUInt === this.GE || this.asUInt === this.LT
    }
    def legalize : TrigMatchEnum = {
      Mux(this.isLegal, this.asUInt, this.default).asTypeOf(new TrigMatchEnum)
    }
  }
  object TrigMatchEnum extends TrigMatchEnum {
    def apply() = new TrigMatchEnum
  }

  /**
    * Check if triggers can fire
    * @param triggerNum
    * @param canFireVec
    * @param hitVec
    * @param timingVec
    * @param chainVec
    */
  def TriggerCheckCanFire(triggerNum: Int, canFireVec: Vec[Bool], hitVec: Vec[Bool], timingVec: Vec[Bool], chainVec: Vec[Bool]): Unit = {
    val trigger2ChainVec = WireInit(VecInit(Seq.fill(triggerNum)(false.B)))
    val trigger2TimingSameVec = WireInit(VecInit(Seq.fill(triggerNum)(true.B)))
    val trigger2TimingOkVec = WireInit(VecInit(Seq.fill(triggerNum)(true.B)))
    val trigger2ChainOkVec = WireInit(VecInit(Seq.fill(triggerNum)(true.B)))
    for (i <- 1 until triggerNum) { // the 0th trigger always chain ok
      trigger2ChainOkVec(i) := chainVec(i - 1) && hitVec(i - 1) || !chainVec(i - 1)
    }

    for (i <- 1 until triggerNum) { // the 0th trigger always timing same, not chain, timing ok
      trigger2TimingSameVec(i) := timingVec(i - 1) === timingVec(i)
      trigger2ChainVec(i) := chainVec(i - 1) && !chainVec(i)
      trigger2TimingOkVec(i) := trigger2ChainVec(i) && trigger2TimingSameVec(i) || !chainVec(i - 1)
    }
    canFireVec.zipWithIndex.foreach {
      case (canFire, i) => canFire := trigger2ChainOkVec(i) && trigger2TimingOkVec(i) && hitVec(i) && !chainVec(i)
    }
  }

  /**
    * Check if chain vector is legal
    * @param chainVec
    * @param chainLen
    * @return true.B if the max length of chain don't exceed the permitted length
    */
  def TriggerCheckChainLegal(chainVec: Seq[Bool], chainLen: Int): Bool = {
    !ConsecutiveOnes(chainVec, chainLen)
  }

  /**
    * Compare data with trigger data
    * @param data data compared
    * @param tdata data from trigger
    * @param matchType trigger match type in UInt
    * @param enable if the trigger is enabled
    * @return true.B if data meet the trigger match condition
    */
  def TriggerCmp(data: UInt, tdata: UInt, matchType: UInt, enable: Bool): Bool = {
    val eq = data === tdata
    val ge = data >= tdata
    val lt = data < tdata
    val res = MuxLookup(matchType, false.B, Seq(
      TrigMatchEnum.EQ -> eq,
      TrigMatchEnum.GE -> ge,
      TrigMatchEnum.LT -> lt
    ))
    res && enable
  }
}
