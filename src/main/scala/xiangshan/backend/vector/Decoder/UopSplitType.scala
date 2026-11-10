package xiangshan.backend.vector.Decoder

import chisel3._
import xiangshan.backend.vector.Decoder.Split.SplitType
import xiangshan.backend.vector.Decoder.Types.EnumLMUL
//import xiangshan.backend.vector.Decoder.Uop.UopType
//import xiangshan.backend.vector.Decoder.Uop.UopType.UopBase
import xiangshan.backend.vector.Decoder.Uop.UopTrait.UopBase
import xiangshan.backend.vector._
import xiangshan.backend.vector.util.Select.Mux1HLookUp

class InstDecodeSplitTypeModule

object UopNumMapType extends Enumeration {
  val lmul    = Value
  val lmulx2  = Value
  val indexls = Value
}

object UopSplitType extends HasVectorSettings {

  sealed abstract class UopSplitBase(
    val splitType: SplitType.Type,
  ) (
    vdAlloc : (EnumLMUL.Type, Int) => Bool = (_,_) => 1.B,
    uopDep  : (EnumLMUL.Type, Int) => Bool = (_,_) => 0.B,
    readVl  : (EnumLMUL.Type, Int) => Bool = (_,_) => 0.B,
    readV0  : (EnumLMUL.Type, Int) => Bool = (_,_) => 0.B,
    writeVl : (EnumLMUL.Type, Int) => Bool = (_,_) => 0.B,
    widenSrc1 : (EnumLMUL.Type, Int) => Bool = (_,_) => 0.B,
    widenSrc2 : (EnumLMUL.Type, Int) => Bool = (_,_) => 0.B,
    widenDest : (EnumLMUL.Type, Int) => Bool = (_,_) => 0.B,
  ) {
    protected var uops: Seq[UopBase] = Seq()

    def bindUops(uops: UopBase*): this.type = {
      require(uops.length == 1)
      this.uops = uops
      this
    }

    def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase]
  }

  case class InstLreg(
    vs2: UInt,
    vs1: UInt,
    vd : UInt,
    vm : Bool,
  )

//  case class UopSplitVVV() extends UopSplitBase(
//    SplitType.VVV
//  )(
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = this.uops.head
//      require(uop.vdEew1b)
//      Seq.tabulate(maxLMUL) { i =>
//        uop(r.vd | i.U, r.vs1 | i.U, r.vs2 | i.U)
//      }
//    }
//  }
//
//  case class UopSplitVVM() extends UopSplitBase(
//    SplitType.VVM
//  )(
//    vdAlloc = (lmul, i) => (i == 0).B,  // only uop0 alloc preg
//    uopDep = (lmul, i) => (i != 0).B,   // uop1~n depend on the former uop
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = this.uops.head
//      require(uop.vdEew1b)
//      Seq.tabulate(maxLMUL) { i =>
//        uop(r.vd, r.vs1 | i.U, r.vs2 | i.U)
//      }
//    }
//  }
//
//  case class UopSplitWVV() extends UopSplitBase(
//    SplitType.WVV
//  )(
//    vdAlloc = (lmul, i) => (i % 2 == 0).B, // only even uop alloc preg
//    uopDep = (lmul, i) => (i % 2 == 1).B, // odd uop depends on former even uop
//    widenSrc2 = (_, _) => true.B,
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = this.uops.head
//      require(uop.src2Widen)
//      Seq.tabulate(maxLMUL) { i =>
//        uop(r.vd | (i / 2).U, r.vs1 | (i / 2).U, r.vs2 | i.U)
//      }
//    }
//  }
//
//  case class UopSplitWVW() extends UopSplitBase(
//    SplitType.WVW
//  )(
//    widenSrc2 = (_, _) => true.B,
//    widenDest = (_, _) => true.B,
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = this.uops.head
//      Seq.tabulate(maxLMUL) { i =>
//        uop(r.vd | i.U, r.vs1 | (i / 2).U, r.vs2 | i.U)
//      }
//    }
//  }
//
//  case class UopSplitVVW() extends UopSplitBase(
//    SplitType.VVW
//  )(
//    widenDest = (_, _) => true.B,
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = this.uops.head
//      Seq.tabulate(maxLMUL) { i =>
//        uop(r.vd | i.U, r.vs1 | (i / 2).U, r.vs2 | (i / 2).U)
//      }
//    }
//  }
//
//  case class UopSplitEXT4() extends UopSplitBase(
//    SplitType.EXT4
//  )(
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = this.uops.head
//      require(uop.src1Type.isEmpty)
//
//      Seq.tabulate(maxLMUL) { i =>
//        uop(null, r.vs2 | (i / 4).U, r.vd)
//      }
//    }
//  }
//
//  case class UopSplitEXT8() extends UopSplitBase(
//    SplitType.EXT8
//  )(
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = this.uops.head
//      require(uop.src1Type.isEmpty)
//
//      Seq.tabulate(maxLMUL) { i =>
//        uop(null, r.vs2 | (i / 8).U, r.vd)
//      }
//    }
//  }
//
//  // lmul=m8       src1           src2
//  // uop0: v[vd] = v[vs2+0] op    v[vs2+7] (src1/2 mask)
//  // uop1: v[vd] = v[vs2+1] op    v[vd]    (src1 mask)
//  // uop2: v[vd] = v[vs2+2] op    v[vd]    (src1 mask)
//  // uop3: v[vd] = v[vs2+3] op    v[vd]    (src1 mask)
//  // uop4: v[vd] = v[vs2+4] op    v[vd]    (src1 mask)
//  // uop5: v[vd] = v[vs2+5] op    v[vd]    (src1 mask)
//  // uop6: v[vd] = v[vs2+6] op    v[vd]    (src1 mask)
//  // uop7: v[vd] = v[vs1]   redop v[vd]    (no mask)
//  // lmul=m4
//  // uop0: v[vd] = v[vs2+0] op    v[vs2+3] (src1/2 mask)
//  // uop1: v[vd] = v[vs2+1] op    v[vd]    (src1 mask)
//  // uop2: v[vd] = v[vs2+2] op    v[vd]    (src1 mask)
//  // uop3: v[vd] = v[vs1]   redop v[vd]    (no mask)
//  // lmul=m2
//  // uop0: v[vd] = v[vs2+0] op    v[vs2+1] (src1/2 mask)
//  // uop1: v[vd] = v[vs1]   redop v[vd]    (no mask)
//  // lmul<=m1
//  // uop0: v[vd] = v[vs1]   redop v[vd]    (mask)
//  case class UopSplitVREDU() extends UopSplitBase(
//    SplitType.VREDU
//  )(
//    vdAlloc = (_, i) => (i == 0).B,   // only uop0 alloc preg
//    uopDep = (_, i) => (i != 0).B,    // uop1~n depend on the former uop
//  ) {
//    override def bindUops(uops: UopBase*): UopSplitVREDU.this.type = {
//      require(uops.length == 2)
////      require(uops(0).isInstanceOf[UopType.UopInt_S2V_S1VXI_DV])
////      require(uops(1).isInstanceOf[UopType.UopIntRed_S1A_S2V_DA])
//      this.uops = uops
//      this
//    }
//
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop_vv = uops(0)
//      val uop_red = uops(1)
//      enumLMUL match {
//        case EnumLMUL.M8 => Seq(
//          //      vd    vs1          vs2
//          uop_vv (r.vd, r.vs2 | 0.U, r.vs2 | 7.U ),
//          uop_vv (r.vd, r.vs2 | 1.U, r.vd        ),
//          uop_vv (r.vd, r.vs2 | 2.U, r.vd        ),
//          uop_vv (r.vd, r.vs2 | 3.U, r.vd        ),
//          uop_vv (r.vd, r.vs2 | 4.U, r.vd        ),
//          uop_vv (r.vd, r.vs2 | 5.U, r.vd        ),
//          uop_vv (r.vd, r.vs2 | 6.U, r.vd        ),
//          uop_red(r.vd, r.vs1      , r.vd        ),
//        )
//        case EnumLMUL.M4 => Seq(
//          uop_vv (r.vd, r.vs2 | 0.U, r.vs2 | 3.U ),
//          uop_vv (r.vd, r.vs2 | 1.U, r.vd        ),
//          uop_vv (r.vd, r.vs2 | 2.U, r.vd        ),
//          uop_red(r.vd, r.vs1      , r.vd        ),
//        )
//        case EnumLMUL.M2 => Seq(
//          uop_vv (r.vd, r.vs2 | 0.U, r.vs2 | 1.U ),
//          uop_red(r.vd, r.vs1      , r.vd        ),
//        )
//        case _  => Seq(
//          uop_red(r.vd, r.vs1      , r.vs2)
//        )
//      }
//    }
//  }
//
//  case class UopSplitVREDO() extends UopSplitBase(
//    SplitType.VREDO
//  )(
//    vdAlloc = (_, i) => (i == 0).B,   // only uop0 alloc preg
//    uopDep = (_, i) => (i != 0).B,    // uop1~n depend on the former uop
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop_red = this.uops.head
//      Seq(
//        uop_red(r.vd, r.vs1, r.vs2 | 0.U),
//        uop_red(r.vd, r.vd , r.vs2 | 1.U),
//        uop_red(r.vd, r.vd , r.vs2 | 2.U),
//        uop_red(r.vd, r.vd , r.vs2 | 3.U),
//        uop_red(r.vd, r.vd , r.vs2 | 4.U),
//        uop_red(r.vd, r.vd , r.vs2 | 5.U),
//        uop_red(r.vd, r.vd , r.vs2 | 6.U),
//        uop_red(r.vd, r.vd , r.vs2 | 7.U),
//      )
//    }
//  }
//
//  // lmul = m4          dest  src1        src2
//  // uop0: VWOP.VV      vd.w, (vs2+0).vl  (vs2+3).vh,   (src mask)
//  // uop1: VWOP.WV      vd.w, (vs2+0).vh  vd.w,         (src mask)
//  // uop2: VWOP.WV      vd.w, (vs2+1).vl  vd.w,         (src mask)
//  // uop3: VWOP.WV      vd.w, (vs2+1).vh  vd.w,         (src mask)
//  // uop4: VWOP.WV      vd.w, (vs2+2).vl  vd.w,         (src mask)
//  // uop5: VWOP.WV      vd.w, (vs2+2).vh  vd.w,         (src mask)
//  // uop6: VWOP.WV      vd.w, (vs2+3).vl  vd.w,         (src mask)
//  // uop7: VREDUOP.VS   vd.w, vs1.ws      vd.w,         // The EEW of VREDUOP is DOUBLE of SEW
//  // lmul = m2          dest  src1        src2
//  // uop0: VWOP.VV      vd.w, (vs2+0).vl  (vs2+1).vh,   (src mask)
//  // uop1: VWOP.WV      vd.w, (vs2+0).vh  vd.w,         (src mask)
//  // uop2: VWOP.WV      vd.w, (vs2+1).vl  vd.w,         (src mask)
//  // uop3: VREDUOP.VS   vd.w, vs1.ws      vd.w,         // The EEW of VREDUOP is DOUBLE of SEW
//  // lmul <= m1         dest  src1        src2
//  // uop0: VWOP.VV      vd.w, (vs2+0).vl  (vs2+0).vh,
//  // uop1: VREDUOP.VS   vd.w, vs1.ws      vd.w,         // The EEW of VREDUOP is DOUBLE of SEW
//  case class UopSplitVWREDU() extends UopSplitBase(
//    SplitType.VWREDU
//  )(
//    vdAlloc = (_, i) => (i == 0).B,   // only uop0 alloc preg
//    uopDep = (_, i) => (i != 0).B,    // uop1~n depend on the former uop
//    widenSrc1 = (lmul, i) => (i != 0).B,
//    widenSrc2 = (lmul, i) => Mux1HLookUp(lmul, (i == 1).B)(Seq(
//      EnumLMUL.M2 -> (i == 3).B,
//      EnumLMUL.M4 -> (i == 7).B,
//    )),
//    widenDest = (lmul, i) => true.B,
//  ) {
//    override def bindUops(uops: UopBase*): UopSplitVWREDU.this.type = {
//      require(uops.length == 3)
////      require(uops(0).isInstanceOf[UopType.UopInt_S2V_S1VXI_DW])
////      require(uops(1).isInstanceOf[UopType.UopInt_S2W_S1VXI_DW])
////      require(uops(2).isInstanceOf[UopType.UopIntRed_S1A_S2V_DA])
//      this.uops = uops
//      this
//    }
//
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop_vv_w = uops(0)
//      val uop_wv_w = uops(1)
//      val uop_red = uops(2).asInstanceOf[UopType.UopIntRed_S1A_S2V_DA].useDoubleSew
//      enumLMUL match {
//        case EnumLMUL.M4 => Seq(
//          uop_vv_w(r.vd, r.vs2 | 0.U, r.vs2 | 3.U ),
//          uop_wv_w(r.vd, r.vs2 | 0.U, r.vd        ),
//          uop_wv_w(r.vd, r.vs2 | 1.U, r.vd        ),
//          uop_wv_w(r.vd, r.vs2 | 1.U, r.vd        ),
//          uop_wv_w(r.vd, r.vs2 | 2.U, r.vd        ),
//          uop_wv_w(r.vd, r.vs2 | 2.U, r.vd        ),
//          uop_wv_w(r.vd, r.vs2 | 3.U, r.vd        ),
//          uop_red (r.vd, r.vs1      , r.vd        ),
//        )
//        case EnumLMUL.M2 => Seq(
//          uop_vv_w(r.vd, r.vs2 | 0.U, r.vs2 | 1.U ),
//          uop_wv_w(r.vd, r.vs2 | 1.U, r.vd        ),
//          uop_wv_w(r.vd, r.vs2 | 2.U, r.vd        ),
//          uop_red (r.vd, r.vs1      , r.vd        ),
//        )
//        case _ => Seq(
//          uop_vv_w(r.vd, r.vs2 | 0.U, r.vs2 | 0.U ),
//          uop_red (r.vd, r.vs1      , r.vd        ),
//        )
//      }
//    }
//  }
//
//  // uop0~7:  VWREDOOP.VS (src mask)
//  // src2: vs2|0,   0,1,1,2,2,3,3
//  // src1: vs1|0,vd|0,0,0,0,0,0,0
//  case class UopSplitVWREDO() extends UopSplitBase(
//    SplitType.VWREDO
//  )(
//    // Todo: can be optimized for frac LMUL
//    vdAlloc = (_, i) => (i == 0).B,   // only uop0 alloc preg
//    uopDep = (_, i) => (i != 0).B,    // uop1~n depend on the former uop
//    widenSrc1 = (lmul, i) => true.B,
//    widenSrc2 = (lmul, i) => false.B,
//    widenDest = (lmul, i) => true.B,
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop_wred = this.uops.head
//        .ensuring(x => x.src1Widen && x.destWiden)
//      Seq(
//        uop_wred(r.vd, r.vs1, r.vs2 | 0.U),
//        uop_wred(r.vd, r.vd , r.vs2 | 0.U),
//        uop_wred(r.vd, r.vd , r.vs2 | 1.U),
//        uop_wred(r.vd, r.vd , r.vs2 | 1.U),
//        uop_wred(r.vd, r.vd , r.vs2 | 2.U),
//        uop_wred(r.vd, r.vd , r.vs2 | 2.U),
//        uop_wred(r.vd, r.vd , r.vs2 | 3.U),
//        uop_wred(r.vd, r.vd , r.vs2 | 3.U),
//      )
//    }
//  }
//
//  case class UopSplitGather() extends UopSplitBase(
//    SplitType.SHUFFLE_GATHER
//  )(
//    uopDep = (_, i) => (i != 0).B,    // uop1~n depend on the former uop
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = uops.head
//      enumLMUL match {
//        case EnumLMUL.M8 => Seq(
//          uop(r.vd | 0.U, r.vs2 | 4.U, r.vs2 | 0.U),
//          uop(r.vd | 1.U, r.vs2 | 5.U, r.vs2 | 1.U),
//          uop(r.vd | 2.U, r.vs2 | 6.U, r.vs2 | 2.U),
//          uop(r.vd | 3.U, r.vs2 | 7.U, r.vs2 | 3.U),
//          uop(r.vd | 4.U, r.vs1 | 4.U, r.vs1 | 0.U),
//          uop(r.vd | 5.U, r.vs1 | 5.U, r.vs1 | 1.U),
//          uop(r.vd | 6.U, r.vs1 | 6.U, r.vs1 | 2.U),
//          uop(r.vd | 7.U, r.vs1 | 7.U, r.vs1 | 3.U),
//        )
//        case EnumLMUL.M4 => Seq(
//          uop(r.vd | 0.U, r.vs2 | 2.U, r.vs2 | 0.U),
//          uop(r.vd | 1.U, r.vs2 | 3.U, r.vs2 | 1.U),
//          uop(r.vd | 2.U, r.vs1 | 2.U, r.vs1 | 0.U),
//          uop(r.vd | 3.U, r.vs1 | 3.U, r.vs1 | 1.U),
//        )
//        case EnumLMUL.M2 => Seq(
//          uop(r.vd | 0.U, r.vs2 | 1.U, r.vs2 | 0.U),
//          uop(r.vd | 1.U, r.vs1 | 1.U, r.vs1 | 0.U),
//        )
//        case _ => Seq(
//          uop(r.vd, r.vs1, r.vs2)
//        )
//      }
//    }
//  }
//
//  case class UopSplitSlideUp() extends UopSplitBase(
//    SplitType.SHUFFLE_SLIDE_UP
//  )(
//    // Todo: optimize by reverse data array read
//    uopDep = (_, i) => (i != 0).B,    // uop1~n depend on the former uop
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = uops.head
//      Seq.tabulate(maxLMUL){ i =>
//        uop(r.vd | i.U, r.vs1, r.vs2 | i.U)
//      }
//    }
//  }
//
//  case class UopSplitCompressDown() extends UopSplitBase(
//    SplitType.SHUFFLE_COMPRESS_DOWN
//  )(
//    uopDep = (_, i) => (i != 0).B,    // uop1~n depend on the former uop
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = uops.head
//      Seq.tabulate(maxLMUL){ i =>
//        uop(r.vd | i.U, r.vs1, r.vs2 | i.U)
//      }
//    }
//  }
//
//  case class UopSplitVlsIdxDIRatio1(order: Boolean) extends UopSplitBase(
//    SplitType.VLSIDX_DI_RATIO_1
//  )(
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = uops.head
//      Seq.tabulate(maxLMUL){ i =>
//        uop(r.vd | i.U, r.vs1, r.vs2 | i.U)
//      }
//    }
//  }
//
//  case class UopSplitVlsIdxDIRatio2(order: Boolean) extends UopSplitBase(
//    SplitType.VLSIDX_DI_RATIO_2
//  )(
//    uopDep = (lmul, i) => (order && i != 0).B
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = uops.head
//      Seq.tabulate(maxLMUL){ i =>
//        uop(r.vd | i.U, r.vs1, r.vs2 | (i / 2).U)
//      }
//    }
//  }
//
//  case class UopSplitVlsIdxDIRatio4(order: Boolean) extends UopSplitBase(
//    SplitType.VLSIDX_DI_RATIO_4
//  )(
//    uopDep = (lmul, i) => (order && i != 0).B
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = uops.head
//      Seq.tabulate(maxLMUL) { i =>
//        uop(r.vd | i.U, r.vs1, r.vs2 | (i / 4).U)
//      }
//    }
//  }
//
//  case class UopSplitVlsIdxDIRatio8(order: Boolean) extends UopSplitBase(
//    SplitType.VLSIDX_DI_RATIO_8
//  )(
//    uopDep = (lmul, i) => (order && i != 0).B
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = uops.head
//      Seq.tabulate(maxLMUL) { i =>
//        uop(r.vd | i.U, r.vs1, r.vs2 | (i / 8).U)
//      }
//    }
//  }
//
//  case class UopSplitVlsIdxDIRatioF8(order: Boolean) extends UopSplitBase(
//    SplitType.VLSIDX_DI_RATIO_F8
//  )(
//    uopDep = (lmul, i) => (order && i != 0).B
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = uops.head
//      Seq.tabulate(maxLMUL) { i =>
//        uop(r.vd | (i / 8).U, r.vs1, r.vs2 | i.U)
//      }
//    }
//  }
//
//  case class UopSplitVlsIdxDIRatioF4(order: Boolean) extends UopSplitBase(
//    SplitType.VLSIDX_DI_RATIO_F4
//  )(
//    uopDep = (lmul, i) => (order && i != 0).B
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = uops.head
//      Seq.tabulate(maxLMUL) { i =>
//        uop(r.vd | (i / 4).U, r.vs1, r.vs2 | i.U)
//      }
//    }
//  }
//
//  case class UopSplitVlsIdxDIRatioF2(order: Boolean) extends UopSplitBase(
//    SplitType.VLSIDX_DI_RATIO_F2
//  )(
//    uopDep = (lmul, i) => (order && i != 0).B
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = uops.head
//      Seq.tabulate(maxLMUL) { i =>
//        uop(r.vd | (i / 2).U, r.vs1, r.vs2 | i.U)
//      }
//    }
//  }
//
//  case class UopSplitVlsNonIdx() extends UopSplitBase(
//    SplitType.VLSNONIDX
//  )(
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = uops.head
//      Seq.tabulate(maxLMUL) { i =>
//        uop(r.vd | i.U, r.vs1, r.vs2)
//      }
//    }
//  }
//
//  case class UopSplitVlsNonIdxFF() extends UopSplitBase(
//    SplitType.VLSNONIDXFF
//  )(
//    uopDep = (lmul, i) => (i != 0).B
//  ) {
//    override def splitUops(r: InstLreg, enumLMUL: EnumLMUL.Type): Seq[UopBase] = {
//      val uop = uops.head
//      Seq.tabulate(maxLMUL) { i =>
//        uop(r.vd | i.U, r.vs1, r.vs2)
//      }
//    }
//  }
}
