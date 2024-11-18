package xiangshan.frontend
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import scala.math.min
import utility._
import xiangshan._

class HistoryManager(implicit p: Parameters) extends XSModule with HasBPUConst with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val resp            = Input(new BasePredictorOutput)
    val s1_valid_dup    = Input(Vec(numDup, Bool()))
    val s0_fire_dup     = Input(Vec(numDup, Bool()))
    val s1_fire_dup     = Input(Vec(numDup, Bool()))
    val s2_fire_dup     = Input(Vec(numDup, Bool()))
    val s0_stall_dup    = Input(Vec(numDup, Bool()))
    val s2_redirect_dup = Input(Vec(numDup, Bool())) // redirect from BPU s2
    val s3_redirect_dup = Input(Vec(numDup, Bool())) // redirect from BPU s3

    val ftq_to_bpu = Flipped(new FtqToBpuIO)

    val s0_ghist         = Output(UInt(HistoryLength.W))
    val ghv_wire         = Output(Vec(HistoryLength, Bool()))
    val s0_folded_gh_dup = Output(Vec(numDup, new AllFoldedHistories(foldedGHistInfos)))
    val s1_folded_gh_dup = Output(Vec(numDup, new AllFoldedHistories(foldedGHistInfos)))
    val s3_ghist_ptr_dup = Output(Vec(numDup, new CGHPtr))
  })

  val resp            = io.resp
  val s1_valid_dup    = io.s1_valid_dup
  val s0_fire_dup     = io.s0_fire_dup
  val s1_fire_dup     = io.s1_fire_dup
  val s2_fire_dup     = io.s2_fire_dup
  val s0_stall_dup    = io.s0_stall_dup
  val s2_redirect_dup = io.s2_redirect_dup
  val s3_redirect_dup = io.s3_redirect_dup

  // redirect from Backend or IFU
  val redirect_dup      = dup_seq(RegNextWithEnable(io.ftq_to_bpu.redirect))
  val redirect_bits_dup = redirect_dup.map(_.bits)

  val s0_folded_gh_dup = dup_wire(new AllFoldedHistories(foldedGHistInfos))
  val s0_folded_gh_reg_dup = s0_folded_gh_dup.zip(s0_stall_dup).map {
    case (x, s0_stall) => RegEnable(x, 0.U.asTypeOf(s0_folded_gh_dup(0)), !s0_stall)
  }
  val s1_folded_gh_dup = RegEnable(s0_folded_gh_dup, 0.U.asTypeOf(s0_folded_gh_dup), s0_fire_dup(1))
  val s2_folded_gh_dup = RegEnable(s1_folded_gh_dup, 0.U.asTypeOf(s0_folded_gh_dup), s1_fire_dup(1))
  val s3_folded_gh_dup = RegEnable(s2_folded_gh_dup, 0.U.asTypeOf(s0_folded_gh_dup), s2_fire_dup(1))

  io.s0_folded_gh_dup := s0_folded_gh_dup
  io.s1_folded_gh_dup := s1_folded_gh_dup

  val s0_last_br_num_oh_dup = dup_wire(UInt((numBr + 1).W))
  val s0_last_br_num_oh_reg_dup = s0_last_br_num_oh_dup.zip(s0_stall_dup).map {
    case (x, s0_stall) => RegEnable(x, 0.U, !s0_stall)
  }
  val s1_last_br_num_oh_dup = RegEnable(s0_last_br_num_oh_dup, 0.U.asTypeOf(s0_last_br_num_oh_dup), s0_fire_dup(1))
  val s2_last_br_num_oh_dup = RegEnable(s1_last_br_num_oh_dup, 0.U.asTypeOf(s0_last_br_num_oh_dup), s1_fire_dup(1))
  val s3_last_br_num_oh_dup = RegEnable(s2_last_br_num_oh_dup, 0.U.asTypeOf(s0_last_br_num_oh_dup), s2_fire_dup(1))

  val s0_ahead_fh_oldest_bits_dup = dup_wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  val s0_ahead_fh_oldest_bits_reg_dup = s0_ahead_fh_oldest_bits_dup.zip(s0_stall_dup).map {
    case (x, s0_stall) => RegEnable(x, 0.U.asTypeOf(s0_ahead_fh_oldest_bits_dup(0)), !s0_stall)
  }
  val s1_ahead_fh_oldest_bits_dup =
    RegEnable(s0_ahead_fh_oldest_bits_dup, 0.U.asTypeOf(s0_ahead_fh_oldest_bits_dup), s0_fire_dup(1))
  val s2_ahead_fh_oldest_bits_dup =
    RegEnable(s1_ahead_fh_oldest_bits_dup, 0.U.asTypeOf(s0_ahead_fh_oldest_bits_dup), s1_fire_dup(1))
  val s3_ahead_fh_oldest_bits_dup =
    RegEnable(s2_ahead_fh_oldest_bits_dup, 0.U.asTypeOf(s0_ahead_fh_oldest_bits_dup), s2_fire_dup(1))

  val foldedGhGen_dup    = Seq.tabulate(numDup)(n => new PhyPriorityMuxGenerator[AllFoldedHistories])
  val ghistPtrGen_dup    = Seq.tabulate(numDup)(n => new PhyPriorityMuxGenerator[CGHPtr])
  val lastBrNumOHGen_dup = Seq.tabulate(numDup)(n => new PhyPriorityMuxGenerator[UInt])
  val aheadFhObGen_dup   = Seq.tabulate(numDup)(n => new PhyPriorityMuxGenerator[AllAheadFoldedHistoryOldestBits])

  val ghvBitWriteGens = Seq.tabulate(HistoryLength)(n => new PhyPriorityMuxGenerator[Bool])
  // val ghistGen = new PhyPriorityMuxGenerator[UInt]

  // global history
  val ghv      = RegInit(0.U.asTypeOf(Vec(HistoryLength, Bool())))
  val ghv_wire = WireInit(ghv)

  io.ghv_wire := ghv_wire

  val s0_ghist = WireInit(0.U.asTypeOf(UInt(HistoryLength.W)))

  io.s0_ghist := s0_ghist

  println(f"history buffer length ${HistoryLength}")
  val ghv_write_datas = Wire(Vec(HistoryLength, Bool()))
  val ghv_wens        = Wire(Vec(HistoryLength, Bool()))

  val s0_ghist_ptr_dup = dup_wire(new CGHPtr)
  val s0_ghist_ptr_reg_dup = s0_ghist_ptr_dup.zip(s0_stall_dup).map {
    case (x, s0_stall) => RegEnable(x, 0.U.asTypeOf(new CGHPtr), !s0_stall)
  }
  val s1_ghist_ptr_dup = RegEnable(s0_ghist_ptr_dup, 0.U.asTypeOf(s0_ghist_ptr_dup), s0_fire_dup(1))
  val s2_ghist_ptr_dup = RegEnable(s1_ghist_ptr_dup, 0.U.asTypeOf(s0_ghist_ptr_dup), s1_fire_dup(1))
  val s3_ghist_ptr_dup = RegEnable(s2_ghist_ptr_dup, 0.U.asTypeOf(s0_ghist_ptr_dup), s2_fire_dup(1))

  io.s3_ghist_ptr_dup := s3_ghist_ptr_dup

  def getHist(ptr: CGHPtr): UInt = (Cat(ghv_wire.asUInt, ghv_wire.asUInt) >> (ptr.value + 1.U))(HistoryLength - 1, 0)
  s0_ghist := getHist(s0_ghist_ptr_dup(0))

  foldedGhGen_dup.zip(s0_folded_gh_reg_dup).map { case (gen, reg) =>
    gen.register(true.B, reg, Some("stallFGH"), 0)
  }
  ghistPtrGen_dup.zip(s0_ghist_ptr_reg_dup).map { case (gen, reg) =>
    gen.register(true.B, reg, Some("stallGHPtr"), 0)
  }
  lastBrNumOHGen_dup.zip(s0_last_br_num_oh_reg_dup).map { case (gen, reg) =>
    gen.register(true.B, reg, Some("stallBrNumOH"), 0)
  }
  aheadFhObGen_dup.zip(s0_ahead_fh_oldest_bits_reg_dup).map { case (gen, reg) =>
    gen.register(true.B, reg, Some("stallAFHOB"), 0)
  }

  // ****************************************************************************************************
  //                                               stage 1
  // ****************************************************************************************************

  val s1_possible_predicted_ghist_ptrs_dup = s1_ghist_ptr_dup.map(ptr => (0 to numBr).map(ptr - _.U))
  val s1_predicted_ghist_ptr_dup = s1_possible_predicted_ghist_ptrs_dup.zip(resp.s1.lastBrPosOH).map { case (ptr, oh) =>
    Mux1H(oh, ptr)
  }
  val s1_possible_predicted_fhs_dup =
    for (
      ((((fgh, afh), br_num_oh), t), br_pos_oh) <-
        s1_folded_gh_dup zip s1_ahead_fh_oldest_bits_dup zip s1_last_br_num_oh_dup zip resp.s1.brTaken zip resp.s1.lastBrPosOH
    )
      yield (0 to numBr).map(i =>
        fgh.update(afh, br_num_oh, i, t & br_pos_oh(i))
      )
  val s1_predicted_fh_dup = resp.s1.lastBrPosOH.zip(s1_possible_predicted_fhs_dup).map { case (oh, fh) =>
    Mux1H(oh, fh)
  }

  val s1_ahead_fh_ob_src_dup = dup_wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  s1_ahead_fh_ob_src_dup.zip(s1_ghist_ptr_dup).map { case (src, ptr) => src.read(ghv, ptr) }

  if (EnableGHistDiff) {
    val s1_predicted_ghist = WireInit(getHist(s1_predicted_ghist_ptr_dup(0)).asTypeOf(Vec(HistoryLength, Bool())))
    for (i <- 0 until numBr) {
      when(resp.s1.shouldShiftVec(0)(i)) {
        s1_predicted_ghist(i) := resp.s1.brTaken(0) && (i == 0).B
      }
    }
    when(s1_valid_dup(0)) {
      s0_ghist := s1_predicted_ghist.asUInt
    }
  }

  val s1_ghv_wens = (0 until HistoryLength).map(n =>
    (0 until numBr).map(b =>
      s1_ghist_ptr_dup(0).value === (CGHPtr(false.B, n.U) + b.U).value &&
        resp.s1.shouldShiftVec(0)(b) && s1_valid_dup(0)
    )
  )
  val s1_ghv_wdatas = (0 until HistoryLength).map(n =>
    Mux1H(
      (0 until numBr).map(b =>
        (
          s1_ghist_ptr_dup(0).value === (CGHPtr(false.B, n.U) + b.U).value && resp.s1.shouldShiftVec(0)(b),
          resp.s1.brTaken(0) && resp.s1.lastBrPosOH(0)(b + 1)
        )
      )
    )
  )

  for (((foldedGhGen, s1_valid), s1_predicted_fh) <- foldedGhGen_dup zip s1_valid_dup zip s1_predicted_fh_dup)
    foldedGhGen.register(s1_valid, s1_predicted_fh, Some("s1_FGH"), 4)
  for (
    ((ghistPtrGen, s1_valid), s1_predicted_ghist_ptr) <- ghistPtrGen_dup zip s1_valid_dup zip s1_predicted_ghist_ptr_dup
  )
    ghistPtrGen.register(s1_valid, s1_predicted_ghist_ptr, Some("s1_GHPtr"), 4)
  for (
    ((lastBrNumOHGen, s1_valid), s1_brPosOH) <-
      lastBrNumOHGen_dup zip s1_valid_dup zip resp.s1.lastBrPosOH.map(_.asUInt)
  )
    lastBrNumOHGen.register(s1_valid, s1_brPosOH, Some("s1_BrNumOH"), 4)
  for (((aheadFhObGen, s1_valid), s1_ahead_fh_ob_src) <- aheadFhObGen_dup zip s1_valid_dup zip s1_ahead_fh_ob_src_dup)
    aheadFhObGen.register(s1_valid, s1_ahead_fh_ob_src, Some("s1_AFHOB"), 4)
  ghvBitWriteGens.zip(s1_ghv_wens).zipWithIndex.map { case ((b, w), i) =>
    b.register(w.reduce(_ || _), s1_ghv_wdatas(i), Some(s"s1_new_bit_$i"), 4)
  }

  // ****************************************************************************************************
  //                                               stage 2
  // ****************************************************************************************************

  val s2_possible_predicted_ghist_ptrs_dup = s2_ghist_ptr_dup.map(ptr => (0 to numBr).map(ptr - _.U))
  val s2_predicted_ghist_ptr_dup = s2_possible_predicted_ghist_ptrs_dup.zip(resp.s2.lastBrPosOH).map { case (ptr, oh) =>
    Mux1H(oh, ptr)
  }

  val s2_possible_predicted_fhs_dup =
    for (
      (((fgh, afh), br_num_oh), full_pred) <-
        s2_folded_gh_dup zip s2_ahead_fh_oldest_bits_dup zip s2_last_br_num_oh_dup zip resp.s2.full_pred
    )
      yield (0 to numBr).map(i =>
        fgh.update(afh, br_num_oh, i, if (i > 0) full_pred.br_taken_mask(i - 1) else false.B)
      )
  val s2_predicted_fh_dup = resp.s2.lastBrPosOH.zip(s2_possible_predicted_fhs_dup).map { case (oh, fh) =>
    Mux1H(oh, fh)
  }

  val s2_ahead_fh_ob_src_dup = dup_wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  s2_ahead_fh_ob_src_dup.zip(s2_ghist_ptr_dup).map { case (src, ptr) => src.read(ghv, ptr) }

  if (EnableGHistDiff) {
    val s2_predicted_ghist = WireInit(getHist(s2_predicted_ghist_ptr_dup(0)).asTypeOf(Vec(HistoryLength, Bool())))
    for (i <- 0 until numBr) {
      when(resp.s2.shouldShiftVec(0)(i)) {
        s2_predicted_ghist(i) := resp.s2.brTaken(0) && (i == 0).B
      }
    }
    when(s2_redirect_dup(0)) {
      s0_ghist := s2_predicted_ghist.asUInt
    }
  }

  val s2_ghv_wens = (0 until HistoryLength).map(n =>
    (0 until numBr).map(b =>
      s2_ghist_ptr_dup(0).value === (CGHPtr(false.B, n.U) + b.U).value &&
        resp.s2.shouldShiftVec(0)(b) && s2_redirect_dup(0)
    )
  )
  val s2_ghv_wdatas = (0 until HistoryLength).map(n =>
    Mux1H(
      (0 until numBr).map(b =>
        (
          s2_ghist_ptr_dup(0).value === (CGHPtr(false.B, n.U) + b.U).value && resp.s2.shouldShiftVec(0)(b),
          resp.s2.full_pred(0).real_br_taken_mask()(b)
        )
      )
    )
  )

  for (((foldedGhGen, s2_redirect), s2_predicted_fh) <- foldedGhGen_dup zip s2_redirect_dup zip s2_predicted_fh_dup)
    foldedGhGen.register(s2_redirect, s2_predicted_fh, Some("s2_FGH"), 5)
  for (
    ((ghistPtrGen, s2_redirect), s2_predicted_ghist_ptr) <-
      ghistPtrGen_dup zip s2_redirect_dup zip s2_predicted_ghist_ptr_dup
  )
    ghistPtrGen.register(s2_redirect, s2_predicted_ghist_ptr, Some("s2_GHPtr"), 5)
  for (
    ((lastBrNumOHGen, s2_redirect), s2_brPosOH) <-
      lastBrNumOHGen_dup zip s2_redirect_dup zip resp.s2.lastBrPosOH.map(_.asUInt)
  )
    lastBrNumOHGen.register(s2_redirect, s2_brPosOH, Some("s2_BrNumOH"), 5)
  for (
    ((aheadFhObGen, s2_redirect), s2_ahead_fh_ob_src) <- aheadFhObGen_dup zip s2_redirect_dup zip s2_ahead_fh_ob_src_dup
  )
    aheadFhObGen.register(s2_redirect, s2_ahead_fh_ob_src, Some("s2_AFHOB"), 5)
  ghvBitWriteGens.zip(s2_ghv_wens).zipWithIndex.map { case ((b, w), i) =>
    b.register(w.reduce(_ || _), s2_ghv_wdatas(i), Some(s"s2_new_bit_$i"), 5)
  }

  // ****************************************************************************************************
  //                                                stage 3
  // ****************************************************************************************************

  val s3_possible_predicted_ghist_ptrs_dup = s3_ghist_ptr_dup.map(ptr => (0 to numBr).map(ptr - _.U))
  val s3_predicted_ghist_ptr_dup = s3_possible_predicted_ghist_ptrs_dup.zip(resp.s3.lastBrPosOH).map { case (ptr, oh) =>
    Mux1H(oh, ptr)
  }

  val s3_possible_predicted_fhs_dup =
    for (
      (((fgh, afh), br_num_oh), full_pred) <-
        s3_folded_gh_dup zip s3_ahead_fh_oldest_bits_dup zip s3_last_br_num_oh_dup zip resp.s3.full_pred
    )
      yield (0 to numBr).map(i =>
        fgh.update(afh, br_num_oh, i, if (i > 0) full_pred.br_taken_mask(i - 1) else false.B)
      )
  val s3_predicted_fh_dup = resp.s3.lastBrPosOH.zip(s3_possible_predicted_fhs_dup).map { case (oh, fh) =>
    Mux1H(oh, fh)
  }

  val s3_ahead_fh_ob_src_dup = dup_wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  s3_ahead_fh_ob_src_dup.zip(s3_ghist_ptr_dup).map { case (src, ptr) => src.read(ghv, ptr) }

  if (EnableGHistDiff) {
    val s3_predicted_ghist = WireInit(getHist(s3_predicted_ghist_ptr_dup(0)).asTypeOf(Vec(HistoryLength, Bool())))
    for (i <- 0 until numBr) {
      when(resp.s3.shouldShiftVec(0)(i)) {
        s3_predicted_ghist(i) := resp.s3.brTaken(0) && (i == 0).B
      }
    }
    when(s3_redirect_dup(0)) {
      s0_ghist := s3_predicted_ghist.asUInt
    }
  }

  val s3_ghv_wens = (0 until HistoryLength).map(n =>
    (0 until numBr).map(b =>
      s3_ghist_ptr_dup(0).value === (CGHPtr(false.B, n.U) + b.U).value && resp.s3.shouldShiftVec(0)(
        b
      ) && s3_redirect_dup(0)
    )
  )
  val s3_ghv_wdatas = (0 until HistoryLength).map(n =>
    Mux1H(
      (0 until numBr).map(b =>
        (
          s3_ghist_ptr_dup(0).value === (CGHPtr(false.B, n.U) + b.U).value && resp.s3.shouldShiftVec(0)(b),
          resp.s3.full_pred(0).real_br_taken_mask()(b)
        )
      )
    )
  )

  for (((foldedGhGen, s3_redirect), s3_predicted_fh) <- foldedGhGen_dup zip s3_redirect_dup zip s3_predicted_fh_dup)
    foldedGhGen.register(s3_redirect, s3_predicted_fh, Some("s3_FGH"), 3)
  for (
    ((ghistPtrGen, s3_redirect), s3_predicted_ghist_ptr) <-
      ghistPtrGen_dup zip s3_redirect_dup zip s3_predicted_ghist_ptr_dup
  )
    ghistPtrGen.register(s3_redirect, s3_predicted_ghist_ptr, Some("s3_GHPtr"), 3)
  for (
    ((lastBrNumOHGen, s3_redirect), s3_brPosOH) <-
      lastBrNumOHGen_dup zip s3_redirect_dup zip resp.s3.lastBrPosOH.map(_.asUInt)
  )
    lastBrNumOHGen.register(s3_redirect, s3_brPosOH, Some("s3_BrNumOH"), 3)
  for (
    ((aheadFhObGen, s3_redirect), s3_ahead_fh_ob_src) <- aheadFhObGen_dup zip s3_redirect_dup zip s3_ahead_fh_ob_src_dup
  )
    aheadFhObGen.register(s3_redirect, s3_ahead_fh_ob_src, Some("s3_AFHOB"), 3)
  ghvBitWriteGens.zip(s3_ghv_wens).zipWithIndex.map { case ((b, w), i) =>
    b.register(w.reduce(_ || _), s3_ghv_wdatas(i), Some(s"s3_new_bit_$i"), 3)
  }

  // ****************************************************************************************************
  //                                            redirect logic
  // ****************************************************************************************************

  val shift_dup       = redirect_bits_dup.map(_.cfiUpdate.shift)
  val addIntoHist_dup = redirect_bits_dup.map(_.cfiUpdate.addIntoHist)
  // TODO: remove these below
  val shouldShiftVec_dup = shift_dup.map(shift =>
    Mux(
      shift === 0.U,
      VecInit(0.U((1 << (log2Ceil(numBr) + 1)).W).asBools),
      VecInit(LowerMask(1.U << (shift - 1.U)).asBools)
    )
  )
  // TODO end
  val afhob_dup       = redirect_bits_dup.map(_.cfiUpdate.afhob)
  val lastBrNumOH_dup = redirect_bits_dup.map(_.cfiUpdate.lastBrNumOH)

  val isBr_dup  = redirect_bits_dup.map(_.cfiUpdate.pd.isBr)
  val taken_dup = redirect_bits_dup.map(_.cfiUpdate.taken)
  val real_br_taken_mask_dup =
    for (((shift, taken), addIntoHist) <- shift_dup zip taken_dup zip addIntoHist_dup)
      yield (0 until numBr).map(i => shift === (i + 1).U && taken && addIntoHist)

  val oldPtr_dup      = redirect_bits_dup.map(_.cfiUpdate.histPtr)
  val updated_ptr_dup = oldPtr_dup.zip(shift_dup).map { case (oldPtr, shift) => oldPtr - shift }
  def computeFoldedHist(hist: UInt, compLen: Int)(histLen: Int): UInt =
    if (histLen > 0) {
      val nChunks     = (histLen + compLen - 1) / compLen
      val hist_chunks = (0 until nChunks) map { i => hist(min((i + 1) * compLen, histLen) - 1, i * compLen) }
      ParallelXOR(hist_chunks)
    } else 0.U

  val oldFh_dup = dup_seq(WireInit(0.U.asTypeOf(new AllFoldedHistories(foldedGHistInfos))))
  oldFh_dup.zip(oldPtr_dup).map { case (oldFh, oldPtr) =>
    foldedGHistInfos.foreach { case (histLen, compLen) =>
      oldFh.getHistWithInfo((histLen, compLen)).folded_hist := computeFoldedHist(getHist(oldPtr), compLen)(histLen)
    }
  }

  val updated_fh_dup =
    for (
      ((((oldFh, oldPtr), taken), addIntoHist), shift) <-
        oldFh_dup zip oldPtr_dup zip taken_dup zip addIntoHist_dup zip shift_dup
    )
      yield VecInit((0 to numBr).map(i => oldFh.update(ghv, oldPtr, i, taken && addIntoHist)))(shift)
  val thisBrNumOH_dup   = shift_dup.map(shift => UIntToOH(shift, numBr + 1))
  val thisAheadFhOb_dup = dup_wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  thisAheadFhOb_dup.zip(oldPtr_dup).map { case (afhob, oldPtr) => afhob.read(ghv, oldPtr) }
  val redirect_ghv_wens = (0 until HistoryLength).map(n =>
    (0 until numBr).map(b =>
      oldPtr_dup(0).value === (CGHPtr(false.B, n.U) + b.U).value && shouldShiftVec_dup(0)(b) && redirect_dup(0).valid
    )
  )
  val redirect_ghv_wdatas = (0 until HistoryLength).map(n =>
    Mux1H(
      (0 until numBr).map(b => oldPtr_dup(0).value === (CGHPtr(false.B, n.U) + b.U).value && shouldShiftVec_dup(0)(b)),
      real_br_taken_mask_dup(0)
    )
  )

  if (EnableGHistDiff) {
    val updated_ghist = WireInit(getHist(updated_ptr_dup(0)).asTypeOf(Vec(HistoryLength, Bool())))
    for (i <- 0 until numBr) {
      when(shift_dup(0) >= (i + 1).U) {
        updated_ghist(i) := taken_dup(0) && addIntoHist_dup(0) && (i == 0).B
      }
    }
    when(redirect_dup(0).valid) {
      s0_ghist := updated_ghist.asUInt
    }
  }

  // Commit time history checker
  if (EnableCommitGHistDiff) {
    val commitGHist    = RegInit(0.U.asTypeOf(Vec(HistoryLength, Bool())))
    val commitGHistPtr = RegInit(0.U.asTypeOf(new CGHPtr))
    def getCommitHist(ptr: CGHPtr): UInt =
      (Cat(commitGHist.asUInt, commitGHist.asUInt) >> (ptr.value + 1.U))(HistoryLength - 1, 0)

    val updateValid:         Bool      = io.ftq_to_bpu.update.valid
    val branchValidMask:     UInt      = io.ftq_to_bpu.update.bits.ftb_entry.brValids.asUInt
    val branchCommittedMask: Vec[Bool] = io.ftq_to_bpu.update.bits.br_committed
    val misPredictMask:      UInt      = io.ftq_to_bpu.update.bits.mispred_mask.asUInt
    val takenMask: UInt =
      io.ftq_to_bpu.update.bits.br_taken_mask.asUInt |
        io.ftq_to_bpu.update.bits.ftb_entry.strong_bias.asUInt // Always taken branch is recorded in history
    val takenIdx:      UInt = (PriorityEncoder(takenMask) + 1.U((log2Ceil(numBr) + 1).W)).asUInt
    val misPredictIdx: UInt = (PriorityEncoder(misPredictMask) + 1.U((log2Ceil(numBr) + 1).W)).asUInt
    val shouldShiftMask: UInt = Mux(takenMask.orR, LowerMask(takenIdx).asUInt, ((1 << numBr) - 1).asUInt) &
      Mux(misPredictMask.orR, LowerMask(misPredictIdx).asUInt, ((1 << numBr) - 1).asUInt) &
      branchCommittedMask.asUInt
    val updateShift: UInt =
      Mux(updateValid && branchValidMask.orR, PopCount(branchValidMask & shouldShiftMask), 0.U)

    // Maintain the commitGHist
    for (i <- 0 until numBr) {
      when(updateShift >= (i + 1).U) {
        val ptr: CGHPtr = commitGHistPtr - i.asUInt
        commitGHist(ptr.value) := takenMask(i)
      }
    }
    when(updateValid) {
      commitGHistPtr := commitGHistPtr - updateShift
    }

    // Calculate true history using Parallel XOR
    // Do differential
    TageTableInfos.map {
      case (nRows, histLen, _) => {
        val nRowsPerBr      = nRows / numBr
        val predictGHistPtr = io.ftq_to_bpu.update.bits.spec_info.histPtr
        val commitTrueHist: UInt = computeFoldedHist(getCommitHist(commitGHistPtr), log2Ceil(nRowsPerBr))(histLen)
        val predictFHist:   UInt = computeFoldedHist(getHist(predictGHistPtr), log2Ceil(nRowsPerBr))(histLen)
        XSWarn(
          updateValid && predictFHist =/= commitTrueHist,
          p"predict time ghist: ${predictFHist} is different from commit time: ${commitTrueHist}\n"
        )
      }
    }
  }

  for (((foldedGhGen, redirect), updated_fh) <- foldedGhGen_dup zip redirect_dup zip updated_fh_dup)
    foldedGhGen.register(redirect.valid, updated_fh, Some("redirect_FGHT"), 2)
  for (((ghistPtrGen, redirect), updated_ptr) <- ghistPtrGen_dup zip redirect_dup zip updated_ptr_dup)
    ghistPtrGen.register(redirect.valid, updated_ptr, Some("redirect_GHPtr"), 2)
  for (((lastBrNumOHGen, redirect), thisBrNumOH) <- lastBrNumOHGen_dup zip redirect_dup zip thisBrNumOH_dup)
    lastBrNumOHGen.register(redirect.valid, thisBrNumOH, Some("redirect_BrNumOH"), 2)
  for (((aheadFhObGen, redirect), thisAheadFhOb) <- aheadFhObGen_dup zip redirect_dup zip thisAheadFhOb_dup)
    aheadFhObGen.register(redirect.valid, thisAheadFhOb, Some("redirect_AFHOB"), 2)
  ghvBitWriteGens.zip(redirect_ghv_wens).zipWithIndex.map { case ((b, w), i) =>
    b.register(w.reduce(_ || _), redirect_ghv_wdatas(i), Some(s"redirect_new_bit_$i"), 2)
  }

  s0_folded_gh_dup.zip(foldedGhGen_dup).map { case (s0_folded_gh, foldedGhGen) => s0_folded_gh := foldedGhGen() }
  s0_ghist_ptr_dup.zip(ghistPtrGen_dup).map { case (s0_ghist_ptr, ghistPtrGen) => s0_ghist_ptr := ghistPtrGen() }
  s0_ahead_fh_oldest_bits_dup.zip(aheadFhObGen_dup).map { case (s0_ahead_fh_oldest_bits, aheadFhObGen) =>
    s0_ahead_fh_oldest_bits := aheadFhObGen()
  }
  s0_last_br_num_oh_dup.zip(lastBrNumOHGen_dup).map { case (s0_last_br_num_oh, lastBrNumOHGen) =>
    s0_last_br_num_oh := lastBrNumOHGen()
  }
  (ghv_write_datas zip ghvBitWriteGens).map { case (wd, d) => wd := d() }
  for (i <- 0 until HistoryLength) {
    ghv_wens(i) := Seq(s1_ghv_wens, s2_ghv_wens, s3_ghv_wens, redirect_ghv_wens).map(_(i).reduce(_ || _)).reduce(_ || _)
    when(ghv_wens(i)) {
      ghv(i) := ghv_write_datas(i)
    }
  }

  // ****************************************************************************************************
  //                                              debug
  // ****************************************************************************************************

  XSError(
    isBefore(redirect_bits_dup(0).cfiUpdate.histPtr, s3_ghist_ptr_dup(0)) && redirect_dup(0).valid,
    p"s3_ghist_ptr ${s3_ghist_ptr_dup(0)} exceeds redirect histPtr ${redirect_bits_dup(0).cfiUpdate.histPtr}\n"
  )
  XSError(
    isBefore(redirect_bits_dup(0).cfiUpdate.histPtr, s2_ghist_ptr_dup(0)) && redirect_dup(0).valid,
    p"s2_ghist_ptr ${s2_ghist_ptr_dup(0)} exceeds redirect histPtr ${redirect_bits_dup(0).cfiUpdate.histPtr}\n"
  )
  XSError(
    isBefore(redirect_bits_dup(0).cfiUpdate.histPtr, s1_ghist_ptr_dup(0)) && redirect_dup(0).valid,
    p"s1_ghist_ptr ${s1_ghist_ptr_dup(0)} exceeds redirect histPtr ${redirect_bits_dup(0).cfiUpdate.histPtr}\n"
  )

  XSDebug(p"s0_ghist_ptr: ${s0_ghist_ptr_dup(0)}\n")
  XSDebug(p"s1_ghist_ptr: ${s1_ghist_ptr_dup(0)}\n")
  XSDebug(p"s2_ghist_ptr: ${s2_ghist_ptr_dup(0)}\n")
  XSDebug(p"s3_ghist_ptr: ${s3_ghist_ptr_dup(0)}\n")
}
