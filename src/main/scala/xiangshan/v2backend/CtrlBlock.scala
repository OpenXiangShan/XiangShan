//package xiangshan.v2backend
//
//import chipsalliance.rocketchip.config.Parameters
//import chisel3._
//import chisel3.util._
//import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
//import utility.{DelayN, PipelineConnect, RegNextWithEnable, SyncDataModuleTemplate}
//import xiangshan._
//import xiangshan.backend.ctrlblock.{MemCtrl, RedirectGenerator}
//import xiangshan.backend.decode.DecodeStage
//import xiangshan.backend.dispatch.{Dispatch, DispatchQueue}
//import xiangshan.backend.rename.{Rename, RenameTableWrapper}
//import xiangshan.backend.rob.{Rob, RobCSRIO, RobExceptionInfo, RobLsqIO}
//import xiangshan.frontend.Ftq_RF_Components
//import xiangshan.v2backend.Bundles.{DynInst, ExceptionInfo}
//
//class CtrlBlock(backendParams: BackendParams)(implicit p: Parameters) extends LazyModule {
//  private implicit val params = backendParams
//  val rob = LazyModule(new Rob(params))
//  lazy val module = new CtrlBlockImp(this)
//}
//
//class CtrlBlockImp(
//  override val wrapper: CtrlBlock
//)(implicit
//  p: Parameters,
//  params: BackendParams
//) extends LazyModuleImp(wrapper) with HasXSParameter {
//  // bjIssueQueue.enq(4) + redirects (1) + loadPredUpdate (1) + robFlush (1)
//  private val numPcMemRead = params.numPcReadPort + 1 + 1 + 1
//  private val numTargetMemRead = params.numPcReadPort
//  private val pcMemReadIdxForRedirect = numPcMemRead
//  private val pcMemReadIdxForMemPred = numPcMemRead + 1
//  private val pcMemReadIdxForRobFlush = numPcMemRead + 2
//
//  println(s"pcMem read num: $numPcMemRead")
//
//  val io = IO(new CtrlBlockIO())
//  // Modules
//  private val decode = Module(new DecodeStage)
////  private val fusionDecoder = Module(new FusionDecoder)
//  private val rat = Module(new RenameTableWrapper)
//  private val rename = Module(new Rename)
//  private val dispatch = Module(new Dispatch)
//  private val intDq = Module(new DispatchQueue(dpParams.IntDqSize, RenameWidth, dpParams.IntDqDeqWidth))
//  private val fpDq = Module(new DispatchQueue(dpParams.FpDqSize, RenameWidth, dpParams.FpDqDeqWidth))
//  private val lsDq = Module(new DispatchQueue(dpParams.LsDqSize, RenameWidth, dpParams.LsDqDeqWidth))
//  private val redirectGen = Module(new RedirectGenerator)
//  private val rob = Module(wrapper.rob.module)
//  //  + redirects (1) + loadPredUpdate (1) + jalr_target (1) + robFlush (1)
//  private val pcMem = Module(new SyncDataModuleTemplate(new Ftq_RF_Components, FtqSize, numPcMemRead, 1, "BackendPC"))
//  private val targetMem = Module(new SyncDataModuleTemplate(UInt(VAddrData().dataWidth.W), FtqSize, numTargetMemRead, 1))
//  private val memCtrl = Module(new MemCtrl(params))
//
//  val s0_robFlushRedirect = rob.io.flushOut
//  val s1_robException = rob.io.exception
//  // T0: rob.io.flushOut, T1: robFlushRedirect, T6: frontendFlush
//  val s1_robFlushRedirect = RegNextWithEnable(s0_robFlushRedirect)
//
//  val s3_redirect = ValidIO(new Redirect)
//  val pendingRedirectValid = RegInit(false.B)
//  // Redirect will be RegNext at ExuBlocks. Todo: Why?
//  val redirectForExu = RegNextWithEnable(s3_redirect)
//
//  when (s3_redirect.valid) {
//    pendingRedirectValid := true.B
//  }.elsewhen(RegNext(io.frontend.toFtq.redirect.valid)) {
//    pendingRedirectValid := false.B
//  }
//
//  // decode io
//  decode.io.in <> io.frontend.cfVec
//  decode.io.csrCtrl := RegNext(io.csrCtrl)
//  decode.io.intRat <> rat.io.intReadPorts
//  decode.io.fpRat <> rat.io.fpReadPorts
//  decode.io.vecRat <> rat.io.vecReadPorts
//  decode.io.fusion := 0.U.asTypeOf(decode.io.fusion) // Todo
//
//  for (i <- 0 until RenameWidth) {
//    PipelineConnect(decode.io.out(i), rename.io.in(i), rename.io.in(i).ready,
//      s3_redirect.valid || pendingRedirectValid, moduleName = Some("decodePipeRename"))
//  }
//
//  rename.io.redirect := s3_redirect
//  rename.io.robCommits <> rob.io.commits
//  rename.io.fusionInfo := 0.U.asTypeOf(rename.io.fusionInfo) // Todo
//  rename.io.waittable := (memCtrl.io.waitTable2Rename zip decode.io.out).map{ case(waittable2rename, decodeOut) =>
//    RegEnable(waittable2rename, decodeOut.fire)
//  }
//  rename.io.ssit := memCtrl.io.ssit2Rename
//  rename.io.intReadPorts := rat.io.intReadPorts
//  rename.io.fpReadPorts := rat.io.fpReadPorts
//  rename.io.vecReadPorts := rat.io.vecReadPorts
//  rename.io.debug_int_rat := rat.io.debug_int_rat
//  rename.io.debug_fp_rat := rat.io.debug_fp_rat
//  rename.io.debug_vconfig_rat := rat.io.debug_vconfig_rat
//  rename.io.debug_vec_rat := rat.io.debug_vec_rat
//
//  private val mdpFlodPcVec = Wire(Vec(DecodeWidth, UInt(MemPredPCWidth.W)))
//  for (i <- 0 until DecodeWidth) {
//    mdpFlodPcVec(i) := Mux(
//      decode.io.out(i).fire,
//      decode.io.in(i).bits.foldpc,
//      rename.io.in(i).bits.foldpc
//    )
//  }
//
//  memCtrl.io.redirect <> s3_redirect
//  memCtrl.io.csrCtrl := io.csrCtrl // RegNext in memCtrl
//  memCtrl.io.stIn := io.stIn
//  memCtrl.io.memPredUpdate := redirectGen.io.memPredUpdate
//  memCtrl.io.mdpFlodPcVec := mdpFlodPcVec
//  memCtrl.io.dispatchLFSTio <> dispatch.io.lfst
//
//  rat.io.redirect := s3_redirect.valid
//  rat.io.robCommits := rob.io.commits
//  rat.io.intRenamePorts := rename.io.intRenamePorts
//  rat.io.fpRenamePorts := rename.io.fpRenamePorts
//  rat.io.vecRenamePorts := rename.io.vecRenamePorts
//
//  for (i <- 0 until RenameWidth) {
//    PipelineConnect(rename.io.out(i), dispatch.io.fromRename(i), dispatch.io.recv(i),
//      s3_redirect.valid, moduleName = Some("renamePipeDispatch"))
//  }
//  dispatch.io.hartId := io.fromTop.hartId
//  dispatch.io.redirect := s3_redirect
//  dispatch.io.singleStep := RegNext(io.csrCtrl.singlestep)
//  dispatch.io.enqRob <> rob.io.enq
//
//  intDq.io.enq <> dispatch.io.toIntDq
//  intDq.io.redirect <> redirectForExu
//
//  fpDq.io.enq <> dispatch.io.toFpDq
//  fpDq.io.redirect <> redirectForExu
//
//  lsDq.io.enq <> dispatch.io.toLsDq
//  lsDq.io.redirect <> redirectForExu
//
//  val dqOut = intDq.io.deq ++ fpDq.io.deq ++ lsDq.io.deq
//  io.toIssueBlock.dispatch <> dqOut
//
//  pcMem.io.wen.head := RegNext(io.frontend.fromFtq.pc_mem_wen)
//  pcMem.io.waddr.head := RegNext(io.frontend.fromFtq.pc_mem_waddr)
//  pcMem.io.wdata.head := RegNext(io.frontend.fromFtq.pc_mem_wdata)
//  targetMem.io.wen.head := RegNext(io.frontend.fromFtq.pc_mem_wen)
//  targetMem.io.waddr.head := RegNext(io.frontend.fromFtq.pc_mem_waddr)
//  targetMem.io.wdata.head := RegNext(io.frontend.fromFtq.pc_mem_wdata.startAddr)
//
//  val jumpPcVec = Wire(Vec(params.numPcReadPort, UInt(VAddrData().dataWidth.W)))
//  val jumpTargetVec = Wire(Vec(params.numPcReadPort, UInt(VAddrData().dataWidth.W)))
//  val jumpTargetReadVec = Wire(Vec(params.numPcReadPort, UInt(VAddrData().dataWidth.W)))
//  for (i <- 0 until params.numPcReadPort) {
//    pcMem.io.raddr(i) := intDq.io.deqNext(i).ftqPtr.value
//    jumpPcVec(i) := pcMem.io.rdata(i).getPc(RegNext(intDq.io.deqNext(i).ftqOffset))
//  }
//  val newestTarget: UInt = io.frontend.fromFtq.newest_entry_target
//  for (i <- 0 until numTargetMemRead) {
//    val targetPtr = intDq.io.deqNext(i).ftqPtr
//    // target pc stored in next entry
//    targetMem.io.raddr(i) := (targetPtr + 1.U).value
//    jumpTargetReadVec(i) := targetMem.io.rdata(i)
//    val needNewestTarget = RegNext(targetPtr === io.frontend.fromFtq.newest_entry_ptr)
//    jumpTargetVec(i) := Mux(
//      needNewestTarget,
//      RegNext(io.frontend.fromFtq.newest_entry_target),
//      jumpTargetReadVec(i)
//    )
//  }
//
//  val exuRedirects: IndexedSeq[ValidIO[Redirect]] = io.fromWB.redirect.map(x => {
//    val killedByIOrder: Bool = x.bits.robIdx.needFlush(Seq(s3_redirect, redirectForExu))
//    val delayedRedirect = Wire(ValidIO(new Redirect))
//    delayedRedirect.valid := RegNext(x.valid && !killedByIOrder)
//    delayedRedirect.bits := RegEnable(x.bits, x.valid)
//    delayedRedirect
//  })
//
//  private val memViolation = io.fromMem.violation
//  val loadReplay = Wire(ValidIO(new Redirect))
//  loadReplay.valid := RegNext(memViolation.valid &&
//    !memViolation.bits.robIdx.needFlush(Seq(s3_redirect, redirectForExu))
//  )
//  loadReplay.bits := RegEnable(memViolation.bits, memViolation.valid)
//
//  pcMem.io.raddr(pcMemReadIdxForRedirect) := redirectGen.io.redirectPcRead.ptr.value
//  redirectGen.io.redirectPcRead.data := pcMem.io.rdata(pcMemReadIdxForRedirect).getPc(RegNext(redirectGen.io.redirectPcRead.offset))
//  pcMem.io.raddr(pcMemReadIdxForMemPred) := redirectGen.io.memPredPcRead.ptr.value
//  redirectGen.io.memPredPcRead.data := pcMem.io.rdata(pcMemReadIdxForMemPred).getPc(RegNext(redirectGen.io.memPredPcRead.offset))
//  redirectGen.io.hartId := io.fromTop.hartId
//  redirectGen.io.exuRedirect := exuRedirects
//  redirectGen.io.loadReplay <> loadReplay
//  redirectGen.io.robFlushRedirect <> s1_robFlushRedirect
//
//  rob.io.hartId := io.fromTop.hartId
//  rob.io.redirect <> s3_redirect
//  rob.io.wfi_enable := decode.io.csrCtrl.wfi_enable
//  rob.io.lsq <> io.robio.lsq
//
//  pcMem.io.raddr(pcMemReadIdxForRobFlush) := s0_robFlushRedirect.bits.ftqIdx.value
//  /**
//    * T0: [[rob.io.flushOut]], [[s0_robFlushRedirect]]
//    * T1: [[s1_robFlushRedirect]], [[s1_robException]]
//    * T2: csr.redirect.valid: [[io.robio.csr.isXRet]]
//    * T3: csr.exception.valid ??
//    * T4: [[io.robio.csr.trapTarget]]
//    * T5: ctrlBlock.trapTarget
//    * T6: io.frontend.toFtq.redirect.valid
//    */
//
//  val s1_robFlushPc = pcMem.io.rdata(pcMemReadIdxForRobFlush).getPc(RegNext(s0_robFlushRedirect.bits.ftqOffset))
//  s3_redirect := redirectGen.io.stage2Redirect
//  val s6_frontendFlushValid = DelayN(s1_robFlushRedirect.valid, 5)
//  val s6_frontendFlushBits = RegEnable(s1_robFlushRedirect.bits, s1_robFlushRedirect.valid)
//
//
//  for (i <- 0 until CommitWidth) {
//    val s0_isCommit = rob.io.commits.commitValid(i) && rob.io.commits.isCommit &&
//      rob.io.commits.info(i).uopIdx.andR && !s0_robFlushRedirect.valid
//    // T0: rob commit, T1: to frontend
//    io.frontend.toFtq.rob_commits(i).valid := RegNext(s0_isCommit)
//    io.frontend.toFtq.rob_commits(i).bits := RegEnable(rob.io.commits.info(i), s0_isCommit)
//  }
//
//  io.frontend.toFtq.redirect.valid := s6_frontendFlushValid || s3_redirect.valid
//  io.frontend.toFtq.redirect.bits := Mux(
//    s6_frontendFlushValid,
//    s6_frontendFlushBits,
//    s3_redirect.bits
//  )
//
//  val s5_isPcFromCsr = io.robio.csr.isXRet || DelayN(s1_robException.valid, 4)
//  val s2_robFlushPc = RegEnable(Mux(s1_robFlushRedirect.bits.flushItself(),
//    s1_robFlushPc, // replay inst
//    s1_robFlushPc + 4.U
//  ), s1_robFlushRedirect.valid)
//  val flushTatget = Mux(s5_isPcFromCsr, io.robio.csr.trapTarget, s2_robFlushPc)
//
//  val s4_s7_pendingRedirect = RegInit(false.B)
//  when (s3_redirect.valid) {
//    s4_s7_pendingRedirect := true.B
//  }.elsewhen(RegNext(s6_frontendFlushValid || s3_redirect.valid)) {
//    s4_s7_pendingRedirect := false.B
//  }
//
//  io.redirect <> s3_redirect
//  io.robio.csr <> rob.io.csr
//  io.robio.csr.perfinfo.retiredInstr := RegNext(rob.io.csr.perfinfo.retiredInstr)
//  io.robio.exception := rob.io.exception
//}
//
//class CtrlBlockIO()(implicit p: Parameters, params: BackendParams) extends XSBundle {
//  val fromTop = new Bundle {
//    val hartId = Input(UInt(8.W))
//  }
//  val toTop = new Bundle {
//    val cpuHalt = Output(Bool())
//  }
//  val frontend = new FrontendToCtrlIO()
//  val toIssueBlock = new Bundle {
//    val allocPregs = Vec(RenameWidth, Output(new ResetPregStateReq))
//    val dispatch = Vec(dpParams.IntDqDeqWidth + dpParams.FpDqDeqWidth + dpParams.LsDqDeqWidth, DecoupledIO(new DynInst))
//  }
//  val fromWB = new Bundle {
//    val wbData = Flipped(MixedVec(params.genWrite2CtrlBundles))
//    val redirect = Flipped(Vec(params.numRedirect, ValidIO(new Redirect)))
//  }
//  val redirect = ValidIO(new Redirect)
//  val fromMem = new Bundle {
//    val stIn = Vec(params.StuCnt, Flipped(ValidIO(new DynInst))) // use storeSetHit, ssid, robIdx
//    val violation = Flipped(ValidIO(new Redirect))
//  }
//  val csrCtrl = Input(new CustomCSRCtrlIO)
//  val robio = new Bundle {
//    val csr = new RobCSRIO
//    val exception = ValidIO(new ExceptionInfo)
//    val lsq = new RobLsqIO
//  }
//}
