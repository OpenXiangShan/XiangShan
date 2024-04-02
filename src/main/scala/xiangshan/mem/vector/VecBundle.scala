/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.rob.RobPtr

class VLSBundle(isVStore: Boolean=false)(implicit p: Parameters) extends VLSUBundle {
  val flowMask            = UInt(VLENB.W) // each bit for a flow
  val byteMask            = UInt(VLENB.W) // each bit for a byte
  val data                = UInt(VLEN.W)
  // val fof            = Bool() // fof is only used for vector loads
  val excp_eew_index      = UInt(elemIdxBits.W)
  // val exceptionVec   = ExceptionVec() // uop has exceptionVec
  val baseAddr            = UInt(VAddrBits.W)
  val stride              = UInt(VLEN.W)
  // val flow_counter = UInt(flowIdxBits.W)

  // instruction decode result
  val flowNum             = UInt(flowIdxBits.W) // # of flows in a uop
  // val flowNumLog2 = UInt(log2Up(flowIdxBits).W) // log2(flowNum), for better timing of multiplication
  val nfields             = UInt(fieldBits.W) // NFIELDS
  val vm                  = Bool() // whether vector masking is enabled
  val usWholeReg          = Bool() // unit-stride, whole register load
  val usMaskReg           = Bool() // unit-stride, masked store/load
  val eew                 = UInt(ewBits.W) // size of memory elements
  val sew                 = UInt(ewBits.W)
  val emul                = UInt(mulBits.W)
  val lmul                = UInt(mulBits.W)
  val vlmax               = UInt(elemIdxBits.W)
  val instType            = UInt(3.W)
  val vd_last_uop         = Bool()
  val vd_first_uop        = Bool()
  // Inst's uop
  val uop                 = new DynInst

  val fof                 = Bool()
  val vdIdxInField        = UInt(log2Up(maxMUL).W)
  val uopOffset           = UInt(VLEN.W)
  val preIsSplit          = Bool() // if uop need split, only not Unit-Stride or not 128bit-aligned unit stride need split
  val mBIndex             = if(isVStore) UInt(vsmBindexBits.W) else UInt(vlmBindexBits.W)

  val alignedType         = UInt(alignTypeBits.W)
}

object VSFQFeedbackType {
  val tlbMiss = 0.U(3.W)
  val mshrFull = 1.U(3.W)
  val dataInvalid = 2.U(3.W)
  val bankConflict = 3.U(3.W)
  val ldVioCheckRedo = 4.U(3.W)
  val feedbackInvalid = 7.U(3.W)

  def apply() = UInt(3.W)
}

class VSFQFeedback (implicit p: Parameters) extends XSBundle {
  // val flowPtr = new VsFlowPtr
  val hit   = Bool()
  //val flushState = Bool()
  val sourceType = VSFQFeedbackType()
  //val dataInvalidSqIdx = new SqPtr
  val paddr = UInt(PAddrBits.W)
  val mmio = Bool()
  val atomic = Bool()
  val exceptionVec = ExceptionVec()
}

class VecPipelineFeedbackIO(isVStore: Boolean=false) (implicit p: Parameters) extends VLSUBundle {
  val mBIndex              = if(isVStore) UInt(vsmBindexBits.W) else UInt(vlmBindexBits.W)
  val hit                  = Bool()
  val isvec                = Bool()
  val flushState           = Bool()
  val sourceType           = VSFQFeedbackType()
  //val dataInvalidSqIdx = new SqPtr
  //val paddr                = UInt(PAddrBits.W)
  val mmio                 = Bool()
  //val atomic               = Bool()
  val exceptionVec         = ExceptionVec()
  //val vec                  = new OnlyVecExuOutput
   // feedback
  val vecFeedback          = Bool()

  val usSecondInv          = Bool() // only for unit stride, second flow is Invalid
  // for load
  val reg_offset           = OptionWrapper(!isVStore, UInt(vOffsetBits.W))
  val elemIdx              = OptionWrapper(!isVStore, UInt(elemIdxBits.W)) // element index
  val elemIdxInsideVd      = OptionWrapper(!isVStore, UInt(elemIdxBits.W)) // element index in scope of vd
  val vecdata              = OptionWrapper(!isVStore, UInt(VLEN.W))
  val mask                 = OptionWrapper(!isVStore, UInt(VLENB.W))
  val alignedType          = OptionWrapper(!isVStore, UInt(alignTypeBits.W))
}

class VecPipeBundle(isVStore: Boolean=false)(implicit p: Parameters) extends VLSUBundle {
  val vaddr               = UInt(VAddrBits.W)
  val mask                = UInt(VLENB.W)
  val isvec               = Bool()
  val uop_unit_stride_fof = Bool()
  val reg_offset          = UInt(vOffsetBits.W)
  val alignedType         = UInt(alignTypeBits.W)
  val vecActive           = Bool() // 1: vector active element, 0: vector not active element
  val is_first_ele        = Bool()
  val isFirstIssue        = Bool()

  val uop = new DynInst

  val usSecondInv         = Bool() // only for unit stride, second flow is Invalid
  val mBIndex             = if(isVStore) UInt(vsmBindexBits.W) else UInt(vlmBindexBits.W)
  val elemIdx             = UInt(elemIdxBits.W)
}

object VecFeedbacks {
  // need to invalid lsq entry
  val FLUSH  = 0
  // merge buffer commits one uop
  val COMMIT  = 1
  // last uop of an inst, sq can commit
  val LAST = 2
  // total feedbacks
  val allFeedbacks = 3
}

class MergeBufferReq(isVStore: Boolean=false)(implicit p: Parameters) extends VLSUBundle{
  val mask                = UInt(VLENB.W)
  val vaddr               = UInt(VAddrBits.W)
  val flowNum             = UInt(flowIdxBits.W)
  val uop                 = new DynInst
  val data                = UInt(VLEN.W)
  // val vdOffset            = UInt(vdOffset.W)
}

class MergeBufferResp(isVStore: Boolean=false)(implicit p: Parameters) extends VLSUBundle{
  val mBIndex             = if(isVStore) UInt(vsmBindexBits.W) else UInt(vlmBindexBits.W)
  val fail                = Bool()
}

class ToMergeBufferIO(isVStore: Boolean=false)(implicit p: Parameters) extends VLSUBundle{
  val req                 = DecoupledIO(new MergeBufferReq(isVStore))
  val resp                = Flipped(ValidIO(new MergeBufferResp(isVStore)))
  // val issueInactive       = ValidIO
}

class FromSplitIO(isVStore: Boolean=false)(implicit p: Parameters) extends VLSUBundle{
  val req                 = Flipped(DecoupledIO(new MergeBufferReq(isVStore)))
  val resp                = ValidIO(new MergeBufferResp(isVStore))
  // val issueInactive       = Flipped(ValidIO())
}

class FeedbackToSplitIO(implicit p: Parameters) extends VLSUBundle{
  val elemWriteback       = Bool()
}

class FeedbackToLsqIO(implicit p: Parameters) extends VLSUBundle{
  val robidx = new RobPtr
  val uopidx = UopIdx()
  val vaddr = UInt(VAddrBits.W)
  val feedback = Vec(VecFeedbacks.allFeedbacks, Bool())

  def isFlush  = feedback(VecFeedbacks.FLUSH)
  def isCommit = feedback(VecFeedbacks.COMMIT)
  def isLast = feedback(VecFeedbacks.LAST)
}

class VSplitIO(isVStore: Boolean=false)(implicit p: Parameters) extends VLSUBundle{
  val redirect            = Flipped(ValidIO(new Redirect))
  val in                  = if(isVStore) Flipped(Decoupled(new MemExuInput(isVector = true))) else Flipped(Decoupled(new MemExuInput(isVector = true))) // from iq
  val toMergeBuffer       = new ToMergeBufferIO(isVStore) //to merge buffer req mergebuffer entry
  val out                 = Decoupled(new VecPipeBundle(isVStore))// to scala pipeline
  val vstd                = OptionWrapper(isVStore, Valid(new MemExuOutput(isVector = true)))
}

class VSplitPipelineIO(isVStore: Boolean=false)(implicit p: Parameters) extends VLSUBundle{
  val redirect            = Flipped(ValidIO(new Redirect))
  val in                  = Flipped(Decoupled(new MemExuInput(isVector = true)))
  val toMergeBuffer       = new ToMergeBufferIO(isVStore) // req mergebuffer entry, inactive elem issue
  val out                 = Decoupled(new VLSBundle())// to split buffer
}

class VSplitBufferIO(isVStore: Boolean=false)(implicit p: Parameters) extends VLSUBundle{
  val redirect            = Flipped(ValidIO(new Redirect))
  val in                  = Flipped(Decoupled(new VLSBundle()))
  val out                 = Decoupled(new VecPipeBundle(isVStore))//to scala pipeline
  val vstd                = OptionWrapper(isVStore, ValidIO(new MemExuOutput(isVector = true)))
}

class VMergeBufferIO(isVStore : Boolean=false)(implicit p: Parameters) extends VLSUBundle{
  val redirect            = Flipped(ValidIO(new Redirect))
  val fromPipeline        = if(isVStore) Vec(StorePipelineWidth, Flipped(DecoupledIO(new VecPipelineFeedbackIO(isVStore)))) else Vec(LoadPipelineWidth, Flipped(DecoupledIO(new VecPipelineFeedbackIO(isVStore))))
  val fromSplit           = if(isVStore) Vec(VecStorePipelineWidth, new FromSplitIO) else Vec(VecLoadPipelineWidth, new FromSplitIO) // req mergebuffer entry, inactive elem issue
  val uopWriteback        = Vec(UopWritebackWidth, DecoupledIO(new MemExuOutput(isVector = true)))
  val toSplit             = if(isVStore) Vec(VecStorePipelineWidth, ValidIO(new FeedbackToSplitIO)) else Vec(VecLoadPipelineWidth, ValidIO(new FeedbackToSplitIO)) // for inorder inst
  val toLsq               = Vec(UopWritebackWidth, ValidIO(new FeedbackToLsqIO)) // for lsq deq
  val feedback            = Vec(UopWritebackWidth, ValidIO(new RSFeedback))//for rs replay
}