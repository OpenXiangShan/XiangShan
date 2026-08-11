package xiangshan.mem.vector

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.ExceptionNO.{breakPoint, hardwareError, loadAccessFault, loadAddrMisaligned, loadGuestPageFault, loadPageFault}
import xiangshan.backend.Bundles.UopIdx
import xiangshan.backend.fu.FuConfig.LduCfg
import xiangshan.backend.fu.vector.Bundles.{VEew, Vl}

class VldExcpGen(implicit p: Parameters) extends XSModule {

  val in = IO(Input(new VldExcpGen.In))
  val out = IO(Output(new VldExcpGen.Out))

  private val isVleff = LSUOpType.isFof(in.fuOpType)
  private val isVle = LSUOpType.isUStride(in.fuOpType) && !isVleff

  private val byteOffsetInBeat = in.vaddr(VldExcpGen.byteIdxWidth - 1, 0)

  private val triggerActiveMask = VldExcpGen.getActiveMask(
    in.s3TriggerMask,
    in.s4TriggerMask,
    byteOffsetInBeat
  )

  private val loadAddrMisalignedActiveMask = getExceptionActiveMask(loadAddrMisaligned)
  private val loadAccessFaultActiveMask = getExceptionActiveMask(loadAccessFault)
  private val loadPageFaultActiveMask = getExceptionActiveMask(loadPageFault)
  private val loadGuestPageFaultActiveMask = getExceptionActiveMask(loadGuestPageFault)
  private val hardwareErrorActiveMask = getExceptionActiveMask(hardwareError)

  private val uopBaseByte = in.uopIdx << VldExcpGen.byteIdxWidth

  private val rawBreakPoint = in.exceptionVec(breakPoint) && triggerActiveMask.orR
  private val rawLoadAddrMisaligned = in.exceptionVec(loadAddrMisaligned) && loadAddrMisalignedActiveMask.orR
  private val rawLoadAccessFault = in.exceptionVec(loadAccessFault) && loadAccessFaultActiveMask.orR
  private val rawLoadPageFault = in.exceptionVec(loadPageFault) && loadPageFaultActiveMask.orR
  private val rawLoadGuestPageFault = in.exceptionVec(loadGuestPageFault) && loadGuestPageFaultActiveMask.orR
  private val rawHardwareError = in.exceptionVec(hardwareError) && hardwareErrorActiveMask.orR

  private val hasBreakPoint = rawBreakPoint && canTakeVleffException(triggerActiveMask)
  private val hasLoadAddrMisaligned = rawLoadAddrMisaligned && canTakeVleffException(loadAddrMisalignedActiveMask)
  private val hasLoadAccessFault = rawLoadAccessFault && canTakeVleffException(loadAccessFaultActiveMask)
  private val hasLoadPageFault = rawLoadPageFault && canTakeVleffException(loadPageFaultActiveMask)
  private val hasLoadGuestPageFault = rawLoadGuestPageFault && canTakeVleffException(loadGuestPageFaultActiveMask)
  private val hasHardwareError = rawHardwareError && canTakeVleffException(hardwareErrorActiveMask)

  private val activeMask = MuxCase(0.U(VldExcpGen.vlenb.W), Seq(
    rawBreakPoint -> triggerActiveMask,
    rawLoadAddrMisaligned -> loadAddrMisalignedActiveMask,
    rawLoadPageFault -> loadPageFaultActiveMask,
    rawLoadGuestPageFault -> loadGuestPageFaultActiveMask,
    rawLoadAccessFault -> loadAccessFaultActiveMask,
    rawHardwareError -> hardwareErrorActiveMask
  ))
  private val hasActiveException = activeMask.orR

  private val byteOffset = PriorityEncoder(activeMask)
  private val vstart = (uopBaseByte + byteOffset) >> in.eew
  private val vl = (uopBaseByte + byteOffset) >> in.eew
  private val hasVleffNonFirstException = isVleff && hasActiveException && vl =/= 0.U
  private val exceptionVaddr = in.vaddr +& byteOffset.pad(XLEN)
  private val exceptionGpaddr = in.gpaddr +& byteOffset.pad(GPAddrBits)

  out.vstart := Mux(hasActiveException, vstart, 0.U).asTypeOf(out.vstart)
  out.vl.valid := hasVleffNonFirstException
  out.vl.bits := Mux(hasVleffNonFirstException, vl, 0.U).asTypeOf(out.vl.bits)

  out.vaddr := Mux(hasActiveException, exceptionVaddr(XLEN - 1, 0), in.vaddr)
  out.gpaddr := Mux(hasActiveException, exceptionGpaddr(GPAddrBits - 1, 0), in.gpaddr)
  out.exceptionVec := in.exceptionVec
  out.exceptionVec(breakPoint) := hasBreakPoint
  out.exceptionVec(loadAddrMisaligned) := hasLoadAddrMisaligned
  out.exceptionVec(loadAccessFault) := hasLoadAccessFault
  out.exceptionVec(loadPageFault) := hasLoadPageFault
  out.exceptionVec(loadGuestPageFault) := hasLoadGuestPageFault
  out.exceptionVec(hardwareError) := hasHardwareError

  private def canTakeVleffException(mask: UInt): Bool = !isVleff || ((uopBaseByte + PriorityEncoder(mask)) >> in.eew === 0.U)

  private def getExceptionActiveMask(exception: Int): UInt = VldExcpGen.getActiveMask(
    Mux(in.s3ExceptionVec(exception), in.s3Mask, 0.U),
    Mux(in.s4ExceptionVec(exception), in.s4Mask, 0.U),
    byteOffsetInBeat
  )

  dontTouch(activeMask)
  dontTouch(triggerActiveMask)
  dontTouch(isVleff)
  dontTouch(isVle)

}

object VldExcpGen {
  def vlenb(implicit p: Parameters): Int = p(XSCoreParamsKey).VLEN / 8
  def byteIdxWidth(implicit p: Parameters): Int = log2Ceil(vlenb)

  class In(implicit p: Parameters) extends XSBundle {
    val uopIdx = UopIdx()
    val fuOpType = FuOpType()
    val exceptionVec = ExceptSparseVec(LduCfg.exceptionOut)
    val eew = VEew()
    val s3TriggerMask = UInt(VldExcpGen.vlenb.W)
    val s4TriggerMask = UInt(VldExcpGen.vlenb.W)
    val s3Mask = UInt(VldExcpGen.vlenb.W)
    val s4Mask = UInt(VldExcpGen.vlenb.W)
    val s3ExceptionVec = ExceptSparseVec(LduCfg.exceptionOut)
    val s4ExceptionVec = ExceptSparseVec(LduCfg.exceptionOut)
    val vaddr             = UInt(XLEN.W)
    val gpaddr            = UInt(GPAddrBits.W)
  }

  class Out(implicit p: Parameters) extends XSBundle {
    val vstart = Vl()
    val vl = Valid(Vl())
    val vaddr = UInt(XLEN.W)
    val gpaddr = UInt(GPAddrBits.W)
    val exceptionVec = ExceptSparseVec(LduCfg.exceptionOut)
  }

  def getActiveMask(s3Mask: UInt, s4Mask: UInt, byteOffsetInBeat: UInt)(implicit p: Parameters): UInt = {
    val mergeMask = (Cat(s3Mask, s4Mask) >> byteOffsetInBeat)(vlenb - 1, 0)
    Mux(byteOffsetInBeat === 0.U, s3Mask, mergeMask)
  }
}
