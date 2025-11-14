package xiangshan.frontend.ftq

import chisel3._
import org.chipsalliance.cde.config.Parameters
import utility.CircularQueuePtr
import xiangshan.XSCoreParamsKey

class CommitQueuePtr(entries: Int) extends CircularQueuePtr[CommitQueuePtr](entries) {
  def this()(implicit p: Parameters) = this(p(XSCoreParamsKey).frontendParameters.ftqParameters.CommitQueueSize)
}

object CommitQueuePtr {
  def apply(f: Bool, v: UInt)(implicit p: Parameters): CommitQueuePtr = {
    val ptr = Wire(new CommitQueuePtr())
    ptr.flag  := f
    ptr.value := v
    ptr
  }
}
