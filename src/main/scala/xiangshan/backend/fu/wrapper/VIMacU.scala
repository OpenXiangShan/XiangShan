// FIXME: VIMacU removed — needs VecFixLatFunc/VecFuConfig migration
// Original file backed up to .copilot/tasks/rebase-fix/backup/VIMacU.scala.bak
package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import xiangshan.backend.fu.FuConfig

class VIMacU(cfg: FuConfig)(implicit p: Parameters) extends chisel3.RawModule {
  // Stub: VIMacU requires VecFixLatFunc/VecFuConfig infrastructure not yet ported
  val io = IO(new Bundle {})
}
