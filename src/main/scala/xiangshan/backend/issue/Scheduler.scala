package xiangshan.backend.issue

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import utility.HasPerfEvents
import utils.OptionWrapper
import xiangshan._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath.DataConfig._
import xiangshan.backend.datapath.WbConfig._
import xiangshan.backend.fu.FuType
import xiangshan.backend.regfile.RfWritePortWithConfig
import xiangshan.backend.datapath.WbConfig.V0WB
import xiangshan.backend.regfile.VlPregParams
import xiangshan.backend.regcache.RegCacheTagTable
import xiangshan.mem.{LsqEnqCtrl, LsqEnqIO, SqPtr, LqPtr}
import xiangshan.mem.Bundles.MemWaitUpdateReqBundle

sealed trait SchedulerType

case class IntScheduler() extends SchedulerType
case class FpScheduler() extends SchedulerType
case class VfScheduler() extends SchedulerType
case class NoScheduler() extends SchedulerType
