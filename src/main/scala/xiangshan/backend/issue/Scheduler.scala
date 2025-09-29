package xiangshan.backend.issue

sealed trait SchedulerType

case class IntScheduler() extends SchedulerType
case class FpScheduler() extends SchedulerType
case class VecScheduler() extends SchedulerType
case class NoScheduler() extends SchedulerType
