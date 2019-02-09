package top

import org.joda.time.{DateTime, DateTimeZone}

object UpTime {
  private def getTime = DateTime.now(DateTimeZone.UTC).getMillis().toInt
  private val bootTime = getTime
  def apply() = getTime - bootTime
}
