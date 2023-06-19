package utils

object OptionWrapper {
  def apply[A](condition: Boolean, x: => A): Option[A] = condition match {
    case true => Some(x)
    case false => None
  }

}
