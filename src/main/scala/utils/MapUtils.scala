package utils

object MapUtils {
  def groupByValueUnique[K, V](map: Map[K, V]) : Map[V, Set[K]] = {
    map.toSeq
      .groupBy(_._2) // group by keys
      .map { case (key: V, seq: Seq[(K, V)]) => (key, seq.map(_._1).toSet) }
  }

  def groupByValue[K, V](map: Map[K, V]) : Map[V, Seq[K]] = {
    map.toSeq
      .groupBy(_._2) // group by keys
      .map { case (key: V, seq: Seq[(K, V)]) => (key, seq.map(_._1)) }
  }
}
