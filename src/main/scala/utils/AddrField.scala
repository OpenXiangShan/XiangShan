// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package utils

/** Utility to print fields(i.e. tag, setIdx, offset) extracted from an address
 *
 * @example {{{
 *   val test = AddrField(
 *     Seq(
 *       ("offset", 12), // we use offset starts from bit 0, width 12
 *       ("setIdx", 4),  // and setIdx starts from bit 12, width 4
 *       ("tag"   , 16), // and tag starts from bit 16, width 16
 *     ),
 *     maxWidth = Option(64), // we have 64 bits in total
 *     extraFields = Seq(
 *       ("anotherSetIdx", 6, 20), // we have another index starts from bit 6, width 20
 *     )
 *   )
 *
 *   test.show()
 *   // 64| 63..32 | 31..16 | 15..12 | 11...0 |
 *   //   | unused |    tag | setIdx | offset |
 *   //                | 25..............6 |
 *   //                |     anotherSetIdx |
 *
 *   test.showList()
 *   // offset: [11:0]
 *   // setIdx: [15:12]
 *   //    tag: [31:16]
 *   // anotherSetIdx: [25:6]
 * }}}
 */
class AddrField(
    fields:      Seq[(String, Int)],           // name, width
    maxWidth:    Option[Int] = None,
    extraFields: Seq[(String, Int, Int)] = Nil // name, start, width
) {
  private class Field(
      val name:  String,
      val start: Int,
      val width: Int
  ) {
    val end: Int = start + width - 1

    private val startString = start.toString
    private val endString   = end.toString

    def fieldLength:   Int = name.length max (startString.length + endString.length + 2)
    def maxNameLength: Int = fields.map(_._1.length).max

    def formatName(fieldLength: Int = fieldLength): String =
      String.format(s" %${fieldLength max 1}s ", name)

    def formatStart(fieldLength: Int = fieldLength): String =
      if (end == start)
        f" ${"." * ((fieldLength - startString.length) max 1)}$startString "
      else
        f" $endString${"." * ((fieldLength - startString.length - endString.length) max 1)}$startString "

    override def toString: String =
      String.format(s"%${maxNameLength}s: [%d:%d]", name, end, start)
  }

  private var currentStart = 0
  private val fieldInstances = fields.map { case (name, width) =>
    val field = new Field(name, currentStart, width)
    currentStart += width
    field
  }

  private val end = maxWidth.getOrElse(currentStart)

  private val unusedFieldInstance =
    Option.when(end != currentStart)(new Field("unused", currentStart, end - currentStart))

  private val extraFieldInstances = extraFields.map { case (name, start, width) =>
    new Field(name, start, width)
  }

  def format(indent: Int = 0): Seq[String] = {
    val allInstances = (unusedFieldInstance ++ fieldInstances.reverse).toSeq
    Seq(
      "%s%d|%s|".format(
        " " * indent,
        end,
        allInstances.map(_.formatStart()).mkString("|")
      ),
      "%s|%s|".format(
        " " * (indent + end.toString.length),
        allInstances.map(_.formatName()).mkString("|")
      )
    ) ++ extraFieldInstances.map { field =>
      def getOffset(p: Int, f: Field): Int = ((p - f.start).toFloat / (f.width - 1) * f.fieldLength).toInt

      val startIdx = allInstances.indexWhere(f => f.end >= field.start && f.start <= field.start)
      val startOffset = getOffset(field.start, allInstances(startIdx))

      val endIdx = allInstances.indexWhere(f => f.end >= field.end && f.start <= field.end)
      val endOffset = getOffset(field.end, allInstances(endIdx))

      val leftPosition = allInstances.take(endIdx + 1).foldLeft(indent)(_ + _.fieldLength + 3) - endOffset - 1
      val rightPosition = allInstances.take(startIdx + 1).foldLeft(indent)(_ + _.fieldLength + 3) - startOffset - 1

      val leftPadding = leftPosition
      val innerLength = rightPosition - leftPosition

      f"${" " * leftPadding}|${field.formatStart(innerLength)}|\n${" " * leftPadding}|${field.formatName(innerLength)}|"
    }
  }

  def show(indent: Int = 0): Unit =
    println(format(indent).mkString("\n"))

  def showList(): Unit =
    (fieldInstances ++ extraFieldInstances).foreach(println)
}

object AddrField {
  def apply(
      fields:      Seq[(String, Int)],
      maxWidth:    Option[Int] = None,
      extraFields: Seq[(String, Int, Int)] = Nil
  ): AddrField =
    new AddrField(fields, maxWidth, extraFields)
}
