// Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
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

import chisel3._

object PartialConnect {
  object Behavior extends Enumeration {

    /** do nothing (leave unconnected) */
    val Ignore: Value = Value("Ignore")

    /** act like ignore, but print a warning on compilation */
    val WarnAndIgnore: Value = Value("WarnAndIgnore")

    /** throw exception */
    val Error: Value = Value("Error")

    /** do sink := DontCare with onTypeMismatch / onWidthMismatch / onExtraFields, do nothing with onUnusedFields */
    val DontCare: Value = Value("DontCare")

    /** act like dontCare, but print a warning on compilation */
    val WarnAndDontCare: Value = Value("WarnAndDontCare")

    /** do sink := DontCare with onTypeMismatch / onWidthMismatch, do nothing with onUnusedFields / onExtraFields */
    val Force: Value = Value("Force")

    /** act like force, but print a warning on compilation */
    val WarnAndForce: Value = Value("WarnAndForce")
  }

  private def except(
      msg:       String,
      behavior:  Behavior.Value,
      sinkOpt:   Option[Data] = None,
      sourceOpt: Option[Data] = None
  ): Unit =
    behavior match {
      case Behavior.Ignore =>
      case Behavior.WarnAndIgnore =>
        println(s"Warning: $msg, leave unconnected")
      case Behavior.Error =>
        throw new Exception(msg)
      case Behavior.DontCare =>
        sinkOpt.foreach(_ := DontCare)
      case Behavior.WarnAndDontCare =>
        println(s"Warning: $msg, connect to DontCare")
        sinkOpt.foreach(_ := DontCare)
      case Behavior.Force =>
        sinkOpt.foreach(sink => sourceOpt.foreach(sink := _.asTypeOf(sink)))
      case Behavior.WarnAndForce =>
        println(s"Warning: $msg, forcing connect")
        sinkOpt.foreach(sink => sourceOpt.foreach(sink := _.asTypeOf(sink)))
    }

  /** connect source to sink (like sink := source), but ignoring extra sinkFields
   * @param sink sink bundle to connect
   * @param source source bundle to connect
   * @param onTypeMismatch behavior on sinkField and sourceField have different types, error by default
   * @param onWidthMismatch behavior on sinkField and sourceField have different width, warn and do connect by default
   * @param onUnusedFields behavior on source have extra fields, leave unused by default
   * @param onExtraFields behavior on sink have extra fields, leave unconnected by default
   * @param exclude explicitly exclude some fields from connection, e.g. `exclude = Seq("field1", "field2")`
   * @example {{{
   * val a = new Bundle {
   *   val uint_value   = UInt(16.W)
   *   val inner_bundle = new InnerBundle()
   *   val debug_info   = new DebugInfo()
   * }
   * val b = new Bundle {
   *   val uint_value   = UInt(8.W)
   *   val inner_bundle = new InnerBundle()
   *   val extra_info   = new ExtraInfo()
   * }
   *
   * // cannot connect them using :=
   * a := b
   *
   * PartialConnect(a, b)
   * // equivalent to:
   * a.uint_value   := b.uint_value
   * a.inner_bundle := b.inner_bundle
   *
   * PartialConnect(a, b, onExtraFields = PartialConnect.Behavior.DontCare)
   * // equivalent to:
   * a.uint_value   := b.uint_value
   * a.inner_bundle := b.inner_bundle
   * a.debug_info   := DontCare
   *
   * PartialConnect(a, b, onWidthMismatch = PartialConnect.Behavior.Error)
   * // will fail because a.uint_value has different width with b.uint_value
   *
   * PartialConnect(a, b, onUnusedFields = PartialConnect.Behavior.Error)
   * // will fail because b.extra_info does not exist in a
   *
   * val c = new Bundle {
   *   val uint_value   = UInt(16.W)
   *   val inner_bundle = new AnotherInnerBundle()
   * }
   * PartialConnect(a, c)
   * // will fail by default because c.inner_bundle is a AnotherInnerBundle, not InnerBundle
   *
   * // can be explicitly ignored by:
   * PartialConnect(a, c, onTypeMismatch = PartialConnect.Behavior.Ignore)
   * // equivalent to:
   * a.uint_value := c.uint_value
   *
   * // or even force to connect (dangerous!) by:
   * PartialConnect(a, c, onTypeMismatch = PartialConnect.Behavior.Force)
   * // equivalent to:
   * a.uint_value := c.uint_value
   * a.inner_bundle := c.inner_bundle.asTypeOf(a.inner_bundle)
   * }}}
   */
  def apply(
      sink:            Bundle,
      source:          Bundle,
      onTypeMismatch:  Behavior.Value = Behavior.Error,
      onWidthMismatch: Behavior.Value = Behavior.WarnAndForce,
      onUnusedFields:  Behavior.Value = Behavior.Ignore,
      onExtraFields:   Behavior.Value = Behavior.Ignore,
      exclude:         Seq[String] = Seq.empty
  ): Unit = {
    sink.elements.filterNot { case (name, _) =>
      exclude.contains(name)
    }.foreach { case (name, sinkField) =>
      source.elements.get(name) match {
        case Some(sourceField) =>
          if (sinkField.getClass == sourceField.getClass) {
            if (sinkField.getWidth == sourceField.getWidth) {
              sinkField := sourceField
            } else {
              except(
                s"Width mismatch for field '$name': sink is ${sinkField.getWidth}, source is ${sourceField.getWidth}",
                onWidthMismatch,
                Option(sinkField),
                Option(sourceField)
              )
            }
          } else {
            except(
              s"Type mismatch for field '$name': sink is ${sinkField.getClass}, source is ${sourceField.getClass}",
              onTypeMismatch,
              Option(sinkField),
              Option(sourceField)
            )
          }
        case None =>
          except(
            s"Extra field in sink: $name",
            onExtraFields,
            Option(sinkField)
          )
      }
    }

    val unusedFields = source.elements.keySet.diff(sink.elements.keySet)
    if (unusedFields.nonEmpty) {
      except(
        s"Unused fields in source: ${unusedFields.mkString(", ")}",
        onUnusedFields
      )
    }
  }
}
