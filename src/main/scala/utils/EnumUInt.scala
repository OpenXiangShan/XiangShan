package utils

import chisel3._
import chisel3.util._

/**
 * Used to define enum values (e.g. finite state machine states)
 *
 * @example {{{
 *   object FsmState extends EnumUInt(3) {
 *     def Idle: UInt = 0.U(width.W)
 *     def Work: UInt = 1.U(width.W)
 *     def Done: UInt = 2.U(width.W)
 *   }
 *   FsmState.validate()
 * }}}
 *
 * validate() will check:
 * 1. all values are unique (unless allowDuplicate is true)
 *    @example {{{
 *      object FsmState extends EnumUInt(3) {
 *        def Idle: UInt = 0.U(width.W)
 *        def Work: UInt = 1.U(width.W)
 *        def Done: UInt = 1.U(width.W)
 *      }
 *      FsmState.validate() // will fail, as Idle/Work are duplicate
 *    }}}
 *    @example {{{
 *      object FsmState extends EnumUInt(3, allowDuplicate = true) {
 *        def Idle: UInt = 0.U(width.W)
 *        def Work: UInt = 1.U(width.W)
 *        def AliasOfWork: UInt = 1.U(width.W)
 *        def Done: UInt = 2.U(width.W)
 *      }
 *      FsmState.validate() // will pass, we allow duplicate values for alias
 *      // but please note that we must have n unique values (to satisfy rule 4)
 *    }}}
 * 2. all value's width is defined width
 *    @example {{{
 *      object FsmState extends EnumUInt(3) {
 *        def Idle: UInt = 0.U
 *        def Work: UInt = 1.U
 *        def Done: UInt = 2.U
 *      }
 *      FsmState.validate() // will fail, as Idle/Work are not defined with width, it's default to 1, but 2 expected
 *    }}}
 * 3. if useOneHot is true, all values are power of 2
 *    @example {{{
 *      object FsmState extends EnumUInt(4, useOneHot = true) {
 *        def Idle: UInt = 0.U(width.W)
 *        def Work: UInt = 1.U(width.W)
 *        def Done: UInt = 2.U(width.W)
 *        def Error: UInt = 3.U(width.W)
 *      }
 *      FsmState.validate() // will fail, as Error = 3.U is not a power of 2, Error = 4.U is expected
 *    }}}
 * 4. all possible values are defined
 *    @example {{{
 *      object FsmState extends EnumUInt(3) {
 *        def Idle: UInt = 0.U(width.W)
 *        def Work: UInt = 1.U(width.W)
 *      }
 *      FsmState.validate() // will fail, as only 2 values are defined, but 3 are expected
 *    }}}
 * NOTE: all methods used to define constants must be in CamelCase, and return UInt, and take no parameters
 */
abstract class EnumUInt(
    n:              Int,
    useOneHot:      Boolean = false,
    allowDuplicate: Boolean = false
) extends NamedUInt(if (useOneHot) n else log2Up(n)) {
  def validate(): Unit = {
    // we assume all "def Xxx: UInt = ..." methods are enum constants (CamelCase, returns UInt, no Parameters)
    val methods = this.getClass.getDeclaredMethods
      .filter { method =>
        method.getReturnType == classOf[UInt] &&
        method.getParameterTypes.isEmpty &&
        method.getName()(0).isUpper
      }

    var values = Seq[BigInt]()

    for (method <- methods) {
      val uint = method.invoke(this).asInstanceOf[UInt]
      // check1: all values must have a correct width
      assert(
        uint.getWidth == width,
        s"EnumUInt ${this.getClass.getName} has value with width(${uint.getWidth}), expected $width: ${method.getName}"
      )

      val value = uint.litValue
      val idx   = values.indexOf(value)
      if (!allowDuplicate) {
        // check2: all values must be unique
        assert(
          idx == -1,
          s"EnumUInt ${this.getClass.getName} has duplicate value($value): ${method.getName} and ${methods(idx).getName}"
        )
      }
      if (useOneHot) {
        // check3: one-hot values must be power of 2
        assert(
          isPow2(value),
          s"EnumUInt ${this.getClass.getName} has non one-hot value($value): ${method.getName}"
        )
      }

      values = values :+ value
    }

    // check4: if allowDuplicate is true, we can have more than n methods, but we must have n unique values
    // check4: otherwise, we must have n methods and n unique values
    assert(
      values.toSet.size == n,
      s"EnumUInt ${this.getClass.getName} has ${methods.length} methods, but expected $n"
    )

    println(
      s"EnumUInt ${this.getClass.getName} validated:" +
      s"${(methods.map(_.getName) zip values).sortBy{case (_, i) => i}.mkString("[", ",", "]")}"
    )
  }

  // call validate() in the constructor
  validate()
}
