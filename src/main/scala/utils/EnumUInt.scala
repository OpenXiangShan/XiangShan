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
 * NOTE: all methods used to define constants must be in UpperCamelCase, and return UInt, and take no parameters
 */
abstract class EnumUInt(
    n:              Int,
    useOneHot:      Boolean = false,
    allowDuplicate: Boolean = false
) extends NamedUInt(if (useOneHot) n else log2Up(n)) {
  def validate(): Unit = {
    val methodsAll = this.getClass.getDeclaredMethods
      .filter { method =>
        method.getReturnType == classOf[UInt] && // return UInt
        method.getParameterTypes.isEmpty         // no parameters
      }
    val methods = methodsAll.filter(_.getName()(0).isUpper) // UpperCamelCase
    var values  = Seq[BigInt]()
    methods.foreach { method =>
      val uint   = method.invoke(this).asInstanceOf[UInt]
      val value  = uint.litValue
      val dupIdx = values.indexOf(value)
      assert( // check1: all values must have a correct width
        uint.getWidth == width,
        s"EnumUInt ${this.getClass.getName} has value with width(${uint.getWidth}), expected $width: ${method.getName}."
      )
      assert( // check2: all values must be unique, unless allowDuplicate is true
        allowDuplicate || dupIdx == -1,
        s"EnumUInt ${this.getClass.getName} has duplicate value($value): ${method.getName} and ${methods(dupIdx).getName}."
      )
      assert( // check3: one-hot values must be power of 2
        !useOneHot || isPow2(value),
        s"EnumUInt ${this.getClass.getName} has non one-hot value($value): ${method.getName}."
      )
      values = values :+ value
    }

    // check4: if allowDuplicate is true, we can have more than n methods, but we must have n unique values
    // check4: otherwise, we must have n methods and n unique values
    if (values.toSet.size != n) {
      // warn if there are methods seems to be constants but not UpperCamelCase before throw AssertionError
      val methodsNotUpperCamelCase = methodsAll.diff(methods)
      methodsNotUpperCamelCase.foreach { method =>
        println(
          s"[Warn]: EnumUInt ${this.getClass.getName} seems has constants defination '${method.getName}' " +
            s"that is not UpperCamelCase, Do you mean '${method.getName.capitalize}'?"
        )
      }
      assert(
        false,
        s"EnumUInt ${this.getClass.getName} has ${values.toSet.size} unique values, but expected $n." +
          (if (methodsNotUpperCamelCase.nonEmpty) " Maybe you miss UpperCamelCase? check warnings above." else "")
      )
    }

    // pass!
    println(
      s"EnumUInt ${this.getClass.getName} validated:" +
        s"${(methods.map(_.getName) zip values).sortBy { case (_, i) => i }.mkString("[", ",", "]")}"
    )
  }

  // call validate() in the constructor
  validate()
}
