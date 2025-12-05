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
 * }}}
 *
 * Validate() will check:
 *
 * 1. all values are unique (unless allowDuplicate is true)
 *    @example {{{
 *      object FsmState extends EnumUInt(3) {
 *        def Idle: UInt = 0.U(width.W)
 *        def Work: UInt = 1.U(width.W)
 *        def Done: UInt = 1.U(width.W) // will fail, as Work/Done are duplicate
 *      }
 *    }}}
 *    @example {{{
 *      object FsmState extends EnumUInt(3, allowDuplicate = true) {
 *        def Idle: UInt = 0.U(width.W)
 *        def Work: UInt = 1.U(width.W)
 *        def AliasOfWork: UInt = 1.U(width.W) // will pass, we allow duplicate values for alias
 *        def Done: UInt = 2.U(width.W)
 *      } // but please note that we must have n=3 unique values (to satisfy rule 4)
 *    }}}
 * 2. all value's width is defined width
 *    @example {{{
 *      object FsmState extends EnumUInt(3) {
 *        def Idle: UInt = 0.U // will fail, as Idle/Work are not defined with width, it's default to 1, but 2 expected
 *        def Work: UInt = 1.U
 *        def Done: UInt = 2.U
 *      }
 *    }}}
 * 3. if useOneHot is true, all values are power of 2, and not 0
 *    @example {{{
 *      object FsmState extends EnumUInt(4, useOneHot = true, /* allowZeroForOneHot = false */) {
 *        // def None: UInt = 0.U(width.W) // will fail, unless allowZeroForOneHot is true
 *        def Idle: UInt = 1.U(width.W)
 *        def Work: UInt = 2.U(width.W)
 *        def Done: UInt = 4.U(width.W)
 *        def Error: UInt = 5.U(width.W) // will fail, as Error = 5.U is not a power of 2, Error = 8.U is expected
 *      }
 *    }}}
 * 4. all possible values are defined
 *    @example {{{
 *      object FsmState extends EnumUInt(3) {
 *        def Idle: UInt = 0.U(width.W)
 *        def Work: UInt = 1.U(width.W)
 *        // will fail, as only 2 values are defined, but 3 are expected
 *      }
 *    }}}
 *
 * NOTE: all methods used to define constants must be in UpperCamelCase, and return UInt, and take no parameters
 *
 * You can use fixedWidth or define reserved values to define the width of the enum
 * @example {{{
 *   // we have only defined 2 values, but we want width=2, so we use fixedWidth=2
 *   object FsmState extends EnumUInt(2, fixedWidth = Some(2)) {
 *     def Idle: UInt = 0.U(width.W)
 *     def Work: UInt = 1.U(width.W)
 *   }
 *   // or, we can define all 4 values, with 2 of them reserved
 *   import annotation.unused
 *   object FsmState extends EnumUInt(4) {
 *     def Idle: UInt = 0.U(width.W)
 *     def Work: UInt = 1.U(width.W)
 *     @unused
 *     def Rsvd2: UInt = 2.U(width.W) // reserved value
 *     @unused
 *     def Rsvd3: UInt = 3.U(width.W) // reserved value
 *   }
 * }}}
 */
abstract class EnumUInt(
    n:                  Int,
    useOneHot:          Boolean = false,
    allowZeroForOneHot: Boolean = false, // if true, we can use 0.U as one-hot value
    allowDuplicate:     Boolean = false,
    fixedWidth:         Option[Int] = None
) extends NamedUInt(
      if (fixedWidth.isDefined) {
        require(
          fixedWidth.get >= log2Up(n),
          s"EnumUInt fixedWidth=${fixedWidth.get} must be greater or equal than log2Up($n)=${log2Up(n)}."
        )
        fixedWidth.get
      } else if (useOneHot) {
        n - (if (allowZeroForOneHot) 1 else 0) // if 0.U is a legal state, only n-1 bits are needed for n states
      } else {
        log2Up(n)
      }
    ) {

  private var names     = Seq[String]()
  private var values    = Seq[UInt]()
  private var litValues = Seq[BigInt]()

  private def getValuesString: String =
    (this.names zip this.litValues).sortBy { case (_, i) => i }.mkString("[", ", ", "]")

  private def validate(): Unit = {
    val methodsAll = this.getClass.getDeclaredMethods
      .filter { method =>
        method.getReturnType == classOf[UInt] && // return UInt
        method.getParameterTypes.isEmpty         // no parameters
      }
    val methods = methodsAll.filter(_.getName()(0).isUpper) // UpperCamelCase
    this.names = methods.map(_.getName).toSeq
    this.values = methods.map(_.invoke(this).asInstanceOf[UInt]).toSeq
    this.litValues = this.values.map(_.litValue)

    (this.names zip this.values zip this.litValues).zipWithIndex.foreach { case (((name, value), litValue), i) =>
      val dupIdx = this.litValues.slice(0, i).indexOf(litValue)
      assert( // check1: all values must have a correct width
        value.getWidth == this.width,
        s"EnumUInt ${this.getClass.getName} has value with width(${value.getWidth}), expected $width: ${name}."
      )
      assert( // check2: all values must be unique, unless allowDuplicate is true
        allowDuplicate || dupIdx == -1,
        s"EnumUInt ${this.getClass.getName} has duplicate value(${litValue}): ${name} and ${this.names(dupIdx)}."
      )
      assert( // check3.1: one-hot values must be power of 2
        !useOneHot || isPow2(litValue),
        s"EnumUInt ${this.getClass.getName} using one-hot has non one-hot value(${litValue}): ${name}."
      )
      assert( // check3.2: one-hot values must not be 0
        !useOneHot || allowZeroForOneHot || litValue != 0,
        s"EnumUInt ${this.getClass.getName} using one-hot has zero value(0): ${name}. " +
          s"Please consider using Vec[Bool] if you want to use 0 as 'None' and other values are one-hot. " +
          s"Or, set allowZeroForOneHot to true if you intentionally want to use 0 as one-hot value for EnumUInt."
      )
    }

    // check4: if allowDuplicate is true, we can have more than n methods, but we must have n unique values
    // check4: otherwise, we must have n methods and n unique values
    val uniqueLitValues = this.litValues.toSet.size
    if (uniqueLitValues != this.n) {
      // warn if there are methods seems to be constants but not UpperCamelCase before throw AssertionError
      val methodsNotUpperCamelCase = methodsAll.diff(methods)
      methodsNotUpperCamelCase.foreach { method =>
        println(
          s"[Warn]: EnumUInt ${this.getClass.getName} seems has constants definition '${method.getName}' " +
            s"that is not UpperCamelCase, Do you mean '${method.getName.capitalize}'?"
        )
      }
      assert(
        cond = false,
        s"EnumUInt ${this.getClass.getName} has ${uniqueLitValues} unique values, but expected ${this.n}." +
          (if (methodsNotUpperCamelCase.nonEmpty) " Maybe you miss UpperCamelCase? check warnings above." else "")
      )
    }

    // pass!
    println(s"EnumUInt ${this.getClass.getName} validated: ${this.getValuesString}")
  }

  /** Check or generate assertion for unsafe inputs
   * @example {{{
   *   class ExceptionType extends Bundle {
   *     val value: UInt = ExceptionType() // this is inherit from NamedUInt.apply(), i.e. UInt(width.W)
   *
   *     def hasException: Bool = value =/= ExceptionType.None
   *   }
   *
   *   object ExceptionType extends EnumUInt(4) {
   *     def None: UInt = 0.U(width.W)
   *     def Pf:   UInt = 1.U(width.W)
   *     def Gpf:  UInt = 2.U(width.W)
   *     def Af:   UInt = 3.U(width.W)
   *
   *     def apply(that: UInt): ExceptionType = {
   *       assertLegal(that) // do check or generate assertion before do wiring
   *       val e = Wire(new ExceptionType)
   *       e.value := that
   *       e
   *     }
   *   }
   *
   *   val e1 = ExceptionType(4.U) // will be checked at compile time, and fails as 4.U is not a legal value
   *
   *   val unsafeWire = Wire(UInt(2.W))
   *   val e2 = ExceptionType(unsafeWire) // will be checked at runtime
   *
   *   val e3 = 0.U.asTypeOf(new ExceptionType) // this cannot be checked, we should avoid at best effort
   *   // -> val e3 = ExceptionType(0.U) // this is better
   * }}}
   */
  def assertLegal(that: UInt): Unit =
    if (that.isLit) { // if is literal, check value at compile time (if legal, we don't need to care about width)
      assert(
        this.litValues.contains(that.litValue),
        s"EnumUInt ${this.getClass.getName} gets illegal input with value(${that.litValue}), " +
          s"expected one of ${this.getValuesString}."
      )
    } else { // otherwise, check width at compile time, and value at runtime
      assert(
        this.width == that.getWidth,
        s"EnumUInt ${this.getClass.getName} gets illegal input with width(${that.getWidth}), expected $width."
      )
      assert(
        VecInit(this.values.map(_ === that)).asUInt.orR,
        s"EnumUInt ${this.getClass.getName} gets illegal input with value(%d), " +
          s"expected one of ${this.getValuesString}.",
        that // that is a hardware Wire/Reg, we cannot use litValue
      )
    }

  /**
   * Get valid vector (or cartesian product with thatVec) for the enum value
   * @example {{{
   *   object ThisEnum extends EnumUInt(3) {
   *     def X: UInt = 0.U(width.W)
   *     def Y: UInt = 1.U(width.W)
   *     def Z: UInt = 2.U(width.W)
   *   }
   *   val thisValue = WireDefault(ThisEnum.X)
   *
   *   object ThatEnum extends EnumUInt(2) {
   *     def A: UInt = 0.U(width.W)
   *     def B: UInt = 1.U(width.W)
   *   }
   *   val thatValue = WireDefault(ThatEnum.B)
   *   val thatSeq = ThatEnum.getValidSeq(thatValue, thisPrefix = "thatIs")
   *   // thatSeq = Seq(
   *   //   "thatIsA" -> false.B,
   *   //   "thatIsB" -> true.B // thatValue === ThatEnum.B
   *   // )
   *
   *   val crossSeq = ThisEnum.getValidSeq(thisValue, thatSeq = thatSeq, exclude = "Z", thisPrefix = "thisIs")
   *   // crossSeq = Seq(
   *   //   "thatIsA_thisIsX" -> false.B,
   *   //   "thatIsA_thisIsY" -> false.B,
   *   //   "thatIsB_thisIsX" -> true.B, // thatValue === ThatEnum.B && thisValue === ThisEnum.X
   *   //   "thatIsB_thisIsY" -> false.B
   *   // )
   * }}}
   */
  def getValidSeq(
      value:      UInt,
      thatSeq:    Seq[(String, Bool)] = Seq.empty,
      exclude:    Set[String] = Set.empty,
      thisPrefix: String = ""
  ): Seq[(String, Bool)] = {
    exclude.filter(name => !this.names.contains(name)).foreach { name =>
      println(s"[Warn]: EnumUInt ${this.getClass.getName} getValidVec exclude set got illegal name '${name}'.")
    }
    val thisVec = (this.names zip this.values).filter { case (name, _) =>
      !exclude.contains(name)
    }.map { case (name, v) =>
      s"${thisPrefix}${name}" -> (v === value)
    }
    if (thatSeq.isEmpty)
      thisVec
    else
      thatSeq.flatMap { case (thatName, thatValid) =>
        thisVec.map { case (thisName, thisValid) =>
          s"${thatName}_${thisName}" -> (thatValid && thisValid)
        }
      }
  }

  // call validate() in the constructor
  validate()
}
