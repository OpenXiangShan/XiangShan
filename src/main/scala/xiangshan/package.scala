/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.BackendParams
import freechips.rocketchip.tile.XLen
import xiangshan.ExceptionNO._
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.fu._
import xiangshan.backend.fu.fpu._
import xiangshan.backend.fu.vector._
import xiangshan.backend.issue._
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.decode.{Imm, ImmUnion}
import xiangshan.backend.vector.util.BString.BinaryStringHelper

package object xiangshan {
  object SrcType {
    def imm = "b0000".U(4.W)
    def pc  = "b0000".U(4.W)
    def xp  = "b0001".U(4.W)
    def fp  = "b0010".U(4.W)
    def vp  = "b0100".U(4.W)
    def v0  = "b1000".U(4.W)
    def no  = "b0000".U(4.W) // this source read no reg but cannot be Any value

    // alias
    def reg = this.xp
    def DC  = imm // Don't Care
    def X   = BitPat("b0000")

    def isPc(srcType: UInt) = srcType===pc
    def isImm(srcType: UInt) = srcType===imm
    def isXp(srcType: UInt) = srcType(0)
    def isFp(srcType: UInt) = srcType(1)
    def isVp(srcType: UInt) = srcType(2)
    def isV0(srcType: UInt) = srcType(3)
    def isPcOrImm(srcType: UInt) = isPc(srcType) || isImm(srcType)
    def isNotReg(srcType: UInt): Bool = !srcType.orR
    def isVfp(srcType: UInt) = isVp(srcType) || isFp(srcType)
    def apply() = UInt(4.W)
  }

  object SrcState {
    def busy    = "b0".U
    def rdy     = "b1".U
    // def specRdy = "b10".U // speculative ready, for future use
    def apply() = UInt(1.W)

    def isReady(state: UInt): Bool = state === this.rdy
    def isBusy(state: UInt): Bool = state === this.busy
  }

  val FuOpType: Opcode.type = Opcode

  object VlduType {
    // bit encoding: | vector or scala (2bit) || mop (2bit) | lumop(5bit) |
    // only unit-stride use lumop
    // mop [1:0]
    // 0 0 : unit-stride
    // 0 1 : indexed-unordered
    // 1 0 : strided
    // 1 1 : indexed-ordered
    // lumop[4:0]
    // 0 0 0 0 0 : unit-stride load
    // 0 1 0 0 0 : unit-stride, whole register load
    // 0 1 0 1 1 : unit-stride, mask load, EEW=8
    // 1 0 0 0 0 : unit-stride fault-only-first
    def vle       = "b01_00_00000".U
    def vlr       = "b01_00_01000".U // whole
    def vlm       = "b01_00_01011".U // mask
    def vleff     = "b01_00_10000".U
    def vluxe     = "b01_01_00000".U // index
    def vlse      = "b01_10_00000".U // strided
    def vloxe     = "b01_11_00000".U // index

    def isUnitStride(fuOpType: UInt): Bool = fuOpType(6, 5) === "b00".U
    def isWhole  (fuOpType: UInt): Bool = isUnitStride(fuOpType) && fuOpType(4, 0) === "b01000".U && (fuOpType(8) ^ fuOpType(7))
    def isMasked (fuOpType: UInt): Bool = isUnitStride(fuOpType) && fuOpType(4, 0) === "b01011".U && (fuOpType(8) ^ fuOpType(7))
    def isStrided(fuOpType: UInt): Bool = fuOpType(6, 5) === "b10".U && (fuOpType(8) ^ fuOpType(7))
    def isIndexed(fuOpType: UInt): Bool = fuOpType(5) && (fuOpType(8) ^ fuOpType(7))
    def isVecLd  (fuOpType: UInt): Bool = fuOpType(8, 7) === "b01".U
    def isFof    (fuOpType: UInt): Bool = isVecLd(fuOpType) && fuOpType(4)
  }

  object VstuType {
    // bit encoding: | padding (2bit) || mop (2bit) | sumop(5bit) |
    // only unit-stride use sumop
    // mop [1:0]
    // 0 0 : unit-stride
    // 0 1 : indexed-unordered
    // 1 0 : strided
    // 1 1 : indexed-ordered
    // sumop[4:0]
    // 0 0 0 0 0 : unit-stride load
    // 0 1 0 0 0 : unit-stride, whole register load
    // 0 1 0 1 1 : unit-stride, mask load, EEW=8
    def vse       = "b10_00_00000".U
    def vsr       = "b10_00_01000".U // whole
    def vsm       = "b10_00_01011".U // mask
    def vsuxe     = "b10_01_00000".U // index
    def vsse      = "b10_10_00000".U // strided
    def vsoxe     = "b10_11_00000".U // index

    def isUnitStride(fuOpType: UInt): Bool = fuOpType(6, 5) === "b00".U
    def isWhole  (fuOpType: UInt): Bool = fuOpType(6, 5) === "b00".U && fuOpType(4, 0) === "b01000".U && (fuOpType(8) ^ fuOpType(7))
    def isMasked (fuOpType: UInt): Bool = fuOpType(6, 5) === "b00".U && fuOpType(4, 0) === "b01011".U && (fuOpType(8) ^ fuOpType(7))
    def isStrided(fuOpType: UInt): Bool = fuOpType(6, 5) === "b10".U && (fuOpType(8) ^ fuOpType(7))
    def isIndexed(fuOpType: UInt): Bool = fuOpType(5) && (fuOpType(8) ^ fuOpType(7))
    def isVecSt  (fuOpType: UInt): Bool = fuOpType(8, 7) === "b10".U
  }

  object IF2VectorType {
    // use last 2 bits for vsew
    def iDup2Vec   = "b1_00".U
    def fDup2Vec   = "b1_01".U
    def immDup2Vec = "b1_10".U
    def i2Vec      = "b0_00".U
    def f2Vec      = "b0_01".U
    def imm2Vec    = "b0_10".U
    def needDup(bits: UInt): Bool = bits(2)
    def isImm(bits: UInt): Bool = bits(1)
    def isFp(bits: UInt): Bool = bits(0)
    def isFmv(bits: UInt): Bool = bits(0) & !bits(2)
    def FMX_D_X    = "b0_01_11".U
    def FMX_W_X    = "b0_01_10".U
    def FMX_H_X   =  "b0_01_01".U
  }

  object CommitType extends NamedUInt(2) {
    def NORMAL = b"00"  // int/fp
    def BRANCH = b"01"  // branch
    def LOAD   = b"10"  // load
    def STORE  = b"11"  // store

    def isLoadStore(commitType: UInt): Bool = commitType(1)
    def lsInstIsStore(commitType: UInt): Bool = commitType(0)
    def isStore(commitType: UInt): Bool = isLoadStore(commitType) && lsInstIsStore(commitType)
    def isBranch(commitType: UInt): Bool = commitType(0) && !commitType(1)
  }

  object RedirectLevel {
    def flushAfter = "b0".U
    def flush      = "b1".U

    def apply() = UInt(1.W)
    // def isUnconditional(level: UInt) = level(1)
    def flushItself(level: UInt) = level(0)
    // def isException(level: UInt) = level(1) && level(0)
  }

  trait SparseVecHelper[T <: Data] { this: SparseVec[T] =>
    val length: Int
    /**
     * get the element at [[idx]] as Option[T]
     * @param idx the index of element
     * @return Some(element) if exist, None otherwise
     */
    def find(idx: Int): Option[T] = this.elements.get(idx.toString)

    /**
     * get the element at [[idx]], or return [[default]] if not exist
     * @param idx the index of element
     * @param default the default value if the element does not exist
     * @tparam T1 the super type of T, to allow default value to be of a super type
     * @return the element at [[idx]] if exist, [[default]] otherwise
     */
    def findOrElse[T1 >: T](idx: Int, default: => T1): T1 = this.elements.getOrElse(idx.toString, default)

    /**
     * find the element at [[idx]], and perform function [[f]] on it if exist, or return [[default]] otherwise
     * @param idx the index of element
     * @param f the function to perform on the element if exist
     * @param default the default value if the element does not exist
     * @tparam R the return type of function [[f]] and default value
     * @return f(element) if exist, [[default]] otherwise
     */
    def getAndPerform[R](idx: Int)(f: T => R, default: R={}): R = {
      this.find(idx) match {
        case Some(bit) => f(bit)
        case None => default
      }
    }

    /**
     * perform function [[f]] on all existing elements, or [[default]] otherwise
     * @param f the function to perform on existing elements
     * @param default the default value if the element does not exist
     */
    def foreach(f: T => Unit, default: Unit = { /* do nothing */ }): Unit = {
      (0 until this.length).foreach { i =>
        this.getAndPerform(i)(f, default)
      }
    }
  }

  // For optional bits in the exception vector
  class ExceptSparseVec(val indices: Seq[Int]) extends SparseVec[Bool](
    size         = ExceptSparseVec.ExceptionVecSize,
    gen          = Bool(),
    indices      = indices,
    defaultValue = SparseVec.DefaultValueBehavior.UserSpecified(0.U)
  )  with SparseVecHelper[Bool] {
    override val length: Int = ExceptSparseVec.ExceptionVecSize

    /**
     * The apply method can get the element at [[idx]], or return [[false.B]] if not exist because exceptions will be
     * ignored by default
     * @param idx the index of element
     * @return the element at [[idx]] if exist, [[false.B]] otherwise
     */
    def apply(idx: Int): Bool = findOrElse(idx, false.B)

    // initialize all existing bits to false
    def zeroInit(): Unit = { this.foreach(_ := false.B) }

    /**
     * Map all existing bits with function [[f]]
     * @param f the function to map existing bits
     * @return a new ExceptSparseVec after mapping
     */
    def map(f: Bool => Bool): ExceptSparseVec = {
      val result = Wire(ExceptSparseVec(indices))
      this.elements.foreach { case (idx, source) =>
        result.elements(idx) := f(source)
      }
      result
    }

    /**
     * Convert this SparseVec to a Vec[Bool],
     * @param default the default value of a non-exist element ([[false.B]] as default)
     * @return a Vec[Bool] after convertion
     */
    def toVec: Vec[Bool] = {
      val result = Wire(Vec(length, Bool()))
      (0 until length).foreach { idx => result(idx) := this(idx) }
      result
    }

    /**
     * Convert this SparseVec to a UInt of length bits
     * the bit at idx 0 of the UInt corresponds to the element at idx 0 of this SparseVec
     * @return a UInt after convertion
     */
    def toUInt: UInt = {
      this.toVec.asUInt
    }

    // TODO: check whether the override of do_asUInt is done correctly
    override def do_asUInt(implicit sourceInfo: SourceInfo): UInt = toUInt

    /**
     * Or reduction operator
     * @return a hardware [[Bool]] resulting from every bit of this vector or'd together
     */
    def orR: Bool = this.toUInt.orR

    def nonEmpty: Boolean = this.indices.nonEmpty

    def select(sel: Seq[Int], unsel: Seq[Int]): ExceptSparseVec = {
      // assert if (select - unselect) is not a subset of this.indices,
      // the result will be wrong but no error will be thrown, so better to avoid this case
      val newIndices = sel.diff(unsel)
      assert(newIndices.toSet.subsetOf(this.indices.toSet), "ExceptSparseVec select indices mismatch")

      val result = Wire(ExceptSparseVec(newIndices))
      newIndices.foreach(i => result(i) := this(i))
      result
    }

    def select(sel: Seq[Int], unsel: Int): ExceptSparseVec          = select(sel, Seq(unsel))
    def select(sel: Seq[Int]): ExceptSparseVec                      = select(sel, Seq.empty)
    def selectByFu(cfg: FuConfig, unsel: Seq[Int]): ExceptSparseVec = select(cfg.exceptionOut, unsel)
    def selectByFu(cfg: FuConfig, unsel: Int): ExceptSparseVec      = select(cfg.exceptionOut, unsel)
    def selectByFu(cfg: FuConfig): ExceptSparseVec                  = select(cfg.exceptionOut)
    def unselect(unsel: Seq[Int]): ExceptSparseVec                  = select(this.indices, unsel)
    def unselect(unsel: Int): ExceptSparseVec                       = select(this.indices, unsel)

    // drive all bits in sink by corresponding bits in source.
    // if not exist in source, use default value false.B
    def extendFrom(source: ExceptSparseVec): Unit = {
      require(this.length == source.length, "ExceptSparseVec extendFrom() length mismatch")
      require(source.indices.toSet.subsetOf(this.indices.toSet),
        "ExceptSparseVec extendFrom(): source indices should be subset of sink indices")

      this.indices.foreach { idx => this (idx) := source(idx) }
    }

    /**
     * Bitwise OR operator, the resulting ExceptSparseVec will have bits that are the OR of corresponding bits
     * in this and right. the result has indices of the union of this.indices and right.indices
     * @param [[that]] the other ExceptSparseVec to OR with
     * @return OR result
     */
    def |(that: ExceptSparseVec): ExceptSparseVec = {
      ExceptSparseVec.orReduce(Seq(this, that))
    }

    /**
     * Bitwise AND operator, the resulting ExceptSparseVec will have bits that are the AND of corresponding bits
     * in this and right. the result has indices of the union of this.indices and right.indices
     * @param [[that]] the other ExceptSparseVec to AND with
     * @return AND result
     */
    def &(that: ExceptSparseVec): ExceptSparseVec = {
      ExceptSparseVec.orReduce(Seq(this, that))
    }
  }

  object ExceptSparseVec {
    val ExceptionVecSize = 24

    // Generate a 24-wide exception vector of Option[Bool], decided by whether the index is given by excpList
    // vec(i).get is a Bool only when i in excpList, otherwise None
    def apply(excpList: Seq[Int]): ExceptSparseVec = new ExceptSparseVec(excpList)
    def apply(): ExceptSparseVec = new ExceptSparseVec(ExceptionNO.all)

    @inline private def mergeHelper(operator: (Vec[Bool], Vec[Bool]) => Bool)(oh: Vec[Bool], seqs: Seq[ExceptSparseVec]): ExceptSparseVec = {
      require(seqs.nonEmpty, "ExceptSparseVec merge/select with empty seqs")
      require(oh.length == seqs.length, "ExceptSparseVec merge/select length mismatch")

      val mergeIndices = seqs.flatMap(_.indices).distinct.sorted
      val result = Wire(ExceptSparseVec(mergeIndices))

      result.elements.foreach { case (idx, source) =>
        val optionsAtIdx: Vec[Bool] = VecInit(seqs.map(_.elements.getOrElse(idx, false.B)))
        source := operator(optionsAtIdx, oh)
      }
      result
    }

    def orReduce(seqs: Seq[ExceptSparseVec]): ExceptSparseVec = {
      require(seqs.nonEmpty, "ExceptSparseVec merge with empty seqs")

      val mergeIndices = seqs.flatMap(_.indices).distinct.sorted
      val result = Wire(ExceptSparseVec(mergeIndices))

      result.indices.foreach { idx =>
        result(idx) := seqs.map(_(idx)).reduce(_ || _)
      }

      result
    }

    def mux1h(oh: Vec[Bool], seqs: Seq[ExceptSparseVec]): ExceptSparseVec = {
      val selectOperator = (ors: Vec[Bool], oh: Vec[Bool]) => Mux1H(oh, ors)
      mergeHelper(selectOperator)(oh, seqs)
    }

    def mux2(cond: Bool, seqTrue: ExceptSparseVec, seqFalse: ExceptSparseVec): ExceptSparseVec = {
      mux1h(VecInit(cond, ~cond), Seq(seqTrue, seqFalse))
    }

    def fill(excpList: Seq[Int], value: Bool): ExceptSparseVec = {
      val result = Wire(apply(excpList))
      result.foreach(_ := value)
      result
    }

    def zeros(excpList: Seq[Int]): ExceptSparseVec = fill(excpList, false.B)
  }

  object PMAMode {
    def R = "b1".U << 0 //readable
    def W = "b1".U << 1 //writeable
    def X = "b1".U << 2 //executable
    def I = "b1".U << 3 //cacheable: icache
    def D = "b1".U << 4 //cacheable: dcache
    def S = "b1".U << 5 //enable speculative access
    def A = "b1".U << 6 //enable atomic operation, A imply R & W
    def C = "b1".U << 7 //if it is cacheable is configable
    def Reserved = "b0".U

    def apply() = UInt(7.W)

    def read(mode: UInt) = mode(0)
    def write(mode: UInt) = mode(1)
    def execute(mode: UInt) = mode(2)
    def icache(mode: UInt) = mode(3)
    def dcache(mode: UInt) = mode(4)
    def speculate(mode: UInt) = mode(5)
    def atomic(mode: UInt) = mode(6)
    def configable_cache(mode: UInt) = mode(7)

    def strToMode(s: String) = {
      var result = 0.U(8.W)
      if (s.toUpperCase.indexOf("R") >= 0) result = result + R
      if (s.toUpperCase.indexOf("W") >= 0) result = result + W
      if (s.toUpperCase.indexOf("X") >= 0) result = result + X
      if (s.toUpperCase.indexOf("I") >= 0) result = result + I
      if (s.toUpperCase.indexOf("D") >= 0) result = result + D
      if (s.toUpperCase.indexOf("S") >= 0) result = result + S
      if (s.toUpperCase.indexOf("A") >= 0) result = result + A
      if (s.toUpperCase.indexOf("C") >= 0) result = result + C
      result
    }
  }

  val CSROpType = Opcode.CSROpType

  val JumpOpType = Opcode.JumpOpType

  val NewJumpOpType = Opcode.NewJumpOpType

  val LinkOpType = Opcode.LinkOpType

  val FenceOpType = Opcode.FenceOpType
  val ALUOpType = Opcode.ALUOpType

  object VSETOpType {
    val setVlmaxBit = 0
    val keepVlBit   = 1
    // destTypeBit == 0: write vl to rd
    // destTypeBit == 1: write vconfig
    val destTypeBit = 5

    // vsetvli's uop
    //   rs1!=x0, normal
    //     uop0: r(rs1), w(vconfig)     | x[rs1],vtypei  -> vconfig
    //     uop1: r(rs1), w(rd)          | x[rs1],vtypei  -> x[rd]
    def uvsetvcfg_xi        = "b1010_0000".U
    def uvsetrd_xi          = "b1000_0000".U
    //   rs1==x0, rd!=x0, set vl to vlmax, set rd to vlmax, set vtype
    //     uop0: w(vconfig)             | vlmax, vtypei  -> vconfig
    //     uop1: w(rd)                  | vlmax, vtypei  -> x[rd]
    def uvsetvcfg_vlmax_i   = "b1010_0001".U
    def uvsetrd_vlmax_i     = "b1000_0001".U
    //   rs1==x0, rd==x0, keep vl, set vtype
    //     uop0: r(vconfig), w(vconfig) | ld_vconfig.vl, vtypei -> vconfig
    def uvsetvcfg_keep_v    = "b1010_0010".U

    // vsetvl's uop
    //   rs1!=x0, normal
    //     uop0: r(rs1,rs2), w(vconfig) | x[rs1],x[rs2]  -> vconfig
    //     uop1: r(rs1,rs2), w(rd)      | x[rs1],x[rs2]  -> x[rd]
    def uvsetvcfg_xx        = "b0110_0000".U
    def uvsetrd_xx          = "b0100_0000".U
    //   rs1==x0, rd!=x0, set vl to vlmax, set rd to vlmax, set vtype
    //     uop0: r(rs2), w(vconfig)     | vlmax, vtypei  -> vconfig
    //     uop1: r(rs2), w(rd)          | vlmax, vtypei  -> x[rd]
    def uvsetvcfg_vlmax_x   = "b0110_0001".U
    def uvsetrd_vlmax_x     = "b0100_0001".U
    //   rs1==x0, rd==x0, keep vl, set vtype
    //     uop0: r(rs2), w(vtmp)             | x[rs2]               -> vtmp
    //     uop0: r(vconfig,vtmp), w(vconfig) | old_vconfig.vl, vtmp -> vconfig
    def uvmv_v_x            = "b0110_0010".U
    def uvsetvcfg_vv        = "b0111_0010".U

    // vsetivli's uop
    //     uop0: w(vconfig)             | vli, vtypei    -> vconfig
    //     uop1: w(rd)                  | vli, vtypei    -> x[rd]
    def uvsetvcfg_ii        = "b0010_0000".U
    def uvsetrd_ii          = "b0000_0000".U

    // read vec, write int
    // keep vl
    def csrrvl              = "b0001_0110".U

    def isVsetvl  (func: UInt)  = func(6)
    def isVsetvli (func: UInt)  = func(7)
    def isVsetivli(func: UInt)  = func(7, 6) === 0.U
    def isNormal  (func: UInt)  = func(1, 0) === 0.U
    def isSetVlmax(func: UInt)  = func(setVlmaxBit)
    def isKeepVl  (func: UInt)  = func(keepVlBit)
    // RG: region
    def writeIntRG(func: UInt)  = !func(5)
    def writeVecRG(func: UInt)  = func(5)
    def readIntRG (func: UInt)  = !func(4)
    def readVecRG (func: UInt)  = func(4)
    // modify fuOpType
    def keepVl(func: UInt)      = func | (1 << keepVlBit).U
    def setVlmax(func: UInt)    = func | (1 << setVlmaxBit).U
  }

  object BRUOpType {
    // branch
    def beq        = "b000_000".U
    def bne        = "b000_001".U
    def blt        = "b000_100".U
    def bge        = "b000_101".U
    def bltu       = "b001_000".U
    def bgeu       = "b001_001".U

    def getBranchType(func: UInt) = func(3, 1)
    def isBranchInvert(func: UInt) = func(0)
  }

  object DIVOpType {
    // div
    // bit encoding: | type (2bit) | isWord(1bit) | isSign(1bit) | opcode(1bit) |
    def div    = "b10000".U
    def divu   = "b10010".U
    def rem    = "b10001".U
    def remu   = "b10011".U

    def divw   = "b10100".U
    def divuw  = "b10110".U
    def remw   = "b10101".U
    def remuw  = "b10111".U

    def isSign(op: UInt) = !op(1)
    def isW(op: UInt) = op(2)
    def isH(op: UInt) = op(0)
  }

  val LSUOpType = Opcode.LSUOpType

  object BKUOpType {

    def clmul       = "b000000".U
    def clmulh      = "b000001".U
    def clmulr      = "b000010".U
    def xpermn      = "b000100".U
    def xpermb      = "b000101".U

    def clz         = "b001000".U
    def clzw        = "b001001".U
    def ctz         = "b001010".U
    def ctzw        = "b001011".U
    def cpop        = "b001100".U
    def cpopw       = "b001101".U

    // 01xxxx is reserve
    def aes64es     = "b100000".U
    def aes64esm    = "b100001".U
    def aes64ds     = "b100010".U
    def aes64dsm    = "b100011".U
    def aes64im     = "b100100".U
    def aes64ks1i   = "b100101".U
    def aes64ks2    = "b100110".U

    // merge to two instruction sm4ks & sm4ed
    def sm4ed0      = "b101000".U
    def sm4ed1      = "b101001".U
    def sm4ed2      = "b101010".U
    def sm4ed3      = "b101011".U
    def sm4ks0      = "b101100".U
    def sm4ks1      = "b101101".U
    def sm4ks2      = "b101110".U
    def sm4ks3      = "b101111".U

    def sha256sum0  = "b110000".U
    def sha256sum1  = "b110001".U
    def sha256sig0  = "b110010".U
    def sha256sig1  = "b110011".U
    def sha512sum0  = "b110100".U
    def sha512sum1  = "b110101".U
    def sha512sig0  = "b110110".U
    def sha512sig1  = "b110111".U

    def sm3p0       = "b111000".U
    def sm3p1       = "b111001".U
  }

  object BTBtype {
    def B = "b00".U  // branch
    def J = "b01".U  // jump
    def I = "b10".U  // indirect
    def R = "b11".U  // return

    def apply() = UInt(2.W)
  }

  object SelImm {
    def IMM_S  = "b0001".U
    def IMM_SB = "b0010".U
    def IMM_U  = "b0011".U
    def IMM_UJ = "b0100".U
    def IMM_I  = "b0101".U
    def IMM_Z  = "b0110".U

    def IMM_OPIVIS    = "b1000".U
    def IMM_OPIVIU    = "b1001".U
    def IMM_VSETVLI   = "b1010".U
    def IMM_VSETIVLI  = "b1011".U
    def IMM_VRORVI    = "b1100".U
    def IMM_LUI32     = "b1110".U

    def X      = BitPat("b0000")

    def apply() = UInt(4.W)

    def mkString(immType: UInt) : String = {
      val strMap = Map(
        IMM_S.litValue         -> "S",
        IMM_SB.litValue        -> "SB",
        IMM_U.litValue         -> "U",
        IMM_UJ.litValue        -> "UJ",
        IMM_I.litValue         -> "I",
        IMM_Z.litValue         -> "Z",
        IMM_OPIVIS.litValue    -> "VIS",
        IMM_OPIVIU.litValue    -> "VIU",
        IMM_VSETVLI.litValue   -> "VSETVLI",
        IMM_VSETIVLI.litValue  -> "VSETIVLI",
        IMM_LUI32.litValue     -> "LUI32",
        IMM_VRORVI.litValue    -> "VRORVI",
      )
      strMap(immType.litValue)
    }

    def getImmUnion(immType: UInt) : Imm = {
      val iuMap = Map(
        IMM_S.litValue         -> ImmUnion.S,
        IMM_SB.litValue        -> ImmUnion.B,
        IMM_U.litValue         -> ImmUnion.U,
        IMM_UJ.litValue        -> ImmUnion.J,
        IMM_I.litValue         -> ImmUnion.I,
        IMM_Z.litValue         -> ImmUnion.Z,
        IMM_OPIVIS.litValue    -> ImmUnion.OPIVIS,
        IMM_OPIVIU.litValue    -> ImmUnion.OPIVIU,
        IMM_VSETVLI.litValue   -> ImmUnion.VSETVLI,
        IMM_VSETIVLI.litValue  -> ImmUnion.VSETIVLI,
        IMM_LUI32.litValue     -> ImmUnion.LUI32,
        IMM_VRORVI.litValue    -> ImmUnion.VRORVI,
      )
      iuMap(immType.litValue)
    }
  }

  object UopSplitType {
    def SCA_SIM          = "b000000".U //
    def VSET             = "b010001".U // dirty: vset
    def VEC_VVV          = "b010010".U // VEC_VVV
    def VEC_VXV          = "b010011".U // VEC_VXV
    def VEC_0XV          = "b010100".U // VEC_0XV
    def VEC_VVW          = "b010101".U // VEC_VVW
    def VEC_WVW          = "b010110".U // VEC_WVW
    def VEC_VXW          = "b010111".U // VEC_VXW
    def VEC_WXW          = "b011000".U // VEC_WXW
    def VEC_WVV          = "b011001".U // VEC_WVV
    def VEC_WXV          = "b011010".U // VEC_WXV
    def VEC_EXT2         = "b011011".U // VF2 0 -> V
    def VEC_EXT4         = "b011100".U // VF4 0 -> V
    def VEC_EXT8         = "b011101".U // VF8 0 -> V
    def VEC_VVM          = "b011110".U // VEC_VVM
    def VEC_VXM          = "b011111".U // VEC_VXM
    def VEC_SLIDE1UP     = "b100000".U // vslide1up.vx
    def VEC_FSLIDE1UP    = "b100001".U // vfslide1up.vf
    def VEC_SLIDE1DOWN   = "b100010".U // vslide1down.vx
    def VEC_FSLIDE1DOWN  = "b100011".U // vfslide1down.vf
    def VEC_VRED         = "b100100".U // VEC_VRED
    def VEC_SLIDEUP      = "b100101".U // VEC_SLIDEUP
    def VEC_SLIDEDOWN    = "b100111".U // VEC_SLIDEDOWN
    def VEC_M0X          = "b101001".U // VEC_M0X  0MV
    def VEC_MVV          = "b101010".U // VEC_MVV  VMV
    def VEC_VWW          = "b101100".U //
    def VEC_RGATHER      = "b101101".U // vrgather.vv, vrgather.vi
    def VEC_RGATHER_VX   = "b101110".U // vrgather.vx
    def VEC_RGATHEREI16  = "b101111".U // vrgatherei16.vv
    def VEC_COMPRESS     = "b110000".U // vcompress.vm
    def VEC_US_LDST      = "b110001".U // vector unit-strided load/store
    def VEC_S_LDST       = "b110010".U // vector strided load/store
    def VEC_I_LDST       = "b110011".U // vector indexed load/store
    def VEC_US_FF_LD     = "b110100".U // vector unit-stride fault-only-first load
    def VEC_VFV          = "b111000".U // VEC_VFV
    def VEC_VFW          = "b111001".U // VEC_VFW
    def VEC_WFW          = "b111010".U // VEC_WVW
    def VEC_VFM          = "b111011".U // VEC_VFM
    def VEC_VFRED        = "b111100".U // VEC_VFRED
    def VEC_VFREDOSUM    = "b111101".U // VEC_VFREDOSUM
    def VEC_MVNR         = "b000100".U // vmvnr

    def AMO_CAS_W        = "b110101".U // amocas_w
    def AMO_CAS_D        = "b110110".U // amocas_d
    def AMO_CAS_Q        = "b110111".U // amocas_q
    // dummy means that the instruction is a complex instruction but uop number is 1
    def dummy     = "b111111".U

    def X = BitPat("b000000")

    def apply() = UInt(6.W)
    def needSplit(UopSplitType: UInt) = UopSplitType(4) || UopSplitType(5)

    def isAMOCAS(UopSplitType: UInt): Bool = UopSplitType === AMO_CAS_W || UopSplitType === AMO_CAS_D || UopSplitType === AMO_CAS_Q
  }

  object ExceptionNO {
    def instrAddrMisaligned = 0
    def instrAccessFault    = 1
    def illegalInstr        = 2
    def breakPoint          = 3
    def loadAddrMisaligned  = 4
    def loadAccessFault     = 5
    def storeAddrMisaligned = 6
    def storeAccessFault    = 7
    def ecallU              = 8
    def ecallS              = 9
    def ecallVS             = 10
    def ecallM              = 11
    def instrPageFault      = 12
    def loadPageFault       = 13
    // def singleStep          = 14
    def storePageFault      = 15
    def doubleTrap          = 16
    def softwareCheck       = 18
    def hardwareError       = 19
    def instrGuestPageFault = 20
    def loadGuestPageFault  = 21
    def virtualInstr        = 22
    def storeGuestPageFault = 23

    // Just alias
    def EX_IAM    = instrAddrMisaligned
    def EX_IAF    = instrAccessFault
    def EX_II     = illegalInstr
    def EX_BP     = breakPoint
    def EX_LAM    = loadAddrMisaligned
    def EX_LAF    = loadAccessFault
    def EX_SAM    = storeAddrMisaligned
    def EX_SAF    = storeAccessFault
    def EX_UCALL  = ecallU
    def EX_HSCALL = ecallS
    def EX_VSCALL = ecallVS
    def EX_MCALL  = ecallM
    def EX_IPF    = instrPageFault
    def EX_LPF    = loadPageFault
    def EX_SPF    = storePageFault
    def EX_DT     = doubleTrap
    def EX_SWC    = softwareCheck
    def EX_HWE    = hardwareError
    def EX_IGPF   = instrGuestPageFault
    def EX_LGPF   = loadGuestPageFault
    def EX_VI     = virtualInstr
    def EX_SGPF   = storeGuestPageFault

    def getAddressMisaligned = Seq(EX_IAM, EX_LAM, EX_SAM)

    def getAccessFault = Seq(EX_IAF, EX_LAF, EX_SAF)

    def getPageFault = Seq(EX_IPF, EX_LPF, EX_SPF)

    def getGuestPageFault = Seq(EX_IGPF, EX_LGPF, EX_SGPF)

    def getLSGuestPageFault = Seq(EX_LGPF, EX_SGPF)

    def getFetchFault = Seq(EX_IAM, EX_IAF, EX_IPF)

    def getLoadFault = Seq(EX_LAM, EX_LAF, EX_LPF, EX_HWE)

    def getStoreFault = Seq(EX_SAM, EX_SAF, EX_SPF, EX_HWE)

    def priorities = Seq(
      doubleTrap,
      breakPoint, // TODO: different BP has different priority
      instrPageFault,
      instrGuestPageFault,
      instrAccessFault,
      softwareCheck,
      illegalInstr,
      virtualInstr,
      instrAddrMisaligned,
      ecallM, ecallS, ecallVS, ecallU,
      storeAddrMisaligned,
      loadAddrMisaligned,
      storePageFault,
      loadPageFault,
      storeGuestPageFault,
      loadGuestPageFault,
      storeAccessFault,
      loadAccessFault,
      hardwareError
    )

    def getHigherExcpThan(excp: Int): Seq[Int] = {
      val idx = this.priorities.indexOf(excp, 0)
      require(idx != -1, s"The irq($excp) does not exists in IntPriority Seq")
      this.priorities.slice(0, idx)
    }

    def all = priorities.distinct.sorted
    def frontendSet = Seq(
      instrAddrMisaligned,
      instrAccessFault,
      illegalInstr,
      instrPageFault,
      instrGuestPageFault,
      virtualInstr,
      breakPoint
    )
    def fromFrontendSet = Seq(
      instrAccessFault,
      illegalInstr,
      instrPageFault,
      hardwareError,
      instrGuestPageFault
    )
    def decodeSet = Seq(
      instrAccessFault,
      illegalInstr,
      breakPoint, // new
      instrPageFault,
      hardwareError,
      instrGuestPageFault,
      virtualInstr, // new
    )
    def exceptionGenSet(params: BackendParams) = (params.exceptionOut ++ decodeSet).distinct.sorted
  }

  object TopDownCounters extends Enumeration {
    val NoStall = Value("NoStall") // Base
    // frontend
    val OverrideBubble = Value("OverrideBubble")
    val FtqUpdateBubble = Value("FtqUpdateBubble")
    // val ControlRedirectBubble = Value("ControlRedirectBubble")
    val TAGEMissBubble = Value("TAGEMissBubble")
    val SCMissBubble = Value("SCMissBubble")
    val ITTAGEMissBubble = Value("ITTAGEMissBubble")
    val RASMissBubble = Value("RASMissBubble")
    val MemVioRedirectBubble = Value("MemVioRedirectBubble")
    val OtherRedirectBubble = Value("OtherRedirectBubble")
    val FtqFullStall = Value("FtqFullStall")

    val ICacheMissBubble = Value("ICacheMissBubble")
    val ITLBMissBubble = Value("ITLBMissBubble")
    val BTBMissBubble = Value("BTBMissBubble")
    val FetchFragBubble = Value("FetchFragBubble")
    val FrontendOtherCoreStall = Value("FrontendOtherCoreStall")

    // backend
    // long inst stall at rob head
    val DivStall = Value("DivStall") // int div, float div/sqrt
    val IntNotReadyStall = Value("IntNotReadyStall") // int-inst at rob head exec long
    val FPNotReadyStall = Value("FPNotReadyStall") // fp-inst at rob head exec long
    val MemNotReadyStall = Value("MemNotReadyStall") // mem-inst at rob head exec long
    val OtherNotReadyStall = Value("OtherNotReadyStall")
    val RobStall = Value("RobStall")
    val LqStall = Value("LqStall")
    val SqStall = Value("SqStall")
    // freelist full
    val IntFlStall = Value("IntFlStall")
    val FpFlStall = Value("FpFlStall")
    val VecFlStall = Value("VecFlStall")
    val V0FlStall = Value("V0FlStall")
    val VlFlStall = Value("VlFlStall")
    val MultiFlStall = Value("MultiFlStall")
    // fusion bubble
    val FusionBubble = Value("FusionBubble")
    // dispatch stall
    // dispatch stall for dispatch policy
    // TODO: explain only load store exist
    val LoadDispatchPolicyStall = Value("LoadDispatchPolicyStall")
    val StoreDispatchPolicyStall = Value("StoreDispatchPolicyStall")
    val OtherDispatchPolicyStall = Value("OtherDispatchPolicyStall")
    // dispatch stall for issuequeue full
    val BalanceDispatchPolicyStallAlu = Value("BalanceDispatchPolicyStallAlu")
    val BalanceDispatchPolicyStallBrh = Value("BalanceDispatchPolicyStallBrh")
    val BalanceDispatchPolicyStallInt = Value("BalanceDispatchPolicyStallInt")
    val BalanceDispatchPolicyStallFp = Value("BalanceDispatchPolicyStallFp")
    val BalanceDispatchPolicyStallVec = Value("BalanceDispatchPolicyStallVec")
    val BalanceDispatchPolicyStallLoad = Value("BalanceDispatchPolicyStallLoad")
    val BalanceDispatchPolicyStallStore = Value("BalanceDispatchPolicyStallStore")
    val OtherBalanceDispatchPolicyStall = Value("OtherBalanceDispatchPolicyStall")
    val IQEnqPolicyStallIssued = Value("IQEnqPolicyStallIssued")
    val IQEnqPolicyStall = Value("IQEnqPolicyStall")
    val IntIQFullStallAlu = Value("IntIQFullStallAlu")
    val IntIQFullStallBrh = Value("IntIQFullStallBrh")
    val IntIQFullStallOther = Value("IntIQFullStallOther")
    val FpIQFullStall = Value("FpIQFullStall")
    val VecIQFullStall = Value("VecIQFullStall")
    val LoadIQFullStall = Value("LoadIQFullStall")
    val StoreIQFullStall = Value("StoreIQFullStall")
    val OtherIQFullStall = Value("OtherIQFullStall")

    // memblock
    val LoadTLBStall = Value("LoadTLBStall")
    val LoadL1Stall = Value("LoadL1Stall")
    val LoadL2Stall = Value("LoadL2Stall")
    val LoadL3Stall = Value("LoadL3Stall")
    val LoadMemStall = Value("LoadMemStall")
    val StoreStall = Value("StoreStall") // include store tlb miss
    val AtomicStall = Value("AtomicStall") // atomic, load reserved, store conditional

    // xs replay (different to gem5)
    val LoadVioReplayStall = Value("LoadVioReplayStall")
    val LoadMSHRReplayStall = Value("LoadMSHRReplayStall")

    // bad speculation
    val ControlRedirectStall = Value("ControlRedirectStall")
    val MemVioRedirectStall = Value("MemVioRedirectStall")
    val OtherRedirectStall = Value("OtherRedirectStall")
    val ControlRecoveryStall = Value("ControlRecoveryStall")
    val MemVioRecoveryStall = Value("MemVioRecoveryStall")
    val OtherRecoveryStall = Value("OtherRecoveryStall")

    val FlushedInsts = Value("FlushedInsts") // control flushed, memvio flushed, others
    val SpecialInsts = Value("SpecialInsts")

    val BackendOtherCoreStall = Value("BackendOtherCoreStall")

    val NumStallReasons = Value("NumStallReasons")
  }
}
