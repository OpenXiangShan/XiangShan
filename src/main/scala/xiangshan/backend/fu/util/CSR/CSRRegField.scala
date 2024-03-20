package xiangshan.backend.fu.util.CSR

import chisel3._
import chisel3.aop.Select.When
import chisel3.experimental.SourceInfo
import chisel3.experimental.BundleLiterals._
import chisel3.internal.firrtl.Width
import chisel3.util.{Cat, MixedVec, MuxCase, RegEnable, ValidIO, Mux1H}
import com.fasterxml.jackson.databind.exc.MismatchedInputException
import top.{ArgParser, Generator}
import utils.OptionWrapper
import xiangshan.backend.fu.util.CSR.CSRDefines._
import xiangshan.macros.CSRMacros.CSRFieldsImpl
import xiangshan.backend.fu.util.CSRDef
import chisel3.internal.firrtl._

import scala.collection.{SeqMap, immutable}
import scala.language.experimental.macros
import scala.reflect.api._
import scala.reflect.macros.blackbox.Context
import scala.reflect._


class CSROldModule[T <: CSRBundle](
  val modName: String,
  val bundle : T,
) extends Module {

  println(s"[CSRModule] ${bundle.getFields.map(_.localName)}")

  override def desiredName: String = "Module" + modName

  // bind bundle and its fields to CSRModule
  bundle.bindCSRModule(this)

  /**
   *  read from and write to these modules
   */
  val refMods = this.getRefModule(bundle)

  this.registerRefedMod(refMods)
  // pass read data to and write data from these modules
  var refedMods: Seq[CSROldModule[_]] = Seq()

  val io = IO(new CSRModuleIO)

  val regs = Reg(bundle.cloneType)
  regs.elements.foreach { case (name: String, field: CSREnumType) =>
    if (field.refField.isEmpty) {
      when(io.w.valid) {
        field := field.factory(field.wfn(io.w.bits.elements(name).asUInt, field.asUInt, Seq()))
      }

    }
  }

  val rRefData = io.rFromRef

  val rdata = /*Wire(gen.cloneType)*/ mergeRData(regs, rRefData.toSeq)
  rdata :|= regs

  io.r.data := rdata.asUInt
  io.rToRef := rdata
  io.wToRef.valid := io.w.valid
  io.wToRef.bits.foreach { x =>
    x := DontCare
    for (field <- io.w.bits.getFields) {
      if (field.refField.nonEmpty) {
        x.elements(field.refField.get.localName) := field
      }
    }
  }

  class CSRModuleIO extends Bundle {
    val w = new Bundle {
      val valid = Input(Bool())
      val bits = Flipped(bundle)
    }
    val r = Output(new Bundle {
      val data = UInt()
    })
    val rFromRef = Input(genReadFromRefBundle)
    val rToRef = Output(bundle)
    val wToRef = Output(ValidIO(genWriteToRefBundle))
    val wFromRef = Flipped(ValidIO(bundle))

    println(s"CSRModuleIO ${rFromRef.map(_.mod)}")
  }

  def hasRefField: Boolean = {
    bundle.getElements.map(_.asInstanceOf[CSREnumType].refField.nonEmpty).fold(false)(_ || _)
  }

  def mergeRData(regField: T, refFields: Seq[CSRBundle]): T = {
    val rdata = Wire(bundle)
    println(s"refFields.size: ${refFields.size}")
    println("mergeRData: ", refFields.map(_.mod))
    println("mergeRData: ", refFields.map(_.mod.get.modName))
    val refFieldsMap: Map[String, CSRBundle] = refFields.map(x => (x.mod.get.modName -> x)).toMap
    val nameSeq = rdata.elements.keys.toIndexedSeq
    println(rdata.getFields)
    rdata.getFields.zipWithIndex.foreach { case (field: CSREnumType, i) =>
      if (field.refField.nonEmpty) {
        println(Arg.earlyLocalName(field))
        field := refFieldsMap(field.refField.get.getCSRModule.modName).elements(nameSeq(i))
      } else {
        field := regField.getFields(i)
      }
    }
    rdata
  }

  // get the reference module
  def getRefModule(bundle: CSRBundle): Seq[CSROldModule[_]] = {
    bundle.getFields.flatMap(_.refField).map(_.getCSRModule).distinct
  }

  def genReadFromRefBundle: MixedVec[CSRBundle] = {
    println(s"genReadFromRefBundle ${this.refMods.map(_.parentModName)}")
    MixedVec(this.refMods.map(_.bundle.asInstanceOf[CSRBundle]))
  }

  def genWriteToRefBundle: MixedVec[CSRBundle] = {
    println(s"genWriteToRefBundle ${this.refMods.map(_.parentModName)}")
    MixedVec(this.refMods.map(_.bundle.asInstanceOf[CSRBundle]))
  }

  def genReadToRefBundle = {
    this.refedMods
  }

  def addRefedCSRModule(mod: CSROldModule[_]): Unit = {
    this.refedMods :+= mod
  }

  def registerRefedMod(refMods: Seq[CSROldModule[_]]) = for (mod <- refMods) {
    mod.addRefedCSRModule(this)
  }
}

abstract class CSRBundle extends Bundle {
  val len: Int = 64
  var mod: Option[CSROldModule[_]] = None

  override def do_asUInt(implicit sourceInfo: SourceInfo): UInt = {
    val fields = this.getFields
    var paddedFields: Seq[Data] = Seq()
    var lsb = len

    for (field <- fields) {
      val diffWidth = lsb - field.lsb - field.getWidth
      if (diffWidth > 0)
        paddedFields :+= 0.U((diffWidth).W)
      paddedFields :+= field
      lsb = field.lsb
    }

    if (fields.last.lsb > 0) {
      paddedFields :+= 0.U(fields.last.lsb.W)
    }

    Cat(paddedFields.map(x => x.asUInt))
  }

  def := (that: UInt): Unit = {
    val fields = this.getFields

    for (field <- fields) {
      field := field.factory.apply(that(field.lsb + field.getWidth - 1, field.lsb))
    }
  }

  /**
   * filtered read connection
   *
   * CSRBundle will be filtered by CSRFields' read filter function.
   */
  def :|= [T <: CSRBundle](that: T): Unit = {
    if (this.getClass != that.getClass) {
      throw MonoConnectException(s"Type miss match! (sink :|= source) " +
        s"sink type: ${this.getClass}, " +
        s"source type: ${that.getClass}")
    }

    for ((sink: CSREnumType, source: CSREnumType)  <- this.getFields.zip(that.getFields)) {
      sink := sink.factory.apply(sink.rfn(source.asUInt, Seq()))
    }
  }

  def getFields = this.getElements.map(_.asInstanceOf[CSREnumType])

  def bindCSRModule(mod: CSROldModule[_]): Unit = {
    this.mod = Some(mod)
    this.getFields.foreach(_.bindCSRModule(mod))
  }

  override def cloneType: CSRBundle.this.type = {
    val c = super.cloneType
    c.mod = this.mod
    c
  }
}

class NewCSR extends Module with CSRFuncTrait {
  val io = IO(new Bundle {
    val w = Flipped(ValidIO(new Bundle {
      val addr = UInt(12.W)
      val data = UInt(64.W)
    }))
    val trap = Flipped(ValidIO(new Bundle {
      val toPRVM = PrivMode()
      val toV = VirtMode()
    }))
    val tret = Flipped(ValidIO(new Bundle {
      val toPRVM = PrivMode()
      val toV = VirtMode()
    }))
  })

  val addr = io.w.bits.addr
  val data = io.w.bits.data
  val wen = io.w.valid

  val PRVM = RegInit(PrivMode.M)
  val V = RegInit(VirtMode.Off)

  val trap = io.trap.valid
  val trapToPRVM = io.trap.bits.toPRVM
  val trapToV = io.trap.bits.toV
  val trapToM = trapToPRVM === PrivMode.M
  val trapToHS = trapToPRVM === PrivMode.S && trapToV === VirtMode.Off
  val trapToHU = trapToPRVM === PrivMode.U && trapToV === VirtMode.Off
  val trapToVS = trapToPRVM === PrivMode.S && trapToV === VirtMode.On
  val trapToVU = trapToPRVM === PrivMode.U && trapToV === VirtMode.On

  val tret = io.tret.valid
  val tretPRVM = io.tret.bits.toPRVM
  val tretV = io.tret.bits.toV
  val isSret = tret && tretPRVM === PrivMode.S
  val isMret = tret && tretPRVM === PrivMode.M

//  val hip = Module(new CSRModule("Hip", new CSRBundle {
//    val VSSIP = CSRFieldWARLBits( 2, wNoFilter)
//    val VSTIP = CSRFieldWARLBits( 6, wNoEffect)
//    val VSEIP = CSRFieldWARLBits(10, wNoEffect)
//    val SGEIP = CSRFieldWARLBits(12, wNoEffect)
//  }))

  val mstatus = Module(new MstatusModule)

  val mtvec = Module(new CSRModule("Mtvec", new CSRBundle {
    val mode = MtvecMode(1, 0, wNoFilter)
    val addr = CSRFieldWARLBits(63, 2, wNoFilter)
  }) {
    when (wen && wdata.mode.isLegal) { reg.mode := wdata.mode }
      .otherwise(reg.mode := reg.mode)
  } )

  val fcsr = Module(new CSRModule("Fcsr", new CSRBundle {
    val NX = CSRFieldWARLBits(0, wNoFilter)
    val UF = CSRFieldWARLBits(1, wNoFilter)
    val OF = CSRFieldWARLBits(2, wNoFilter)
    val DZ = CSRFieldWARLBits(3, wNoFilter)
    val NV = CSRFieldWARLBits(4, wNoFilter)
    val FRM = CSRFieldWARLBits(7, 5, wNoFilter)
  }) {
    val wAliasFflags = IO(Input(new CSRAddrWriteBundle(new CSRBundle{
      val NX = CSRFieldWARLBits(0, wNoFilter)
      val UF = CSRFieldWARLBits(1, wNoFilter)
      val OF = CSRFieldWARLBits(2, wNoFilter)
      val DZ = CSRFieldWARLBits(3, wNoFilter)
      val NV = CSRFieldWARLBits(4, wNoFilter)
    })))
    val wAliasFfm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val FRM = CSRFieldWARLBits(2, 0, wNoFilter)
    })))
    val fflags = IO(Output(UInt(64.W)))
    val frm = IO(Output(UInt(64.W)))

    dontTouch(fflags)
    dontTouch(frm)
    dontTouch(wAliasFflags)
    dontTouch(wAliasFfm)
    // write connection
    this.wfn(reg)(Seq(wAliasFflags, wAliasFfm))

    // read connection
    fflags := reg.asUInt(4, 0)
    frm := reg.FRM.asUInt
  })

  val CSRWMap: immutable.SeqMap[Int, CSRAddrWriteBundle[_ <: CSRBundle]] = SeqMap(
    0x001 -> fcsr.wAliasFflags,
    0x002 -> fcsr.wAliasFfm,
    0x003 -> fcsr.w,
    0x100 -> mstatus.wAliasSstatus,
    0x300 -> mstatus.w,
    0x305 -> mtvec.w,
//    0x644 -> hip,
  )

  val csrMods = Seq(
    fcsr,
    mstatus,
    mtvec,
  )

  for ((id, wBundle) <- CSRWMap) {
    wBundle.wen := wen && addr === id.U
    wBundle.wdata := data
  }

  csrMods.foreach { mod =>
    mod.commonIn.status := mstatus.mstatus
    mod.commonIn.prvm := PRVM
    mod.commonIn.v := V

    dontTouch(mod.rdata)
    dontTouch(mod.commonIn)
  }
}

object NewCSRMain extends App with CSRDef {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform")

  Generator.execute(
    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "backend",
    new NewCSR,
    Array()
  )
  println("done")
}
