/***************************************************************************************
* Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.backend.fu.NewCSR

import chisel3._
import freechips.rocketchip.rocket.CSRs
import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}
import system.SoCParamsKey
import xiangshan.PMParameKey
import xiangshan.backend.fu.PMAConfigEntry
import xiangshan.backend.fu.util.CSRConst
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSRFunc._

trait CSRDocDump { self: NewCSR =>
  import CSRConfig._

  private class VxsatAliasBundle extends CSRBundle {
    val VXSAT = CSRRWField(0).withDescription("Vector fixed-point saturation accrued flag.")
  }

  private class VxrmAliasBundle extends CSRBundle {
    val VXRM = CSRRWField(1, 0).withDescription("Vector fixed-point rounding mode.")
  }

  private class FcsrAliasBundle extends CSRBundle {
    val NX  = CSRWARLField(0, wNoFilter).withDescription("Inexact accrued exception flag.")
    val UF  = CSRWARLField(1, wNoFilter).withDescription("Underflow accrued exception flag.")
    val OF  = CSRWARLField(2, wNoFilter).withDescription("Overflow accrued exception flag.")
    val DZ  = CSRWARLField(3, wNoFilter).withDescription("Divide-by-zero accrued exception flag.")
    val NV  = CSRWARLField(4, wNoFilter).withDescription("Invalid-operation accrued exception flag.")
    val FRM = CSRWARLField(2, 0, wNoFilter).withReset(0.U).withDescription("Floating-point dynamic rounding mode.")
  }

  private def csrCsvPath: Path = {
    Paths.get(sys.env.getOrElse("NOOP_HOME", ".")).resolve("build").resolve("csr.csv")
  }

  private def sanitizeCSVCell(value: String): String = {
    Option(value).getOrElse("").replace('\r', ' ').replace('\n', ' ').trim
  }

  private def csvEscape(value: String): String = {
    "\"" + sanitizeCSVCell(value).replace("\"", "\"\"") + "\""
  }

  private def writeCSVRow(writer: PrintWriter, values: Seq[String]): Unit = {
    writer.println(values.map(csvEscape).mkString(","))
  }

  private def fieldResetValueString(bundle: CSRBundle, field: CSREnumType): String = {
    val resetValue =
      if (field.init != null) Some(field.init.litValue)
      else if (bundle.needReset) Some(BigInt(0))
      else None
    resetValue.map(value => s"0x${value.toString(16)}").getOrElse("")
  }

  private def fieldDescriptionString(fieldName: String, field: CSREnumType): String = {
    sanitizeCSVCell(field.getDocumentedDescription(fieldName))
  }

  private def writeFieldCSV(
    writer: PrintWriter,
    group: String,
    csrName: String,
    csrAddr: Int,
    implName: String,
    fieldName: String,
    rwType: String,
    msb: Int,
    lsb: Int,
    reset: String,
    isRef: Boolean,
    isHardWired: Boolean,
    description: String,
  ): Unit = {
    val width = msb - lsb + 1
    writeCSVRow(writer, Seq(
      group,
      csrName,
      s"0x${csrAddr.toHexString}",
      implName,
      fieldName,
      rwType,
      msb.toString,
      lsb.toString,
      width.toString,
      reset,
      isRef.toString,
      isHardWired.toString,
      description,
    ))
  }

  private def dumpBundleCSV(
    writer: PrintWriter,
    group: String,
    csrName: String,
    csrAddr: Int,
    bundle: CSRBundle,
    implName: String,
  ): Unit = {
    bundle.elements.foreach { case (fieldName, field: CSREnumType) =>
      writeFieldCSV(
        writer,
        group,
        csrName,
        csrAddr,
        implName,
        fieldName,
        field.rwType.toString,
        field.msb,
        field.lsb,
        fieldResetValueString(bundle, field),
        field.isRef,
        field.isHardWired,
        fieldDescriptionString(fieldName, field),
      )
    }
  }

  private def dumpSelectedBundleCSV(
    writer: PrintWriter,
    group: String,
    csrName: String,
    csrAddr: Int,
    bundle: CSRBundle,
    implName: String,
    selectedFields: Seq[String],
  ): Unit = {
    selectedFields.foreach { fieldName =>
      val field = bundle.elements(fieldName).asInstanceOf[CSREnumType]
      writeFieldCSV(
        writer,
        group,
        csrName,
        csrAddr,
        implName,
        fieldName,
        field.rwType.toString,
        field.msb,
        field.lsb,
        fieldResetValueString(bundle, field),
        field.isRef,
        field.isHardWired,
        fieldDescriptionString(fieldName, field),
      )
    }
  }

  private def dumpModuleCSV(writer: PrintWriter, group: String, mod: CSRModule[_]): Unit = {
    dumpBundleCSV(writer, group, mod.modName, mod.addr, mod.bundle.asInstanceOf[CSRBundle], mod.modName)
  }

  private def dumpTriggerCSVs(writer: PrintWriter): Unit = {
    tdata1RegVec.zip(tdata2RegVec).zipWithIndex.foreach { case ((tdata1Mod, tdata2Mod), idx) =>
      dumpBundleCSV(
        writer,
        "D",
        s"tdata1[$idx]",
        CSRs.tdata1,
        tdata1Mod.bundle.asInstanceOf[CSRBundle],
        tdata1Mod.modName,
      )
      dumpBundleCSV(
        writer,
        "D",
        s"mcontrol6[$idx]",
        CSRs.tdata1,
        new Mcontrol6,
        s"${tdata1Mod.modName}(tdata1.DATA view)",
      )
      dumpBundleCSV(
        writer,
        "D",
        s"tdata2[$idx]",
        CSRs.tdata2,
        tdata2Mod.bundle.asInstanceOf[CSRBundle],
        tdata2Mod.modName,
      )
    }
  }

  private def pmaInitEntries: Seq[PMAConfigEntry] = {
    val numPMAReal = p(PMParameKey).NumPMAReal
    val configs = p(SoCParamsKey).PMAConfigs
    (configs ++ Seq.fill(numPMAReal - configs.length)(PMAConfigEntry(0))).reverse
  }

  private def pmaGenAddr(init: PMAConfigEntry): BigInt = {
    if (init.a < 2) {
      init.base_addr >> PMPOffBits
    } else {
      val platformGrainBytes = BigInt(1) << p(PMParameKey).PlatformGrain
      require((init.base_addr % platformGrainBytes) == 0)
      require((init.range % platformGrainBytes) == 0)
      (init.base_addr + (init.range / 2 - 1)) >> PMPOffBits
    }
  }

  private def dumpPMACSV(writer: PrintWriter): Unit = {
    pmacfgs.zipWithIndex.foreach { case (mod, idx) =>
      val csrIndex = idx / 8
      val fieldOffset = (idx % 8) * 8
      val csrName = s"Pmacfg${csrIndex * 2}"
      val csrAddr = CSRConst.PmacfgBase + csrIndex * 2
      val bundle = mod.bundle.asInstanceOf[CSRBundle]
      bundle.elements.foreach { case (fieldName, field: CSREnumType) =>
        writeFieldCSV(
          writer,
          "PMA",
          csrName,
          csrAddr,
          mod.modName,
          s"${mod.modName}.$fieldName",
          field.rwType.toString,
          field.msb + fieldOffset,
          field.lsb + fieldOffset,
          fieldResetValueString(bundle, field),
          field.isRef,
          field.isHardWired,
          fieldDescriptionString(fieldName, field),
        )
      }
    }

    pmaaddr.zipWithIndex.foreach { case (mod, idx) =>
      val reset = s"0x${pmaGenAddr(pmaInitEntries(idx)).toString(16)}"
      writeFieldCSV(
        writer,
        "PMA",
        mod.modName,
        mod.addr,
        "PMAEntryHandleModule",
        "ALL",
        "RW",
        63,
        0,
        reset,
        false,
        false,
        "Protected-region address encoding.",
      )
    }
  }

  private def dumpPMPCSV(writer: PrintWriter): Unit = {
    pmpcfgs.zipWithIndex.foreach { case (mod, idx) =>
      val csrIndex = idx / 8
      val fieldOffset = (idx % 8) * 8
      val csrName = s"Pmpcfg${csrIndex * 2}"
      val csrAddr = CSRs.pmpcfg0 + csrIndex * 2
      val bundle = mod.bundle.asInstanceOf[CSRBundle]
      bundle.elements.foreach { case (fieldName, field: CSREnumType) =>
        writeFieldCSV(
          writer,
          "PMP",
          csrName,
          csrAddr,
          mod.modName,
          s"${mod.modName}.$fieldName",
          field.rwType.toString,
          field.msb + fieldOffset,
          field.lsb + fieldOffset,
          fieldResetValueString(bundle, field),
          field.isRef,
          field.isHardWired,
          fieldDescriptionString(fieldName, field),
        )
      }
    }

    pmpcfg.zipWithIndex.foreach { case (mod, idx) =>
      if (idx >= (p(PMParameKey).NumPMPReal / 8)) {
        dumpModuleCSV(writer, "PMP", mod)
      }
    }

    pmpaddr.foreach(mod => dumpModuleCSV(writer, "PMP", mod))
  }

  protected def dumpCSRDoc(): Unit = {
    val output = csrCsvPath
    Files.createDirectories(output.getParent)
    val writer = new PrintWriter(output.toFile)
    try {
      writeCSVRow(writer, Seq(
        "group",
        "csr_name",
        "csr_addr",
        "impl_mod",
        "field_name",
        "rw_type",
        "msb",
        "lsb",
        "width",
        "reset",
        "is_ref",
        "is_hardwired",
        "description",
      ))

      machineLevelCSRMods.foreach(mod => dumpModuleCSV(writer, "M", mod))
      supervisorLevelCSRMods.foreach(mod => dumpModuleCSV(writer, "S", mod))
      hypervisorCSRMods.foreach(mod => dumpModuleCSV(writer, "H", mod))
      virtualSupervisorCSRMods.foreach(mod => dumpModuleCSV(writer, "VS", mod))
      unprivilegedCSRMods.foreach(mod => dumpModuleCSV(writer, "U", mod))
      debugCSRMods.foreach(mod => dumpModuleCSV(writer, "D", mod))
      dumpTriggerCSVs(writer)
      aiaCSRMods.foreach(mod => dumpModuleCSV(writer, "AIA", mod))
      customCSRMods.foreach(mod => dumpModuleCSV(writer, "CUSTOM", mod))
      indCSRMods.foreach(mod => dumpModuleCSV(writer, "IND", mod))
      dumpPMPCSV(writer)
      dumpPMACSV(writer)

      dumpBundleCSV(writer, "S", "sstatus", CSRs.sstatus, new SstatusBundle, "Mstatus(alias)")
      val fcsrAliasBundle = new FcsrAliasBundle
      dumpSelectedBundleCSV(writer, "U", "fflags", CSRs.fflags, fcsrAliasBundle, "Fcsr(alias)", Seq("NX", "UF", "OF", "DZ", "NV"))
      dumpSelectedBundleCSV(writer, "U", "frm", CSRs.frm, fcsrAliasBundle, "Fcsr(alias)", Seq("FRM"))
      dumpBundleCSV(writer, "U", "vxsat", CSRs.vxsat, new VxsatAliasBundle, "Vcsr(alias)")
      dumpBundleCSV(writer, "U", "vxrm", CSRs.vxrm, new VxrmAliasBundle, "Vcsr(alias)")
    } finally {
      writer.close()
    }
  }
}
