/***************************************************************************************
 * Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
 * Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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
package tracertl

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage.ChiselStage
import xiangshan.frontend.tracertl.{TraceAXISPackage, TraceAXISUnpackage}
import xiangshan.frontend.tracertl.TraceRTLParamKey
import xiangshan.DebugOptionsKey
import top.ArgParser

// used to generated TraceAXISUnpackage.sv and *Package.sv without SimTop
object TraceRTLAXISModGen extends App {
  // FIXME: updatge args in Makefile. manual add parse
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)
  val trtlOpts = config(TraceRTLParamKey)

  val targetDir = "./build/tracertl"

  (new ChiselStage).execute(
    Array(
      "--target-dir", targetDir,
      "--target", "systemverilog",
    ),
    Seq(ChiselGeneratorAnnotation(() => new TraceAXISPackage(
      trtlOpts.TraceFpgaCollectWidth,
      trtlOpts.TraceFpgaAxisWidth
    )(config.alter((site, here, up) => {
      case DebugOptionsKey => up(DebugOptionsKey).copy(FPGAPlatform = true)
    }))))
  )

  println(s"Successfully generated TraceAXISPackage.sv in $targetDir")

  (new ChiselStage).execute(
    Array(
      "--target-dir", targetDir,
      "--target", "systemverilog",
    ),
    Seq(ChiselGeneratorAnnotation(() => new TraceAXISUnpackage(
      trtlOpts.TraceFpgaUnpackInstNum,
      trtlOpts.TraceFpgaAxisWidth,
      trtlOpts.TraceFpgaRecvWidth
    )(config.alter((site, here, up) => {
      case DebugOptionsKey => up(DebugOptionsKey).copy(FPGAPlatform = true)
    }))))
  )

  println(s"Successfully generated TraceAXISUpackage.sv in $targetDir")
}