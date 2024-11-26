/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
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
package top

import org.chipsalliance.cde.config.Parameters
import xiangshan.DebugOptionsKey

object DifftestBuilder extends JarExtractor {
  def build(outputDir: String, config: Parameters): Unit = {
    // skip if not running in JAR envrionment
    val classPath = getClass.getProtectionDomain.getCodeSource.getLocation.getPath
    if (!classPath.endsWith(".jar"))
      return

    // extrace difftest source from JAR
    extractResources(classPath, "difftest-src/", s"${outputDir}/difftest")

    // build libdifftest.so
    println(s"Building difftest:")
    try {
      val customEnv = Map(
        "NOOP_HOME"       -> System.getenv("NOOP_HOME"),
        "WITH_CHISELDB"   -> (if (config(DebugOptionsKey).EnableChiselDB) "1" else "0"),
        "WITH_CONSTANTIN" -> (if (config(DebugOptionsKey).EnableConstantin) "1" else "0"),
        "NUM_CORES"       -> "1")
      val result = os.proc("make", "difftest-so").call(cwd = os.pwd / s"${outputDir}" / "difftest",
                                                       env = customEnv)
    } catch {
      case e: os.SubprocessException =>
        println(s"ERROR: difftest build failed, err=${e.result.exitCode}")
    }
  }
}
