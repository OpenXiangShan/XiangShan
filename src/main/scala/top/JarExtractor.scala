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

import java.io._
import java.util.jar._

trait JarExtractor {
  /**
   * Extract resources in JAR to target directory
   * @param jarPath        File path of JAR
   * @param resourcePrefix Path prefix of resources to extract
   * @param outputDir      Target directory
   */
  def extractResources(jarPath: String, resourcePrefix: String, outputDir: String): Unit = {
    val jarFile = new JarFile(jarPath)
    val outputDirectory = new File(outputDir)

    if (!outputDirectory.exists()) {
      outputDirectory.mkdirs()
    }

    jarFile.entries().asIterator().forEachRemaining { entry =>
      if (entry.getName.startsWith(resourcePrefix)) {
        val outputFile = new File(outputDirectory, entry.getName.substring(resourcePrefix.length))
        if (entry.isDirectory) {
          outputFile.mkdirs()
        } else {
          outputFile.getParentFile.mkdirs()
          val inputStream = jarFile.getInputStream(entry)
          val outputStream = new FileOutputStream(outputFile)
          try {
            val buffer = new Array[Byte](4096)
            var bytesRead = 0
            while ({
              bytesRead = inputStream.read(buffer)
              bytesRead != -1
            }) {
              outputStream.write(buffer, 0, bytesRead)
            }
          } finally {
            inputStream.close()
            outputStream.close()
          }
        }
      }
    }
    jarFile.close()
    println(s"Extracted resources with prefix '$resourcePrefix' to $outputDir")
  }
}
