package utils

import java.io.{File, FileWriter}

object FileRegisters {
  var files: Seq[(String, () => String)] = Nil

  def add(filename: String, contents: => String): Unit = {
    files = (filename, () => contents) +: files
  }

  def contains(filename: String): Boolean = {
    files.foldLeft(false)((t, s) => {s._1 == filename | t})
  }

  def write(fileDir: String = "./build", filePrefix: String = ""): Unit = {
    files.foreach { case (filename, contents) =>
      writeOutputFile(fileDir, filePrefix + filename, contents())
    }
  }

  def writeOutputFile(targetDir: String, fname: String, contents: String): File = {
    val f = new File(targetDir, fname)
    val fw = new FileWriter(f)
    fw.write(contents)
    fw.close
    f
  }
}

