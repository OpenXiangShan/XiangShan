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

package utils

/*
    https://github.com/Lingrui98/scalaTage/blob/vme/src/main/scala/getVerilogModules.scala
 */

import scala.io.Source
import java.io._
import scala.language.postfixOps
import sys.process._
import sys._

class VerilogModuleExtractor {
  //                            name
  val modulePattern = "module ([\\w]+)\\(".r.unanchored
  //                       type      name
  val subMoudlePattern = "([\\w]+) ([\\w]+) \\((?: //.*)*\\Z".r.unanchored
  val endMoudleIOPattern = "\\);".r.unanchored
  val endMoudlePattern = "endmodule".r.unanchored

  //                           (submoudle type, submoudle name)
  type SubMoudleRecord = Tuple2[String, String]

  //                        (content,          submodules)
  type ModuleRecord = Tuple2[List[String], List[SubMoudleRecord]]
  //                   name
  type ModuleMap = Map[String, ModuleRecord]

  def getLines(s: scala.io.BufferedSource): Iterator[String] = s.getLines()

  def makeRecord(s: Iterator[String]): ModuleMap = {
    val m: ModuleMap = Map()
    // called before we see the first line of a module
    def processModule(firstLine: String, it: Iterator[String]): ModuleRecord = {
      val content: List[String] = List(firstLine)
      val submodules: List[SubMoudleRecord] = List()
      def iter(cont: List[String], subm: List[SubMoudleRecord]): ModuleRecord =
        it.next() match {
          case l: String => l match {
            case endMoudlePattern() => (l :: cont, subm)
            case subMoudlePattern(ty, name) =>
              // println(s"submoudle $ty $name")
              iter(l :: cont, (ty, name) :: subm)
            case _ => iter(l :: cont, subm)
          }
          case _ => println("Should not reach here"); (cont, subm)
        }
      val temp = iter(content, submodules)
      (temp._1.reverse, temp._2)
    }
    def traverse(m: ModuleMap, it: Iterator[String]): ModuleMap =
      if (it.hasNext) {
        it.next() match {
          case l: String =>
            // println(f"traversing $l")
            l match {
              case modulePattern(name) =>
                // println(f"get Module of name $name")
                traverse(m ++ Map(name -> processModule(l, it)), it)
              case _ =>
                println(f"line $l is not a module definition")
                traverse(m, it)
            }
          case _ => traverse(m, it)
        }
      }
      else m

    traverse(m, s)
  }

  def makeRecordFromFile(file: String): ModuleMap = {
    val bufSrc = Source.fromFile(file)
    makeRecord(bufSrc.getLines())
  }

  def writeModuleToFile(name: String, record: ModuleRecord, dir: String) = {
    val path = dir+name+".v"
    val writer = new PrintWriter(new File(path))
    println(f"Writing module $name%20s to $path")
    record._1.foreach(r => {
      writer.write(f"$r\n")
    })
    writer.close()
  }

  // get moudle definition of specified name
  def getModule(name: String, m: ModuleMap): ModuleRecord = {
    m(name)
  }

  def showModuleRecord(r: ModuleRecord) = {
    val (content, submodules) = r
    submodules.foreach {
      case (t, n) => println(f"submoudle type: $t, submodule name: $n")
    }
    println("\nprinting module contents...")
    content.foreach(println(_))
  }

  // We first get records of all the modules and its submodule record
  // Then we choose a module as the root node to traverse its submodule
  def processFromModule(name: String, map: ModuleMap, outPath: String, doneSet: Set[String] = Set(), top: Tuple2[String, Boolean]): Unit = {
    def printSRAMs(sub: List[SubMoudleRecord]) = {
      sub map {
        case (ty, subn) if (ty contains "SRAM") => println(s"top module $name, sub module type $ty, name $subn")
        case _ =>
      }
    }
    val (topName, isTop) = top
    if (!map.contains(name)) {
      println(s"${if (isTop) "chosen top" else s"submodule of ${topName},"} module $name does not exist!")
      return
    }
    if (isTop) println(s"\nProcessing top module $name")
    val r = map(name)
    new File(outPath).mkdirs() // ensure the path exists
    writeModuleToFile(name, r, outPath)
    val submodules = r._2
    // printSRAMs(submodules)
    // DFS
    val subTypesSet = submodules map (m => m._1) toSet
    val nowMap = map - name
    val nowSet = doneSet ++ subTypesSet
    subTypesSet.foreach { s  => if (!doneSet.contains(s)) processFromModule(s, nowMap, outPath, nowSet, (if (isTop) name else topName, false)) }
  }

  def getDate: String = {
    val d = java.time.LocalDate.now
    d.toString.toCharArray.filterNot(_ == '-').mkString
  }

  def makePath(topModule: String, outDir: String , user: String = "glr"): String = {
    (if (outDir.last == '/')
      outDir
    else
      outDir+"/") + getDate + "-" + user + "-" + topModule + "/"
  }



  def extract(src: String, topModule: String, outDir: String, user: String, mapp: Option[ModuleMap]): Unit = {
    val useMap = mapp.getOrElse(makeRecordFromFile(src))
    val path = makePath(topModule, outDir, user)
    processFromModule(topModule, useMap, path, top=(topModule, true))
  }

  def extract(src: String, topModules: List[String], outDir: String, user: String): Unit = {
    // avoid repeat
    val mapp = makeRecordFromFile(src)
    topModules.foreach(n => extract(src, n, outDir, user, Some(mapp)))
  }
}

trait VMEArgParser {
  type OptionMap = Map[String, Option[Any]]

  val usage = """
        Usage: sbt "run [OPTION...]"
            -s, --source   the verilog file generated by chisel, all in one file
                           default: $NOOP_HOME/build/XSSimTop.v
            -h, --help     print this help info
            -o, --output   the place you want to store your extracted verilog
                           default: $NOOP_HOME/build/extracted
            -u, --usr      your name, will be used to name the output folder
                           default: current user
            -m, --modules  the top modules you would like to extract verilog from
                           should always be the last argument
                           default: IFU
      """

  def parse(args: List[String]) = {
    def nextOption(map: OptionMap, l: List[String]): OptionMap = {
      def isSwitch(s : String)= (s(0) == '-')
      l match {
        case Nil => map
        case ("--help" | "-h") :: tail => {
          println(usage)
          sys.exit()
          map
        }
        case ("--source" | "-s") :: file :: tail =>
          nextOption(map ++ Map("source" -> Some(file)), tail)
        case ("--output" | "-o") :: path :: tail =>
          nextOption(map ++ Map("output" -> Some(path)), tail)
        case ("--usr" | "-u") :: name :: tail =>
          nextOption(map ++ Map("usr" -> Some(name)), tail)
        // this should always be the last argument, since it is length variable
        case ("--modules" | "-m") :: m :: tail =>
          map ++ Map("modules" -> Some(m :: tail))
        case s :: tail => {
          if (isSwitch(s)) println(s"unexpected argument $s")
          nextOption(map, tail)
        }
      }
    }
    nextOption(Map("source" -> None, "output" -> None, "usr" -> None, "modules" -> None), args)
  }

  def wrapParams(args: Array[String]): (String, List[String], String, String) = {
    val argL = args.toList
    val paramMap = parse(argL)
    (paramMap("source").map(_.asInstanceOf[String]).getOrElse(env("NOOP_HOME")+"/build/XSSimTop.v"),
      paramMap("modules").map(_.asInstanceOf[List[String]]).getOrElse(List("IFU")),
      paramMap("output").map(_.asInstanceOf[String]).getOrElse(env("NOOP_HOME")+"/build/extracted/"),
      paramMap("usr").map(_.asInstanceOf[String]).getOrElse("whoami".!!.init))
  }
}

object ExtractVerilogModules extends VMEArgParser {
  def main(args: Array[String]): Unit = {
    val vme = new VerilogModuleExtractor()
    val (sourceFile, topModules, outTopDir, usr) = wrapParams(args)
    vme.extract(sourceFile, topModules, outTopDir, usr)
  }
}
