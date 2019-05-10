package top

import noop._

object TestMain extends App {
  var imgPath = ""
  var newArgs: Array[String] = Array()
  args.sliding(2, 2).toList.collect {
    case Array("--image", argImg: String) => imgPath = argImg
    case Array(a: String, b: String) => newArgs = newArgs :+ a :+ b
  }

  chisel3.Driver.execute(newArgs, () => new NOOPSimTop(memInitFile = imgPath))
}
