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

import mill._
import scalalib._
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.build
import $file.huancun.common
import $file.coupledL2.common

val defaultScalaVersion = "2.13.10"

def defaultVersions(chiselVersion: String) = Map(
  "chisel" -> (chiselVersion match {
    case "chisel"  => ivy"org.chipsalliance::chisel:6.0.0-M3"
    case "chisel3" => ivy"edu.berkeley.cs::chisel3:3.6.0"
  }),
  "chisel-plugin" -> (chiselVersion match {
    case "chisel"  => ivy"org.chipsalliance:::chisel-plugin:6.0.0-M3"
    case "chisel3" => ivy"edu.berkeley.cs:::chisel3-plugin:3.6.0"
  }),
  "chiseltest" -> (chiselVersion match {
    case "chisel"  => ivy"edu.berkeley.cs::chiseltest:5.0.2"
    case "chisel3" => ivy"edu.berkeley.cs::chiseltest:0.6.2"
  })
)

trait HasChisel extends SbtModule with Cross.Module[String] {
  def chiselModule: Option[ScalaModule] = None

  def chiselPluginJar: T[Option[PathRef]] = None

  def chiselIvy: Option[Dep] = Some(defaultVersions(crossValue)("chisel"))

  def chiselPluginIvy: Option[Dep] = Some(defaultVersions(crossValue)("chisel-plugin"))

  override def scalaVersion = defaultScalaVersion

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}

object rocketchip extends Cross[RocketChip]("chisel", "chisel3")

trait RocketChip
  extends millbuild.`rocket-chip`.common.RocketChipModule
    with HasChisel {
  def scalaVersion: T[String] = T(defaultScalaVersion)

  override def millSourcePath = os.pwd / "rocket-chip"

  def macrosModule = macros

  def hardfloatModule = hardfloat(crossValue)

  def cdeModule = cde

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.5.4"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.6"

  object macros extends Macros

  trait Macros
    extends millbuild.`rocket-chip`.common.MacrosModule
      with SbtModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${defaultScalaVersion}"
  }

  object hardfloat extends Cross[Hardfloat](crossValue)

  trait Hardfloat
    extends millbuild.`rocket-chip`.hardfloat.common.HardfloatModule with HasChisel {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = os.pwd / "rocket-chip" / "hardfloat" / "hardfloat"

  }

  object cde extends CDE

  trait CDE extends millbuild.`rocket-chip`.cde.common.CDEModule with ScalaModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = os.pwd / "rocket-chip" / "cde" / "cde"
  }
}

object utility extends Cross[Utility]("chisel", "chisel3")
trait Utility extends HasChisel {

  override def millSourcePath = os.pwd / "utility"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip(crossValue)
  )

}

object yunsuan extends SbtModule with HasChisel {

  override def millSourcePath = os.pwd / "yunsuan"

}

object huancun extends Cross[HuanCun]("chisel", "chisel3")
trait HuanCun extends millbuild.huancun.common.HuanCunModule with HasChisel {

  override def millSourcePath = os.pwd / "huancun"

  def rocketModule: ScalaModule = rocketchip(crossValue)

  def utilityModule: ScalaModule = utility(crossValue)

}

object coupledL2 extends Cross[CoupledL2]("chisel", "chisel3")
trait CoupledL2 extends millbuild.coupledL2.common.CoupledL2Module with HasChisel {

  override def millSourcePath = os.pwd / "coupledL2"

  def rocketModule: ScalaModule = rocketchip(crossValue)

  def utilityModule: ScalaModule = utility(crossValue)

  def huancunModule: ScalaModule = huancun(crossValue)

}

object difftest extends Cross[Difftest]("chisel", "chisel3")
trait Difftest extends HasChisel {

  override def millSourcePath = os.pwd / "difftest"

}

object fudian extends Cross[FuDian]("chisel", "chisel3")
trait FuDian extends HasChisel {

  override def millSourcePath = os.pwd / "fudian"

}

// extends this trait to use XiangShan in other projects
trait XiangShanModule extends ScalaModule {

  def rocketModule: ScalaModule

  def difftestModule: ScalaModule

  def huancunModule: ScalaModule

  def coupledL2Module: ScalaModule

  def fudianModule: ScalaModule

  def utilityModule: ScalaModule

  def yunsuanModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
    difftestModule,
    huancunModule,
    coupledL2Module,
    yunsuanModule,
    fudianModule,
    utilityModule,
  )

  val resourcesPATH = os.pwd.toString() + "/src/main/resources"
  val envPATH = sys.env("PATH") + ":" + resourcesPATH

  override def forkEnv = Map("PATH" -> envPATH)
}

object xiangshan extends Cross[XiangShan]("chisel", "chisel3")
trait XiangShan extends XiangShanModule with HasChisel {

  override def millSourcePath = os.pwd

  def rocketModule = rocketchip(crossValue)

  def difftestModule = difftest(crossValue)

  def huancunModule = huancun(crossValue)

  def coupledL2Module = coupledL2(crossValue)

  def fudianModule = fudian(crossValue)

  def utilityModule = utility(crossValue)

  def yunsuanModule = yunsuan

  override def forkArgs = Seq("-Xmx20G", "-Xss256m")

  override def sources = T.sources {
    super.sources() ++ Seq(PathRef(millSourcePath / s"src-${crossValue}" / "main" / "scala"))
  }

  override def ivyDeps = super.ivyDeps() ++ Agg(
    defaultVersions("chiseltest"),
  )

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def forkArgs = Seq("-Xmx20G", "-Xss256m")

    override def sources = T.sources {
      super.sources() ++ Seq(PathRef(millSourcePath / s"src-${crossValue}" / "test" / "scala"))
    }

    override def forkEnv = XiangShan.forkEnv

    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions(crossValue)("chiseltest")
    )
  }
}
