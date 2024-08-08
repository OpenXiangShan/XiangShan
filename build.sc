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

import mill._
import scalalib._
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.build
import $file.huancun.common
import $file.coupledL2.common
import $file.openLLC.common

val defaultScalaVersion = "2.13.14"

def defaultVersions = Map(
  "chisel"        -> ivy"org.chipsalliance::chisel:6.5.0",
  "chisel-plugin" -> ivy"org.chipsalliance:::chisel-plugin:6.5.0",
  "chiseltest"    -> ivy"edu.berkeley.cs::chiseltest:6.0.0"
)

trait HasChisel extends SbtModule {
  def chiselModule: Option[ScalaModule] = None

  def chiselPluginJar: T[Option[PathRef]] = None

  def chiselIvy: Option[Dep] = Some(defaultVersions("chisel"))

  def chiselPluginIvy: Option[Dep] = Some(defaultVersions("chisel-plugin"))

  override def scalaVersion = defaultScalaVersion

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}

object rocketchip
  extends millbuild.`rocket-chip`.common.RocketChipModule
    with HasChisel {
  def scalaVersion: T[String] = T(defaultScalaVersion)

  override def millSourcePath = os.pwd / "rocket-chip"

  def macrosModule = macros

  def hardfloatModule = hardfloat

  def cdeModule = cde

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.7.0"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.7"

  object macros extends Macros

  trait Macros
    extends millbuild.`rocket-chip`.common.MacrosModule
      with SbtModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${defaultScalaVersion}"
  }

  object hardfloat
    extends millbuild.`rocket-chip`.hardfloat.common.HardfloatModule with HasChisel {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = os.pwd / "rocket-chip" / "hardfloat" / "hardfloat"

  }

  object cde
    extends millbuild.`rocket-chip`.cde.common.CDEModule with ScalaModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = os.pwd / "rocket-chip" / "cde" / "cde"
  }
}

object utility extends HasChisel {

  override def millSourcePath = os.pwd / "utility"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip
  )

}

object yunsuan extends HasChisel {

  override def millSourcePath = os.pwd / "yunsuan"

}

object huancun extends millbuild.huancun.common.HuanCunModule with HasChisel {

  override def millSourcePath = os.pwd / "huancun"

  def rocketModule: ScalaModule = rocketchip

  def utilityModule: ScalaModule = utility

}

object coupledL2 extends millbuild.coupledL2.common.CoupledL2Module with HasChisel {

  override def millSourcePath = os.pwd / "coupledL2"

  def rocketModule: ScalaModule = rocketchip

  def utilityModule: ScalaModule = utility

  def huancunModule: ScalaModule = huancun

}

object openLLC extends millbuild.openLLC.common.OpenLLCModule with HasChisel {

  override def millSourcePath = os.pwd / "openLLC"

  def coupledL2Module: ScalaModule = coupledL2

  def rocketModule: ScalaModule = rocketchip

  def utilityModule: ScalaModule = utility
}

object difftest extends HasChisel {

  override def millSourcePath = os.pwd / "difftest"

}

object fudian extends HasChisel {

  override def millSourcePath = os.pwd / "fudian"

}

object macros extends ScalaModule {

  override def millSourcePath = os.pwd / "macros"

  override def scalaVersion: T[String] = T(defaultScalaVersion)

  override def ivyDeps = super.ivyDeps() ++ Agg(ivy"org.scala-lang:scala-reflect:${defaultScalaVersion}")

  def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${defaultScalaVersion}"
}

// extends this trait to use XiangShan in other projects
trait XiangShanModule extends ScalaModule {

  def rocketModule: ScalaModule

  def difftestModule: ScalaModule

  def huancunModule: ScalaModule

  def coupledL2Module: ScalaModule

  def openLLCModule: ScalaModule

  def fudianModule: ScalaModule

  def utilityModule: ScalaModule

  def yunsuanModule: ScalaModule

  def macrosModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
    difftestModule,
    huancunModule,
    coupledL2Module,
    openLLCModule,
    yunsuanModule,
    fudianModule,
    utilityModule,
    macrosModule,
  )

  val resourcesPATH = os.pwd.toString() + "/src/main/resources"
  val envPATH = sys.env("PATH") + ":" + resourcesPATH

  override def forkEnv = Map("PATH" -> envPATH)
}

object xiangshan extends XiangShanModule with HasChisel {

  override def millSourcePath = os.pwd

  def rocketModule = rocketchip

  def difftestModule = difftest

  def huancunModule = huancun

  def coupledL2Module = coupledL2

  def openLLCModule = openLLC

  def fudianModule = fudian

  def utilityModule = utility

  def yunsuanModule = yunsuan

  def macrosModule = macros

  override def forkArgs = Seq("-Xmx40G", "-Xss256m")

  override def ivyDeps = super.ivyDeps() ++ Agg(
    defaultVersions("chiseltest"),
  )

  override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def forkArgs = Seq("-Xmx40G", "-Xss256m")

    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions("chiseltest")
    )

    override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")

    val resourcesPATH = os.pwd.toString() + "/src/main/resources"
    val envPATH = sys.env("PATH") + ":" + resourcesPATH

    override def forkEnv = Map("PATH" -> envPATH)
  }
}
