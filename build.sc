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

val defaultVersions = Map(
  "chisel" -> ivy"edu.berkeley.cs::chisel3:3.6.0",
  "chisel-plugin" -> ivy"edu.berkeley.cs:::chisel3-plugin:3.6.0",
  "chiseltest" -> ivy"edu.berkeley.cs::chiseltest:0.6.2",
)

trait HasChisel extends ScalaModule {
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

object rocketchip extends RocketChip

trait RocketChip
  extends millbuild.`rocket-chip`.common.RocketChipModule
    with SbtModule with HasChisel {
  def scalaVersion: T[String] = T(defaultScalaVersion)

  override def millSourcePath = os.pwd / "rocket-chip"

  def macrosModule = macros

  def hardfloatModule = hardfloat

  def cdeModule = cde

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.5.0"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.5"

  object macros extends Macros

  trait Macros
    extends millbuild.`rocket-chip`.common.MacrosModule
      with SbtModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    def scalaReflectIvy = ivy"org.scala-lang:scala-reflect:${defaultScalaVersion}"
  }

  object hardfloat extends Hardfloat

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

object utility extends SbtModule with HasChisel {

  override def millSourcePath = os.pwd / "utility"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip
  )

}

object huancun extends millbuild.huancun.common.HuanCunModule with SbtModule with HasChisel {

  override def millSourcePath = os.pwd / "huancun"

  def rocketModule: ScalaModule = rocketchip

  def utilityModule: ScalaModule = utility

}

object coupledL2 extends millbuild.coupledL2.common.CoupledL2Module with SbtModule with HasChisel {

  override def millSourcePath = os.pwd / "coupledL2"

  def rocketModule: ScalaModule = rocketchip

  def utilityModule: ScalaModule = utility

  def huancunModule: ScalaModule = huancun

}

object difftest extends SbtModule with HasChisel {

  override def millSourcePath = os.pwd / "difftest"

}

object fudian extends SbtModule with HasChisel {

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

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
    difftestModule,
    huancunModule,
    coupledL2Module,
    fudianModule,
    utilityModule,
  )

}

object XiangShan extends XiangShanModule with SbtModule with HasChisel {

  override def millSourcePath = millOuterCtx.millSourcePath

  def rocketModule = rocketchip

  def difftestModule = difftest

  def huancunModule = huancun

  def coupledL2Module = coupledL2

  def fudianModule = fudian

  def utilityModule = utility

  override def forkArgs = Seq("-Xmx20G", "-Xss256m")

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def forkArgs = XiangShan.forkArgs

    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions("chiseltest"),
    )
  }

}
