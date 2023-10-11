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

import os.Path
import mill._
import scalalib._
import publish._
import coursier.maven.MavenRepository
import $file.`rocket-chip`.common
import $file.`rocket-chip`.`api-config-chipsalliance`.`build-rules`.mill.build
import $file.`rocket-chip`.hardfloat.build

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

  override def scalaVersion = ivys.sv

  override def compileIvyDeps = Agg(ivys.macroParadise)

  def hardfloatModule = hardfloat(crossValue)

  override def scalacOptions = Seq("-Xsource:2.11")

  def mainargsIvy = ivy"com.lihaoyi::mainargs:0.5.4"

  def json4sJacksonIvy = ivy"org.json4s::json4s-jackson:4.0.6"

  def publishVersion = "0.0.1"

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

  val rcPath = os.pwd / "rocket-chip"

  override def scalaVersion = ivys.sv

  override def scalacOptions = Seq("-Xsource:2.11")

  override def millSourcePath = rcPath

  object configRocket extends `rocket-chip`.`api-config-chipsalliance`.`build-rules`.mill.build.config with PublishModule {
    override def millSourcePath = rcPath / "api-config-chipsalliance" / "design" / "craft"

    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }

    override def pomSettings = T {
      rocketchip.pomSettings()
    }

    override def publishVersion = T {
      rocketchip.publishVersion()
    }
  }

  object hardfloatRocket extends `rocket-chip`.hardfloat.build.hardfloat {
    override def millSourcePath = rcPath / "hardfloat"

    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }

    def chisel3IvyDeps = if(chisel3Module.isEmpty) Agg(
      common.getVersion("chisel3")
    ) else Agg.empty[Dep]
    
    def chisel3PluginIvyDeps = Agg(common.getVersion("chisel3-plugin", cross=true))
  }

  def hardfloatModule = hardfloatRocket

  def configModule = configRocket

}

object huancun extends XSModule with SbtModule {

  override def millSourcePath = os.pwd / "huancun"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip(crossValue)
  )
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
trait CommonXiangShan extends XSModule with SbtModule { m =>

  // module deps
  def rocketModule: PublishModule
  def difftestModule: PublishModule
  def huancunModule: PublishModule
  def fudianModule: PublishModule

  override def millSourcePath = os.pwd

  override def forkArgs = Seq("-Xmx64G", "-Xss256m")

  override def ivyDeps = super.ivyDeps() ++ Seq(ivys.chiseltest)

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
    difftestModule,
    huancunModule,
    fudianModule
  )

  object test extends Tests with TestModule.ScalaTest {

object xiangshan extends Cross[XiangShan]("chisel", "chisel3")
trait XiangShan extends XiangShanModule with HasChisel {

  override def millSourcePath = os.pwd

  def rocketModule = rocketchip(crossValue)

  def difftestModule = difftest(crossValue)

  def huancunModule = huancun(crossValue)

  def coupledL2Module = coupledL2(crossValue)

  def fudianModule = fudian(crossValue)

  def utilityModule = utility(crossValue)

  override def forkArgs = Seq("-Xmx20G", "-Xss256m")

  override def sources = T.sources {
    super.sources() ++ Seq(PathRef(millSourcePath / s"src-${crossValue}" / "main" / "scala"))
  }

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def forkArgs = Seq("-Xmx20G", "-Xss256m")

    override def sources = T.sources {
      super.sources() ++ Seq(PathRef(millSourcePath / s"src-${crossValue}" / "test" / "scala"))
    }

    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions(crossValue)("chiseltest")
    )
  }
}

object XiangShan extends CommonXiangShan {
  override def rocketModule = rocketchip
  override def difftestModule = difftest
  override def huancunModule = huancun
  override def fudianModule = fudian
}
