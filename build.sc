/***************************************************************************************
* Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
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

val defaultScalaVersion = "2.13.14"

val defaultVersions = Map(
  "chisel" -> ivy"edu.berkeley.cs::chisel3:3.6.1",
  "chisel-plugin" -> ivy"edu.berkeley.cs:::chisel3-plugin:3.6.1",
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

  object cde extends CDE

  trait CDE extends millbuild.`rocket-chip`.cde.common.CDEModule with ScalaModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = os.pwd / "rocket-chip" / "cde" / "cde"
  }
}

object hardfloat extends SbtModule with HasChisel {

  override def millSourcePath = os.pwd / "berkeley-hardfloat"

}

object difftest extends SbtModule with HasChisel {

  override def millSourcePath = os.pwd / "difftest"

}

object sifiveblockinclusivecache extends ScalaModule with HasChisel {

  override def millSourcePath = os.pwd / "block-inclusivecache-sifive" / "design" / "craft" / "inclusivecache"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip,
  )
}

// extends this trait to use XiangShan in other projects
trait XiangShanModule extends ScalaModule {

  def rocketModule: ScalaModule

  def inclusiveCacheModule: ScalaModule

  def difftestModule: ScalaModule

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketModule,
    sifiveblockinclusivecache,
    difftestModule,
  )

}

object XiangShan extends XiangShanModule with SbtModule with HasChisel {

  override def millSourcePath = millOuterCtx.millSourcePath

  def rocketModule = rocketchip

  def inclusiveCacheModule = sifiveblockinclusivecache

  def difftestModule = difftest

  override def forkArgs = Seq("-Xmx20G", "-Xss256m")

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def forkArgs = XiangShan.forkArgs

    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions("chiseltest"),
    )
  }

}
