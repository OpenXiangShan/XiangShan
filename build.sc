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
import scalafmt._
import os.Path
import publish._
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.build

val validChiselVersion = List("chisel3", "chisel")
val defaultScalaVersion = "2.13.10"

val defaultVersions = Map(
  "chisel" -> ivy"edu.berkeley.cs::chisel3:3.6.0",
  "chisel-plugin" -> ivy"edu.berkeley.cs:::chisel3-plugin:3.6.0",
  "chiseltest" -> ivy"edu.berkeley.cs::chiseltest:0.6.2",
)

trait CommonModule extends ScalaModule {
  override def scalaVersion = defaultScalaVersion

  override def scalacPluginIvyDeps = Agg(defaultVersions("chisel-plugin"))

  override def scalacOptions = super.scalacOptions() ++ Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")
}

object rocketchip extends RocketChip

trait RocketChip
  extends millbuild.`rocket-chip`.common.RocketChipModule
    with SbtModule {
  def scalaVersion: T[String] = T(defaultScalaVersion)

  override def millSourcePath = os.pwd / "rocket-chip"

  def chiselModule = None

  def chiselPluginJar = None

  def chiselIvy = Some(defaultVersions("chisel"))

  def chiselPluginIvy = Some(defaultVersions("chisel-plugin"))

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
    extends millbuild.`rocket-chip`.hardfloat.common.HardfloatModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = os.pwd / "rocket-chip" / "hardfloat" / "hardfloat"

    def chiselModule = None

    def chiselPluginJar = None

    def chiselIvy = Some(defaultVersions("chisel"))

    def chiselPluginIvy = Some(defaultVersions("chisel-plugin"))
  }

  object cde extends CDE

  trait CDE extends millbuild.`rocket-chip`.cde.common.CDEModule with ScalaModule {

    def scalaVersion: T[String] = T(defaultScalaVersion)

    override def millSourcePath = os.pwd / "rocket-chip" / "cde" / "cde"
  }
}

object huancun extends SbtModule with ScalafmtModule with CommonModule {

  override def ivyDeps = Agg(defaultVersions("chisel"))

  override def millSourcePath = os.pwd / "huancun"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip, utility
  )
}

object coupledL2 extends SbtModule with ScalafmtModule with CommonModule {

  override def ivyDeps = Agg(defaultVersions("chisel"))

  override def millSourcePath = os.pwd / "coupledL2"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip,
    huancun,
    utility
  )
}

object difftest extends SbtModule with ScalafmtModule with CommonModule {

  override def ivyDeps = Agg(defaultVersions("chisel"))

  override def millSourcePath = os.pwd / "difftest"
}

object fudian extends SbtModule with ScalafmtModule with CommonModule {

  override def ivyDeps = Agg(defaultVersions("chisel"))

  override def millSourcePath = os.pwd / "fudian"
}

object utility extends SbtModule with ScalafmtModule with CommonModule {

  override def ivyDeps = Agg(defaultVersions("chisel"))

  override def millSourcePath = os.pwd / "utility"

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip
  )
}

object XiangShan extends SbtModule with ScalafmtModule with CommonModule {

  override def millSourcePath = millOuterCtx.millSourcePath

  override def forkArgs = Seq("-Xmx20G", "-Xss256m")

  override def ivyDeps = super.ivyDeps() ++ Agg(
    defaultVersions("chisel"),
  )

  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip,
    utility,
    huancun,
    difftest,
    coupledL2,
    fudian
  )

  object test extends SbtModuleTests with TestModule.ScalaTest {
    override def forkArgs = XiangShan.forkArgs

    override def ivyDeps = super.ivyDeps() ++ Agg(
      defaultVersions("chiseltest"),
    )
  }
}
