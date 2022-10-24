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

object ivys {
  val sv = "2.12.13"
  val chisel3 = ivy"edu.berkeley.cs::chisel3:3.5.3"
  val chisel3Plugin = ivy"edu.berkeley.cs:::chisel3-plugin:3.5.3"
  val chiseltest = ivy"edu.berkeley.cs::chiseltest:0.5.1"
  val scalatest = ivy"org.scalatest::scalatest:3.2.2"
  val macroParadise = ivy"org.scalamacros:::paradise:2.1.1"
}

trait XSModule extends ScalaModule with PublishModule {

  // override this to use chisel from source
  def chiselOpt: Option[PublishModule] = None

  override def scalaVersion = ivys.sv

  override def compileIvyDeps = Agg(ivys.macroParadise)

  override def scalacPluginIvyDeps = Agg(ivys.macroParadise, ivys.chisel3Plugin)

  override def scalacOptions = Seq("-Xsource:2.11")

  override def ivyDeps = if(chiselOpt.isEmpty) Agg(ivys.chisel3) else Agg.empty[Dep]

  override def moduleDeps = Seq() ++ chiselOpt

  def publishVersion = "0.0.1"

  // TODO: fix this
  def pomSettings = PomSettings(
    description = "XiangShan",
    organization = "",
    url = "https://github.com/OpenXiangShan/XiangShan",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("OpenXiangShan", "XiangShan"),
    developers = Seq.empty
  )
}

object rocketchip extends `rocket-chip`.common.CommonRocketChip {

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
    rocketchip
  )
}

object difftest extends XSModule with SbtModule {
  override def millSourcePath = os.pwd / "difftest"
}

object fudian extends XSModule with SbtModule

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

    override def forkArgs = m.forkArgs

    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivys.scalatest
    )

  }

}

object XiangShan extends CommonXiangShan {
  override def rocketModule = rocketchip
  override def difftestModule = difftest
  override def huancunModule = huancun
  override def fudianModule = fudian
}
