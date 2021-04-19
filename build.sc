import os.Path
import mill._
import mill.modules.Util
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import $ivy.`com.lihaoyi::mill-contrib-bsp:$MILL_VERSION`
import mill.contrib.buildinfo.BuildInfo
import scalalib._
import coursier.maven.MavenRepository

object CustomZincWorkerModule extends ZincWorkerModule {
  def repositories() = super.repositories ++ Seq(
    MavenRepository("https://maven.aliyun.com/repository/public"),
    MavenRepository("https://maven.aliyun.com/repository/apache-snapshots")
  )
}

trait CommonModule extends ScalaModule {
  override def scalaVersion = "2.12.10"

  override def scalacOptions = Seq("-Xsource:2.11")

  override def zincWorker = CustomZincWorkerModule

  private val macroParadise = ivy"org.scalamacros:::paradise:2.1.0"

  override def compileIvyDeps = Agg(macroParadise)

  override def scalacPluginIvyDeps = Agg(macroParadise)
}

val chisel = Agg(
  ivy"edu.berkeley.cs::chisel3:3.4.3"
)

object `api-config-chipsalliance` extends CommonModule {
  override def millSourcePath = super.millSourcePath / "design" / "craft"
}

object hardfloat extends SbtModule with CommonModule {
  override def millSourcePath = os.pwd / "berkeley-hardfloat"
  override def ivyDeps = super.ivyDeps() ++ chisel
}

object `rocket-chip` extends SbtModule with CommonModule {

  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}",
    ivy"org.json4s::json4s-jackson:3.6.1"
  ) ++ chisel

  object macros extends SbtModule with CommonModule

  override def moduleDeps = super.moduleDeps ++ Seq(
    `api-config-chipsalliance`, macros, hardfloat
  )

}

object `block-inclusivecache-sifive` extends CommonModule {
  override def ivyDeps = super.ivyDeps() ++ chisel

  override def millSourcePath = super.millSourcePath / 'design / 'craft / 'inclusivecache

  override def moduleDeps = super.moduleDeps ++ Seq(`rocket-chip`)
}

object chiseltest extends CommonModule with SbtModule {
  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"edu.berkeley.cs::treadle:1.3.0",
    ivy"org.scalatest::scalatest:3.2.0",
    ivy"com.lihaoyi::utest:0.7.4"
  ) ++ chisel
  object test extends Tests {
    def ivyDeps = Agg(ivy"org.scalacheck::scalacheck:1.14.3")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}


object XiangShan extends CommonModule with SbtModule {
  override def millSourcePath = millOuterCtx.millSourcePath

  override def forkArgs = Seq("-Xmx10G")

  override def ivyDeps = super.ivyDeps() ++ chisel
  override def moduleDeps = super.moduleDeps ++ Seq(
    `rocket-chip`,
    `block-inclusivecache-sifive`,
    chiseltest
  )

  object test extends Tests {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.scalatest::scalatest:3.2.0"
    )

    def testFrameworks = Seq(
      "org.scalatest.tools.Framework"
    )

    def testOnly(args: String*) = T.command {
      super.runMain("org.scalatest.tools.Runner", args: _*)
    }
  }

}
