val chiselVersion = "3.4.3"
scalaVersion := "2.12.10"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation","-unchecked","-Xsource:2.11"),
  libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value),
  libraryDependencies ++= Seq("org.json4s" %% "json4s-jackson" % "3.6.1"),
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.0" % "test"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.mavenLocal
  )
)

lazy val chiselSettings = Seq(
  libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel3" % chiselVersion),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)
)

lazy val `api-config-chipsalliance` = (project in file("api-config-chipsalliance/build-rules/sbt"))
  .settings(commonSettings)

lazy val hardfloat = (project in file("berkeley-hardfloat"))
  .settings(commonSettings, chiselSettings)

lazy val rocketMacros = (project in file("rocket-chip/macros"))
  .settings(commonSettings)

lazy val `rocket-chip` = (Project("rocket-chip", file("rocket-chip/src")))
  .settings(commonSettings, chiselSettings)
  .settings(
    scalaSource in Compile := baseDirectory.value / "main" / "scala",
    resourceDirectory in Compile := baseDirectory.value / "main" / "resources"
  )
  .dependsOn(rocketMacros)
  .dependsOn(`api-config-chipsalliance`)
  .dependsOn(hardfloat)

lazy val `block-inclusive-cache` = (project in file("block-inclusivecache-sifive"))
  .settings(commonSettings, chiselSettings)
  .settings(
    scalaSource in Compile := baseDirectory.value / "design" / "craft" / "inclusivecache",
  )
  .dependsOn(`rocket-chip`)

lazy val chiseltest = (project in file("chiseltest"))
  .settings(commonSettings, chiselSettings)

lazy val xiangshan = (Project("XiangShan", base = file(".")))
  .settings(commonSettings, chiselSettings)
  .dependsOn(`rocket-chip`, `block-inclusive-cache`, chiseltest)